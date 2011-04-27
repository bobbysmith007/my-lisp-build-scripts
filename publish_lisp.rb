# -*- mode: ruby -*-

require 'rubygems'
require 'vlad'
require 'vlad/core'
require 'erb'
require 'hoe/rake'


###################################
### Setup our standard variable set

# SEARCH for this, because some commands must change this
set :rsync_flags, ['-azP', '--delete', 
                   '--exclude=.git', '--exclude=*.erb', '--exclude=.svn',
                   '--exclude=*~']

set :repository, nil
set :owner, "www-data:webadmin"
set :erb_files, ["etc/apache.conf.erb"]
set :logroot, Proc.new { "#{shared_path}/log" }
set :run_as, "www-data"

set :lisp_root,"/opt/lisp"
set :init_dot_d_template,"#{lisp_root}/ADWCodeBase/misc/init.d.erb"
#what extra symlinks should be added
set :extra_symlinks, []

#Folders that are shared among every release (e.g. persistent upload folders)
set :share_folders, []

#we use these mods for just about everything
set :a2mods, ["rewrite", "ssl", "ldap", "authnz_ldap", "proxy", 
              "headers", "proxy_http", "filter", "cache", "disk_cache", "deflate"]

###
##################################

namespace :lisp do
  desc "returns the current place we are publishing to and whether it is considered live.".cleanup
  task :check do
    puts "Application:#{application}"
    puts "IsLive:#{Rake::RemoteTask.fetch(:liveserver,"true")}"
    puts "deploy to:#{domain}:#{deploy_to}"
  end 

  def set_ownerships (*args)
    list = args.join ', '
    run "sudo chown -R #{owner} #{list}"
    run "sudo chmod -R ug+rw #{list}"
  end

  def safe_rsync_current (*files)
    @happy = true
    args = files + ["#{target_host}:#{current_path}"]
    rsync(*args)
    @happy = false
  end

  def safe_rsync_release (*files)
    @happy = true
    args = files + ["#{target_host}:#{release_path}"]
    rsync(*args)
    @happy = false
  end

  def run_rc(cmd)
    rc = run("#{cmd}; echo $?").split(' ')
    return rc[-1].to_i
  end

  desc "Make sure the directory for the new release is ready.".cleanup
  remote_task :ensure_release_path => ['vlad:setup_app'] do
    run "umask 02 && mkdir -p #{release_path}"
    set_ownerships(release_path)
  end

  desc "Remove all the build products in preparation for a fresh build.".cleanup
  task :clean do
    system("rm -rf obj bin")
    system("mkdir obj bin")
  end

  desc "Git Tags the publish so we know what version it is.".cleanup
  task :gittag do
    live = Rake::RemoteTask.fetch(:liveserver,"true")
    extra = Rake::RemoteTask.fetch(:gittag_extra,"")
    sh("git tag -am 'Autotag on publish #{release_name}, LIVE_SERVER:#{live}, application:#{application} #{extra}' publish-autotag-#{release_name}")
  end

  desc "Build the lisp image.".cleanup
  task :build => [:clean] do
    logroot_clean = ((logroot =~ /\/$/) and logroot or (logroot + "/"))
    ENV['LIVE_SERVER'] = Rake::RemoteTask.fetch(:liveserver,"true")
    ENV['SBCL_HOME'] = "/usr/local/lib/sbcl"
    result = system("sbcl --core #{lisp_root}/sbcl-site-ucw-publish.core --userinit #{lisp_root}/ADWCodeBase/misc/sbcl-publish-init.lisp --load make-image.lisp --end-toplevel-options --binary #{application} --port #{port} --log-root #{logroot_clean}")
    raise "Error building lisp image." unless result
  end

  def process_erb(inputname, outputname)
    if(File.exists?(inputname))
      template = ERB.new(File.read(inputname))
      File.open(outputname, 'w') do |f|
        f.write(template.result)
      end
    else
      raise "Can't find erb file to translate: #{inputname}."
    end
  end

  desc "Rewrite all the templated configuration files. Resolve the templates of
   everything listed by the erb_files variable.".cleanup
  task :rewrite_conf do
    process_erb(init_dot_d_template, "etc/init.d")
    erb_files.each do |tempfile|
      if(tempfile.kind_of? Array)
        process_erb(*tempfile)
      else
        outputname = tempfile.sub(/\.erb$/,'')
        process_erb(tempfile,outputname)
      end
    end
  end

  desc "Reupload www folder to the current publish (useful for updating images.".cleanup
  remote_task :reupload_www do
    #use the intersection of copy and web since some web folders are shared across publishes.
    uploads = copy_folders & web_folders
    safe_rsync_current(*uploads)
  end

  desc "Upload all the files specified by the 'copy_folders' var.".cleanup
  remote_task :upload_new_release => [ :ensure_release_path] do
    safe_rsync_release(*copy_folders)
    set_ownerships(release_path)
  end

  desc "Upload any folders listed by the share_folders variable, REPLACING existing ones.".cleanup
  remote_task :replace_shared_folders => [ :ensure_release_path] do
    share_folders.each do |sf|
      run "sudo rm -f #{shared_path}/#{sf}"
      rsync(sf, target_host + ":" + shared_path)
    end
    set_ownerships(shared_path)
  end

  remote_task :ensure_shared_folders => [  ] do
    share_folders.each do |sf|
      dir = "#{shared_path}/#{sf}"
      rc = run_rc "test -d #{dir}"
      if(rc != 0)
        puts "Creating shared directory via rsync: #{dir}"
        rsync(sf, target_host + ":" + shared_path)
      else 
        puts "Shared dir exists: #{dir}"
      end
    end
    set_ownerships(shared_path)
  end    

  remote_task :resync_shared_folders => [ :ensure_shared_folders] do
    # dont delete from shared folders, as this could cause application problems
    old_flags = get :rsync_flags
    set :rsync_flags, ['-azP',
                       '--exclude=.git', '--exclude=*.erb', '--exclude=.svn',
                       '--exclude=*~']
    share_folders.each do |sf|
      puts "syncing #{target_host}:#{shared_path}/#{sf}"
      rsync(sf, target_host + ":" + shared_path)
    end
    set_ownerships(shared_path)
    set :rsync_flags, old_flags
  end

  desc "Symlink anything in the shared directory to the most recent release folder.".cleanup
  remote_task :add_shared_to_current => [:ensure_shared_folders] do
    run "cd #{current_release} && ln -s #{shared_path}/* ."
  end

  desc "Create any extra symlinks described in 'extra_symlinks' var.".cleanup
  remote_task :make_extra_symlinks do
    Rake::RemoteTask.fetch(:extra_symlinks,[]).each do |p|
      target = p[0]
      name = p[1].sub(/^\//,'') #strip leading slash
      pth = "#{current_release}/#{name}"
      # make sure that the path we want exists then link to it (builds missing parent folders)
      run "mkdir -p #{pth} && rmdir #{pth} && ln -s -f #{target} #{pth}"
    end
  end

  desc "Creates the symlink to init.d and calls update-rc.d to make that active.".cleanup
  remote_task :create_initd do
    run "sudo ln -f -s #{current_path}/etc/init.d /etc/init.d/#{application}"
    run "sudo chmod ug+x #{current_path}/etc/init.d"
    run "sudo /usr/sbin/update-rc.d #{application} defaults 80"
  end

  desc "Creates the symlink to apache.conf from /etc/apache2/sites-enabled.".cleanup
  remote_task :create_apacheconf do
    run "sudo ln -f -s #{current_path}/etc/apache.conf /etc/apache2/sites-enabled/#{application}"
  end

  desc "Creates all the external links from the publish to the rest of the computer and vice versa".cleanup
  remote_task :external_links => [:create_initd, :create_apacheconf] do
  end

  desc "Ensure that all the required modules are present and enabled on the server.".cleanup
  remote_task :ensure_a2mods do
    Rake::RemoteTask.fetch(:a2mods,[]).each do |m|
      puts "Ensuring apache2 module '#{m}'" if $TRACE
      rc = run_rc("sudo /usr/sbin/a2enmod #{m} > /dev/null")
      raise "Mod: #{m} failed." unless rc == 0 #watch for rc of '0'
    end
  end
  
  desc "Make the last uploaded stuff be the current application.".cleanup
  remote_task :symlink_latest_as_current do
    run "rm -f #{current_path} && ln -s #{current_release} #{current_path}"
  end

  desc "(re)starts the lisp application and gracefully hit the webserver.".cleanup
  remote_task :restart do
    run "sudo #{current_path}/etc/init.d restart"
    run "sudo /usr/sbin/apache2ctl graceful"
    puts "Restarted lisp and apache on the remote server."
  end

  desc "Do a publish without restarting anything."
  remote_task :soft_publish=>[:gittag, :build, :soft_publish_existing] do
    puts "Everything published and linked. Nothing started or cycled (yet)."
  end

  desc "Do a publish without restarting or building anything."
  remote_task :soft_publish_existing=>[:rewrite_conf, :upload_new_release, 
                                       :add_shared_to_current, :make_extra_symlinks, 
                                       :symlink_latest_as_current, :ensure_a2mods,
                                       :ensure_apt_dependencies, :external_links] do
    puts "Everything published and linked. Nothing started or cycled (yet)."
  end

  desc "Do the full publish.".cleanup
  remote_task :publish=>[:soft_publish, :restart ] do
  end

  desc "First time publish should get shared_folders too.".cleanup
  remote_task :first_publish_setup=>[:soft_publish,:replace_shared_folders, :add_shared_to_current, :publish] do
  end

  remote_task :ensure_apt_dependencies do
    deps = Rake::RemoteTask.fetch(:apt_dependencies,[])

    deps.each do |m|
      puts "processing #{m}" if $TRACE
      if m.kind_of? Array
        prog,pkg = m
      else
        prog,pkg = m,m
      end
      puts "Checking for '#{prog}'" if $TRACE
      rc = run_rc("which #{prog} > /dev/null")
      if rc != 0
        puts "Didn't find '#{prog}', attempting to install '#{pkg}'" if $TRACE
        rc = sudo "aptitude install -q -y #{pkg} > /dev/null"
        raise "Failed to install '#{pkg}' to provide program '#{prog}'" if rc != 0
      end
    end
  end
end
