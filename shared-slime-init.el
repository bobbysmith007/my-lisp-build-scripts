(or (fboundp 'match-string-no-properties)
     (defun match-string-no-properties (number)
       "Return string of text matched by last search."
       (buffer-substring-no-properties (match-beginning number)
				       (match-end number))))

(push ".fasl" completion-ignored-extensions)
(push "_darcs/" completion-ignored-extensions)
(push ".lib" completion-ignored-extensions)
(push ".dll" completion-ignored-extensions)
(push ".so" completion-ignored-extensions)

;(push "L:/Slime" load-path)
;(push "L:/slime/contrib" load-path)

(require 'slime)
(slime-setup '(slime-presentation-streams
               slime-fancy slime-asdf
               slime-tramp slime-indentation
               slime-compiler-notes-tree
               slime-sprof))


;;;;Functions for startin a lisp
(defun slime ()
  "Start Slime without an inferior lisp."
  (interactive)
  (slime-connect "localhost" 4005))


;;;;;; Specify modes for Lisp file extensions
(setq auto-mode-alist
      (append '(("\.lisp$" . lisp-mode)
		("\.lsp$" . lisp-mode)
		("\.cl$" . lisp-mode)
		("\.asd$" . lisp-mode)
		("\.system$" . lisp-mode)) auto-mode-alist))

;; Hooks into lisp mode
(add-hook 'lisp-mode-hook
	  (lambda () (slime-mode t)))

(add-hook 'inferior-lisp-mode-hook
	  (lambda () (inferior-slime-mode t)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'mic-paren)
(paren-activate)
(setf paren-priority 'close)

;;some slime settings
(setq
      ;common-lisp-hyperspec-root "file:///L:/HyperSpec/HyperSpec/"
      lisp-indent-function 'cl-indent:function
      slime-complete-symbol-function 'slime-fuzzy-complete-symbol
      slime-startup-animation t
      slime-repl-return-behaviour :send-only-if-after-complete)


(defun mb:move-past-close ()
  "Move past next `)' and ensure just one space after it."
  (interactive)
  (delete-horizontal-space)
  (indent-according-to-mode)
  (up-list))

(defun mb:unwrap-next-sexp (&optional kill-n-sexps)
  "Convert (x ...) to ..."
  (interactive "P")
  (unless kill-n-sexps
    (setf kill-n-sexps 1))
  (save-excursion
    (forward-sexp)
    (backward-delete-char 1)
    (backward-up-list)
    (delete-char 1)
    (unless (zerop kill-n-sexps)
      (kill-sexp kill-n-sexps)))
  (just-one-space)
  (indent-according-to-mode))

(defun adw:unwrap-next-sexp-0 (&optional foo)
  "Convert (a (...) b) to (a ... b)"
  (interactive "P")
  (mb:unwrap-next-sexp 0))

(defun adw:splice-sexp (&optional foo)
  "Convert (a (...) b) to (a ... b)"
  (interactive "P")
  (save-excursion
   (backward-up-list)
   (mb:unwrap-next-sexp 0)
   ))

(defun mb:center-next-sexp (&optional num-sexps)
  (interactive "P")
  (save-excursion
    (forward-sexp num-sexps)
    (let ((end-point (point)))
      (backward-sexp num-sexps)
      (let* ((start-point (point))
	     (start-line (count-lines (point-min) (point)))
	     (num-lines (count-lines start-point end-point)))
	(goto-line (+ start-line (/ num-lines 2)))
	(recenter)))))


;;general lisp mode keybindings
(defun add-key-to-lisp-maps (key fun)
  (define-key slime-mode-map key fun)
  (define-key slime-repl-mode-map key fun)
  (define-key emacs-lisp-mode-map key fun))

(add-key-to-lisp-maps (kbd "C-(") (lambda () "Insert an open parenthesis '('" (interactive) (insert "(")))
(add-key-to-lisp-maps (kbd "C-)") (lambda () "Insert a close parenthesis ')'" (interactive) (insert ")")))
(add-key-to-lisp-maps (kbd "(") 'insert-parentheses)
(add-key-to-lisp-maps (kbd ")") 'mb:move-past-close)

(add-key-to-lisp-maps (kbd "C-t") 'transpose-sexps)
(add-key-to-lisp-maps (kbd "C-M-t") 'transpose-chars)
(add-key-to-lisp-maps (kbd "C-k") 'kill-sexp)
(add-key-to-lisp-maps (kbd "C-M-k") 'kill-line)


(add-key-to-lisp-maps (kbd "C-'") 'mb:unwrap-next-sexp)
(add-key-to-lisp-maps (kbd "C-M-'") 'adw:unwrap-next-sexp-0)
(add-key-to-lisp-maps (kbd "M-s") 'adw:splice-sexp)
(add-key-to-lisp-maps (kbd "<C-right>") 'forward-sexp)
(add-key-to-lisp-maps (kbd "<C-left>") 'backward-sexp)

(when (fboundp 'backward-sexp-mark)
  (add-key-to-lisp-maps (kbd "<C-S-left>") 'backward-sexp-mark))
(when (fboundp 'forward-sexp-mark) 
  (add-key-to-lisp-maps (kbd "<C-S-right>") 'forward-sexp-mark))

(add-key-to-lisp-maps (kbd "C-M-l") 'mb:center-next-sexp)

(add-key-to-lisp-maps (kbd "C-x p") 'slime-toggle-profile-fdefinition)

(add-key-to-lisp-maps (kbd "C-c M-p") 'slime-repl-set-package)

(defun mb:complete-backslash ()
  (interactive)
  (if (eql last-command 'mb:complete-backslash)
      (progn
	(backward-delete-char 1)
	(slime-complete-symbol))
      (self-insert-command 1)))

(define-key slime-mode-map (kbd "<C-up>") 'backward-up-list)
(define-key emacs-lisp-mode-map (kbd "<C-up>") 'backward-up-list)

(define-key slime-mode-map (kbd "<C-down>") 'down-list)
(define-key emacs-lisp-mode-map (kbd "<C-down>") 'down-list)

(define-key slime-mode-map (kbd "\\") 'mb:complete-backslash)
(define-key slime-mode-map (kbd "C-c TAB") 'slime-complete-form)
(define-key slime-mode-map (kbd "C-SPC") 'slime-complete-form)
(define-key global-map (kbd "<f12>") 'slime-selector)
(define-key slime-mode-map (kbd "RET") 'newline-and-indent)
(define-key slime-mode-map (kbd "<return>") 'newline-and-indent)


;; Make up and down move up and down in teh completions buffer
(define-key slime-target-buffer-fuzzy-completions-map (kbd "<up>") 'slime-fuzzy-prev)
(define-key slime-target-buffer-fuzzy-completions-map (kbd "<down>") 'slime-fuzzy-next)

(defun slime-fuzzy-select-and-space ()
  "Selects the current completion, making sure that it is inserted into the target buffer with an extra space.  This tells the connected Lisp what completion was selected."
  (interactive)
  (when slime-fuzzy-target-buffer
    (let ((buff slime-fuzzy-target-buffer))
      (slime-fuzzy-select)
      (with-current-buffer buff
	(insert-and-inherit " ")))))
(define-key slime-fuzzy-completions-map (kbd "SPC") 'slime-fuzzy-select-and-space)


; fix [] in slime
(add-hook 'lisp-mode-hook
	  (lambda ()
	    (slime-mode t)
	    (modify-syntax-entry ?\[ "(]  " lisp-mode-syntax-table)
	    (modify-syntax-entry ?\] ")[  " lisp-mode-syntax-table)))



(defun shared-lisp-map-remote (map) (car map))
(defun shared-lisp-map-local (map) (cdr map))
(defun string-starts-with (str test-str)
  (string= (downcase test-str)
           (downcase (substring str 0 (length test-str)))))


(or (fboundp 'replace-in-string)
    (defun replace-in-string (string find replace)
      (replace-regexp-in-string find replace string)))


(push (slime-create-filename-translator  :machine-instance "mahes")
      slime-filename-translations)


(defconst *progden-lisp-map* '(("/home/ACCELERATION/" . "S:/home/")
			       ("/usr/local/share/common-lisp/source/" . "S:/site/")
			       ("/opt/lisp" . "S:")
			       ("/var/git" . "S:/gitroot")
			       ("/var/darcs" . "S:/darcs-root")))

(defconst *sekhmet-lisp-map* '(("/home/ACCELERATION/" . "L:/home/")
			       ("/usr/local/lib/sbcl/site" . "L:/site")
			       ("/var/local/lisp" . "L:")
			       ("/var/git" . "L:/gitroot")
			       ("/var/darcs" . "L:/darcs-root")))

(push (list "sekhmet"
            (lambda (local-filename)
              (setf local-filename (replace-in-string local-filename "\\\\" "/"))
              (let ((the-map (find-if (lambda (amap)
                                        (string-starts-with local-filename (shared-lisp-map-local amap)))
                                      *sekhmet-lisp-map*)))
                (concat (shared-lisp-map-remote the-map)
                        (substring local-filename (length (shared-lisp-map-local the-map))))))
            (lambda (remote-filename)
              (let ((the-map (find-if (lambda (amap)
                                        (string-starts-with remote-filename (shared-lisp-map-remote amap)))
                                      *sekhmet-lisp-map*)))
                (concat (shared-lisp-map-local the-map)
                        (substring remote-filename
                                   (length (shared-lisp-map-remote the-map)))))))
      slime-filename-translations)

(push (list "progden"
            (lambda (local-filename)
              (setf local-filename (replace-in-string local-filename "\\\\" "/"))
              (let ((the-map (find-if (lambda (amap)
                                        (string-starts-with local-filename (shared-lisp-map-local amap)))
                                      *progden-lisp-map*)))
                (concat (shared-lisp-map-remote the-map)
                        (substring local-filename (length (shared-lisp-map-local the-map))))))
            (lambda (remote-filename)
              (let ((the-map (find-if (lambda (amap)
                                        (string-starts-with remote-filename (shared-lisp-map-remote amap)))
                                      *progden-lisp-map*)))
                (concat (shared-lisp-map-local the-map)
                        (substring remote-filename
                                   (length (shared-lisp-map-remote the-map)))))))
      slime-filename-translations)

