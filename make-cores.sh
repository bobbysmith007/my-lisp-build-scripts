#!/bin/bash

time sbcl --no-userinit --load make-cores.lisp 
sleep 2
for f in *.core; do
   echo "Unlinking: /opt/lisp/$f"
   unlink /opt/lisp/$f || true
done
mv *.core /opt/lisp
