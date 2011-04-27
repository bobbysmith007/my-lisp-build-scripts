#!/bin/bash
export CONFIGFILE=~/ucw-config.lisp
export SBCL_HOME=/usr/local/lib/sbcl
sbcl --script ~/run-builds.lisp "$@"
