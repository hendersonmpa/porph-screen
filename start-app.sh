#!/bin/bash

screen -d -m sbcl --load start-app.lisp

#nohup sbcl --script start-app.lisp > /dev/null 2> /dev/null &
