#!/bin/bash
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
env HOME="$DIR" REAL_HOME="$HOME" emacs --maximized $@
