#!/usr/bin/env bash

alias pacup="sudo pacman -Syu"
alias top="htop"
alias mkdir="mkdir -pv"
alias wget="wget -c"
alias c="tput clear"
alias py="env python"
alias py3="env python3"
alias df="df -h -T -l -x tmpfs -x devtmpfs"
alias free="free -h"
alias psu="ps -u $USER -o pid,args"
alias rm="rm -i"
alias b="c; tput cup $(tput lines)"
alias ll="ls -l"
alias la="ls -al"