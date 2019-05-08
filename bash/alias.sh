#!/usr/bin/env bash

alias pacup='sudo pacman -Syu'
alias top='htop'
alias mkdir='mkdir -pv'
alias wget='wget -c'
alias c='tput clear'
alias py='env python'
alias py3='env python3'
alias df='df -h -T -l -x tmpfs -x devtmpfs'
alias free='free -h'
alias psu='ps -u $USER -o pid,args'
alias rm='rm -i'
alias b='c; tput cup $(tput lines)'
alias ll='ls -l'
alias la='ll -ah'
alias pbcopy='xsel --clipboard --input'
alias pbpaste='xsel --clipboard --output'
alias g="git"
alias t='tmux -2 new-session -A -s'
alias rec='asciinema rec $(random).cast'
alias asciicast2gif='docker run --rm -v $PWD:/data asciinema/asciicast2gif'
alias lol='la | lolcat'
alias vpn='sudo wg-quick up wg0'
