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
alias lc='ls --color'
alias ll='lc -l'
alias la='ll -ah'
alias pbcopy='xsel --clipboard --input'
alias pbpaste='xsel --clipboard --output'
alias g="git"
alias t='tmux -2 new-session -A -s'
alias rec='asciinema rec $(random).cast'
alias asciicast2gif='docker run --rm -v $PWD:/data asciinema/asciicast2gif'
alias lol='la | lolcat'
alias vpn='sudo wg-quick up wg0'
alias qr='qrencode -t ansiutf8'
alias server='python3 -m http.server'
alias curly='curl -C - -A "Mozilla/5.0 (Windows NT 10.0; Win64; x64) \
AppleWebKit/537.36 (KHTML, like Gecko) Chrome/69.0.3497.100 Safari/537.36" -v'
alias e='emacsclient'
alias em='e -nw'
alias emc='e -c -a emacs'
alias emq='emacs -nw -Q -f menu-bar-mode -f global-hl-line-mode -f global-linum-mode --eval="(fset (quote yes-or-no-p) (quote y-or-n-p))" --eval="(progn(setq vc-follow-symlinks t)(load-theme (quote leuven)))"'
alias less='less --quit-if-one-screen --line-numbers --no-init'
alias prambors='cvlc -q http://masima.rastream.com/masima-pramborsjakarta'
alias sdnf='sudo dnf'
alias szypper='sudo zypper'
alias sapt='sudo apt'
alias pubip='curl -q http://ipv4.whatismyip.akamai.com/'
alias pubiphtml='curl -q http://checkip.dyndns.org/'
alias wanip4opendns='dig @resolver1.opendns.com A myip.opendns.com +short -4'
alias wanip4akamai='dig @ns1-1.akamaitech.net ANY whoami.akamai.net +short -4'
alias wanipgoogle='dig @ns1.google.com TXT o-o.myaddr.l.google.com +short'
alias pip='python3 -m pip'
alias pipu='pip --user --upgrade'
alias j='jmacs'
alias ff='firefox'
alias spool='ff localhost:631'
alias prevd='cd -'
alias p='pushd'
alias o='popd'
alias testcolor='msgcat --color=test'
alias open=$(which xdg-open 2>/dev/null|| which gnome-open 2>/dev/null|| which open 2>/dev/null)
alias speak=$(which speak-ng 2>/dev/null|| which speak 2>/dev/null)
alias v="vim -u NONE -N -n '+set number' '+syntax enable'"
alias note="cd ~/Notebook; jupyter notebook --ip=127.0.0.1 --port=7777"
alias bulan='cal $(date +%m) $(date +%Y)'
