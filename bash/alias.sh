#!/usr/bin/env bash

alias asciicast2gif='docker run --rm -v $PWD:/data asciinema/asciicast2gif'
alias b='c; tput cup $(tput lines)'
alias bulan='cal $(date +%m) $(date +%Y)'
alias c='tput clear'
alias curly='curl -C - -A "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/69.0.3497.100 Safari/537.36" -v'
alias df='df -h -T -l -x tmpfs -x devtmpfs'
alias e='emacsclient'
alias em='e -nw'
alias emc='e -c -a emacs'
alias emq='emacs -nw -Q -f menu-bar-mode -f global-hl-line-mode -f global-linum-mode --eval="(fset (quote yes-or-no-p) (quote y-or-n-p))" --eval="(progn(setq vc-follow-symlinks t)(load-theme (quote leuven)))" --eval="(setq make-backup-files nil)"'
alias ff='firefox'
alias ffmpeg="ffmpeg -hide_banner"
alias free='free -h'
alias g="git"
alias gmount="gio mount"
alias gumount="gio mount -u"
alias icat="kitty +kitten icat"
alias j='jmacs'
alias la='ll -ah'
alias lc='ls --color'
alias less='less --quit-if-one-screen --line-numbers --no-init'
alias ll='lc -l'
alias lol='la | lolcat'
alias mkdir='mkdir -pv'
alias mpvv="mpv --vo=tct --profile=sw-fast --no-terminal --vo-tct-width=$(tput cols) --vo-tct-height=$(tput lines)"
alias note="cd ~/Notebook; jupyter notebook --ip=127.0.0.1 --port=7777"
alias o='popd'
alias open=$(which xdg-open 2>/dev/null|| which gnome-open 2>/dev/null|| which open 2>/dev/null)
alias p='pushd'
alias pacup='sudo pacman -Syu'
alias pbcopy='xsel --clipboard --input'
alias pbpaste='xsel --clipboard --output'
alias pip='python3 -m pip'
alias pipu='pip install --upgrade --user'
alias playt="mpv --ytdl-format='bestvideo[height<=?1440]+bestaudio/best'"
alias prambors='mpv --vo=null https://22283.live.streamtheworld.com/PRAMBORS_FM.mp3'
alias prevd='cd -'
alias psu='ps -u $USER -o pid,args'
alias pubip='curl -q http://ipv4.whatismyip.akamai.com/'
alias pubiphtml='curl -q http://checkip.dyndns.org/'
alias pulseaudio-volume-control="pavucontrol"
alias py3='env python3'
alias py='env python'
alias qr='qrencode -t ansiutf8'
alias rec='asciinema rec $(random).cast'
alias rm='rm -i'
alias sapt='sudo apt'
alias sdnf='sudo dnf'
alias server='python3 -m http.server'
alias speak=$(which speak-ng 2>/dev/null|| which speak 2>/dev/null)
alias spool='ff localhost:631'
alias szypper='sudo zypper'
alias t='tmux -2 new-session -A -s'
alias testcolor='msgcat --color=test'
alias top='htop'
alias v="vim -u NONE -N -n '+set number' '+syntax enable'"
alias vpn='sudo wg-quick up wg0'
alias wanip4akamai='dig @ns1-1.akamaitech.net ANY whoami.akamai.net +short -4'
alias wanip4opendns='dig @resolver1.opendns.com A myip.opendns.com +short -4'
alias wanipgoogle='dig @ns1.google.com TXT o-o.myaddr.l.google.com +short'
alias wget='wget -c'
