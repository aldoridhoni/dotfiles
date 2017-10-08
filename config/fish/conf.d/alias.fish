# Aliases
alias c="command tput clear"
alias py="command env python"
alias py3="command env python3"
alias df="command df -h -T -l -x tmpfs -x devtmpfs"
alias free="command free -h"
alias psu="command ps -u $USER -o pid,args"
alias rm="command rm -i"

# Functions
function b --description 'Natural clear'
    c
    command tput cup (tput lines)
end

function mkdir --description 'Make dir and change to it'
    command mkdir -pv $argv
    cd (echo $argv |command rev|cut -d' ' -f1|command rev)
end

function curly --description 'curl with custom useragent and resume download'
    set -l ua "Mozilla/5.0 (Macintosh; Intel Mac OS X 11_11) \
    AppleWebKit/638.34.48 (KHTML, like Gecko) \Version/12.0 Safari/638.35.8"
    curl -C - -A $ua -v $argv
end

function rec
    set -l now (date +%Y%m%d-%H%M)
    command asciinema rec -w 1 "asciinema-$now.json"
end

function pl
    command asciinema play $argv
end

function prambors --description 'Listen to prambors radio using VLC'
    command cvlc 'http://masima.rastream.com/masima-pramborsjakarta'
end
