# Aliases
alias t="command tmux -2 new-session -A -s"
alias c="command tput clear"
alias b="c; command tput cup (tput lines)"
alias py="command env python"
alias py3="command env python3"
alias df="command df -H"
alias free="command free -H"
alias psu="command ps -u $USER -o pid,args"
alias rm="command rm -i"

# Functions
function vim --description 'Kill the habit'
  if test "$EDITOR" = "vim"
    command vim $argv
  else
    em $argv
  end
end

function curly --description 'curl with custom useragent and resume download'
  set -l ua "Mozilla/5.0 (Macintosh; Intel Mac OS X 11_11) AppleWebKit/638.34.48 (KHTML, like Gecko) Version/12.0 Safari/638.35.8"
	curl -C - -A $ua -v $argv;
end

function rec
  set -l now (date +%Y%m%d-%H%M)
  command asciinema rec -w 1 "asciinema-$now.json"
end

function pl
  command asciinema play $argv
end

function present
  set -x THEME 'light'
  command tmux -2 -L 1 new -A -s "-"
end

function prambors --description 'Listen to prambors radio using VLC'
  command cvlc 'http://masima.rastream.com/masima-pramborsjakarta'
end
