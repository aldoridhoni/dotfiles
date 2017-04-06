# Trivial alias
function df --description 'Displays disk free space'
  command df -H $argv
end

function vim --description 'Kill the habit'
  if test "$EDITOR" = "vim"
    command vim $argv
  else
    em $argv
  end
end

function em
  command emacsclient -t $argv
end

function sem
  command sudo emacsclient -t $argv
end

function emc
  command emacsclient -c -a emacs $argv
end

function t
  command tmux -2 new-session -A -s $argv
end

function c
  command tput clear
end

function b
  set -l lines
  set lines (tput lines)
  command tput clear
  command tput cup $lines
end

function curly
	curl -A "Mozilla/5.0 (Macintosh; Intel Mac OS X 11_11) AppleWebKit/638.34.48 (KHTML, like Gecko) Version/12.0 Safari/638.35.8" -v $argv;
end

function py
	command env python $argv;
end

function py3
  command env python3 $argv;
end

function rec
  set -l now (date +%Y%m%d-%H%M)
  command asciinema rec -w 1 "asciinema-"$now".json"
end

function pl
  command asciinema play $argv
end

function present
  set -x THEME 'light'
  command tmux -2 -L 1 new -A -s "-"
end

function prambors
  command cvlc 'http://masima.rastream.com/masima-pramborsjakarta'
end
