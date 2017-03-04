# Trivial alias
function df --description 'Displays disk free space'
  command df -H $argv
end

function em
  command emacs -nw $argv
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
	python $argv;
end
