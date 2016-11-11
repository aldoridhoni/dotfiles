set -x LANG en_US.UTF-8

# Latex
if test -e '/usr/local/textlive/2015/bin/x86_64-linux/tex'
  set fish_user_paths $fish_user_paths '/usr/local/texlive/2015/bin/x86_64-linux'
end

# /usr/local/bin
set fish_user_paths $fish_user_paths '/usr/local/bin'

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
  command clear
end

if test -e '/etc/lsb-release' -o -e '/etc/os-release'
# GNU/Linux
set -l dist (command cat /etc/*release 2>/dev/null | head -n1 | awk '{print $1;}')
switch $dist
  case 'Fedora'
  case '*'
    # Set color theme
    set fish_color_autosuggestion 969896
    set fish_color_command d70000
    set fish_color_comment e7c547
    set fish_color_cwd red
    set fish_color_end c397d8
    set fish_color_error d75f00
    set fish_color_operator green
    set fish_color_param d75f00
    set fish_color_quote b9ca4a
    set fish_color_redrection 70c0b1
    set fish_color_status red
    set fish_color_user red
end
else
# Mac
end

