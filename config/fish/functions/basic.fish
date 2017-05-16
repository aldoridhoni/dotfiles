function basic
  if test -e "$HOME/dotfiles"
    set -l dotfiles "$HOME/dotfiles"
    set -l path $dotfiles/emacs/basic
    command env HOME=$path emacs --maximized $argv
  end
end
