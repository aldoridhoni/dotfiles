function basic
  set -l dotfiles "$HOME/dotfiles"
  set -l path $dotfiles/emacs/basic
  if not command emacsclient --socket-name=basic --no-wait -a "$path/start.sh" $argv 2>/dev/null
    command env HOME=$path REAL_HOME=$HOME emacs $argv
  end
end
