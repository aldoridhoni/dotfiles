function basic
  if command emacsclient --socket-name=basic --no-wait --eval "(version)" > /dev/null 2>&1
    echo "Server already running."
    if count $argv > /dev/null
      command emacsclient --socket-name=basic --no-wait $argv
    else
      echo "Creating new frame."
      command emacsclient --socket-name=basic --no-wait -c
    end
  else if test -e "$HOME/dotfiles"
    set -l dotfiles "$HOME/dotfiles"
    set -l path $dotfiles/emacs/basic
    command env HOME=$path REAL_HOME=$HOME emacs $argv
  end
end
