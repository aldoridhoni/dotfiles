function basic
    set -l path $HOME/basic-emacs
    if not command emacsclient --socket-name=basic -a "$path/start.sh" $argv 2>/dev/null
        command env HOME=$path REAL_HOME=$HOME emacs $argv
    end
end
