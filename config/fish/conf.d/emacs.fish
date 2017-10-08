# Emacs
alias e="command emacsclient"
alias en="e -n"
alias em="e -t"

function sem
    command sudo emacsclient -t $argv
end

function emc --description 'Run emacs with new frame'
    e -c -a emacs $argv
end

function inside_emacs
    if test "$TERM" = "eterm-color" -o \( -n "$EMACS" -a -n "INSIDE_EMACS" \)
        return 0
    end
    return 1
end

# emacs-shell
if inside_emacs
    alias tmux="command tmux -L emacs new-session -A -s" # don't force 256color
    function fish_title; end;

    function man --description 'Open manual in Emacs window'
        e -e "(switch-to-buffer-other-window (manual-entry \"$argv\"))" > /dev/null
    end

    function make-term --description 'Run program in term buffer'
        e -e "(progn (switch-to-buffer (make-term $argv))(multi-term-internal))" > /dev/null
    end

    function eshell-command
        e -e "(eshell-command \"$argv\")" > /dev/null
    end

    function ssh
        eshell-command "ssh $argv"
    end

    function ff
        eshell-command "find-file $argv"
    end

end
