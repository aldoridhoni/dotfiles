alias t="command tmux -2 new-session -A -s"
alias t-kill="command tmux kill-session -t"

function present
    if command tmux -L 1 has-session -t "-" 2> /dev/null
        if inside-tmux
            # echo "Now you can switch to session '-' with Prefix + s"
            echo "Now you must exit this tmux session."
        else
            command tmux -L 1 attach -t "-"
        end
    else
        command env -i THEME=light tmux -L 1 -2 new -s "-" -d
        present
    end
end

function t-new --description "Create new tmux session in current server and attach to it."
    command tmux new-session -d -s $argv
    if inside-tmux
        command tmux switch-client -t $argv
    else
        command tmux attach-session -t $argv
    end
end

function inside-tmux --description "Check if inside tmux"
    if test \( -n "$TMUX" -a -n "$TMUX_VERSION" \)
        echo 'Inside tmux'
        return 0
    end
    return 1
end
