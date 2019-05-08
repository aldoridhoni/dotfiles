# Defined in /home/aldo/.config/fish/conf.d/tmux.fish @ line 4
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
