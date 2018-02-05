# Defined in /home/aldo/.config/fish/conf.d/tmux.fish @ line 27
function inside-tmux --description 'Check if inside tmux'
	if test \( -n "$TMUX" -a -n "$TMUX_VERSION" \)
        return 0
    end
    return 1
end
