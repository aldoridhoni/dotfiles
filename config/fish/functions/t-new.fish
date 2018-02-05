# Defined in /home/aldo/.config/fish/conf.d/tmux.fish @ line 18
function t-new --description 'Create new tmux session in current server and attach to it.'
	command tmux new-session -d -s $argv
    if inside-tmux
        command tmux switch-client -t $argv
    else
        command tmux attach-session -t $argv
    end
end
