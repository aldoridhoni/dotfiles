# Defined in - @ line 0
function t-kill --description 'alias t-kill=command tmux kill-session -t'
	command tmux kill-session -t $argv;
end
