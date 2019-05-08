# Defined in - @ line 0
function t --description 'alias t=command tmux -2 new-session -A -s'
	command tmux -2 new-session -A -s $argv;
end
