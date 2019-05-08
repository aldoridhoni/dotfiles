function dnf
	if command sudo 2>/dev/null
		command sudo dnf $argv;
	end
end
