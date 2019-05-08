# Defined in - @ line 2
function df
	if is_mac
        command df -h -l
    else
        command df -h -T -l -x tmpfs -x devtmpfs --sync $argv
    end
end
