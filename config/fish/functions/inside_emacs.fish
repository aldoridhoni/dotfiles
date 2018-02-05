# Defined in /home/aldo/.config/fish/conf.d/emacs.fish @ line 14
function inside_emacs
	if test "$TERM" = "eterm-color" -o \( -n "$EMACS" -a -n "INSIDE_EMACS" \)
        return 0
    end
    return 1
end
