# Emacs
function e
  command emacsclient $argv
end

function em --description 'Run emacsclient in terminal'
  e -t $argv
end

function sem
  command sudo emacsclient -t $argv
end

function emc --description 'Run emacs with new frame'
  e -c -a emacs $argv
end


# emacs-shell
if test "$TERM" = "eterm-color" -o \( -n "$EMACS" -a -n "INSIDE_EMACS" \)
  function fish_title; end; funcsave fish_title

  function man --description 'Open manual in Emacs window'
    e -e "(manual-entry \"$argv\")" > /dev/null
  end

  function make-term --description 'Run program in term buffer'
    e -e "(progn (switch-to-buffer (make-term $argv))(multi-term-internal))" > /dev/null
  end

  function eshell-command
    e -e "(eshell-command \"$argv\")" > /dev/null
  end

  function ssh
    # make-term \"ssh\" \"ssh\" nil \"$argv\"
    eshell-command "ssh $argv"
  end

  function ff
    eshell-command "find-file $argv"
  end

end
