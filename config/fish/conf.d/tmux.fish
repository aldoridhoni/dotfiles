alias t="command tmux -2 new-session -A -s"

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

function inside-tmux --description 'Check if inside tmux'
  if test \( -n "$TMUX" -a -n "$TMUX_VERSION" \)
    echo 'Inside tmux'
    return 0
  end
  return 1
end
