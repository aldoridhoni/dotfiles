if echo | command less --quit-if-one-screen --line-numbers --no-init "" >/dev/null 2>&1
  function less
    command less --quit-if-one-screen --line-numbers --no-init $argv
  end
end
