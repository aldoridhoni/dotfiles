# For fish before 2.3.0
switch "$FISH_VERSION"
    case 2.2.0 2.1.2 2.1.1 2.1.0 2.0.0
        for file in ~/.config/fish/conf.d/*
            [ -f $file -a -r $file ]; and source $file
        end
end

# UTF
set -x LANG en_US.UTF-8

# Python
set -gx PYTHONDONTWRITEBYTECODE 1

# Git prompt
set -x __fish_git_prompt_show_informative_status 1
set -x __fish_git_prompt_char_stateseparator '|'
set -x __fish_git_prompt_char_dirtystate '*'
set -x __fish_git_prompt_char_invalidstate '#'
set -x __fish_git_prompt_char_stagedstate '+'
set -x __fish_git_prompt_char_stashstate '$'
