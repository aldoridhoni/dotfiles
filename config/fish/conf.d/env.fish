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
