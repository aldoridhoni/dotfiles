#!/usr/bin/env bash

GREEN="$(tput setaf 2)"
RED="$(tput setaf 1)"
RESET="$(tput sgr0)"

function nonzero_return() {
	RETVAL=$?
	[ $RETVAL -ne 0 ] && echo " [$RETVAL]"
}

function git_branch() {
	if git branch &>/dev/null; then
		BRANCH=$(git branch 2>/dev/null | grep \* |  cut -d " " -f 2)
		echo " ($BRANCH)"
	fi
}

function prompt_command() {
	# https://unix.stackexchange.com/questions/26844/
	PS1X=$(p="${PWD#${HOME}}"
		   [ "${PWD}" != "${p}" ] && printf "~"
		   [ "${PWD}" == "/" ] && printf "/"
		   IFS=/
		   for q in ${p:1}; do
			   printf /${q:0:1}
		   done
		   printf "${q:1}")
}
# https://stackoverflow.com/questions/5076127/
function settitle () {
	export PREV_COMMAND=${PREV_COMMAND}${@}
	printf "\033]0;%s\007" "${BASH_COMMAND//[^[:print:]]/}"
	export PREV_COMMAND=${PREV_COMMAND}' | '
}

# export PROMPT_COMMAND=${PROMPT_COMMAND}';export PREV_COMMAND=""'
# export PROMPT_COMMAND=""
# trap 'settitle "$BASH_COMMAND"' DEBUG

# Calling Prompt
export PROMPT_COMMAND=prompt_command;
if [[ "$TERM" =~ 256color ]]; then
    export PS1='\u@\h ${GREEN}${PS1X}${RESET}${RED}$(git_branch)$(nonzero_return)${RESET}> '
    export PS2='${GREEN}>${RESET} '
    export PS4='${GREEN}+${RESET} '
else
    export PS1='\u@\h ${PS1X}$(git_branch)$(nonzero_return)> '
    export PS2='> '
    export PS4='+ '
fi