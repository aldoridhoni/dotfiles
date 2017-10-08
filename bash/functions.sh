#!/usr/bin/env bash

GREEN="\[$(tput setaf 2)\]"
RESET="\[$(tput sgr0)\]"

function nonzero_return() {
	RETVAL=$?
	[ $RETVAL -ne 0 ] && echo " [$RETVAL]"
}

function prompt {
	if [[ "$TERM" =~ 256color ]]; then
		export PS1="\u@\h ${GREEN}\w${RESET}\$(nonzero_return)> "
		export PS2="${GREEN}>${RESET} "
		export PS4="${GREEN}+${RESET} "
	fi
}

# https://stackoverflow.com/questions/5076127/\
# bash-update-terminal-title-by-running-a-second-command/5080670#5080670
function settitle () {
	export PREV_COMMAND=${PREV_COMMAND}${@}
	printf "\033]0;%s\007" "${BASH_COMMAND//[^[:print:]]/}"
	export PREV_COMMAND=${PREV_COMMAND}' | '
}

# export PROMPT_COMMAND=${PROMPT_COMMAND}';export PREV_COMMAND=""'
export PROMPT_COMMAND=""
trap 'settitle "$BASH_COMMAND"' DEBUG
