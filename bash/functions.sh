#!/usr/bin/env bash

function nonzero_return() {
	[[ $RETVAL -ne 0 ]] && printf " [$RETVAL]"
	unset RETVAL
}

function git_branch() {
	if command_exists git; then
		if [[ -f ${BASHRC_DIR}/git-prompt.sh ]]; then
			source "${BASHRC_DIR}/git-prompt.sh"
			export GIT_PS1_SHOWDIRTYSTATE=1
			printf "$(__git_ps1)"
			return 0
		fi
		if git branch &>/dev/null; then
			if [[ $(git branch) ]]; then
				local BRANCH=$(git branch 2>/dev/null | grep \* |  cut -d " " -f 2)
				printf " ($BRANCH)"
			fi
			if [[ $(git status) ]]; then
				local STATUS=$(git status -u no --ignored=no 2>/dev/null)
				# TODO
			fi
		fi
	fi
}

function fn_sgr_end() {
	echo -en "\e[0m"
}

function fn_sgr_output() {
	local _param=$1
	echo -en "\e[${_param}m"
}

function fn_sgr_fg() {
	local -i _color=$1
	fn_sgr_output "38;5;${_color}"
}

function fn_sgr_bg() {
	local -i _color=$1
	fn_sgr_output "48;5;${_color}"
}

function prompt_command() {
	# get return value
	RETVAL=$?
	return_info

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

case $EUID in
	0) ARROW=$(echo -ne '\u203c\ufe0e\c') ;; # ‼
	*) ARROW=$(echo -ne '\u227b\c') ;; # ≻
esac

# Console tty
if [[ $TERM = 'linux' ]]; then
	case $EUID in
		0) ARROW="#" ;;
		*) ARROW=">" ;;
	esac
fi

# Assuming tput is available
if command_exists tput; then
	ncolor=$(tput colors 2>/dev/null)

	function fn_sgr_end() {
		echo -n "$(tput sgr0)"
	}

	function fn_sgr_fg() {
		echo -n "$(tput setaf $1)"
	}

	function fn_sgr_br() {
		echo -n "$(tput setab $1)"
	}

	trap 'get_window_size' WINCH
	function get_window_size() {
		local -i _WINDOW_X=$(tput lines)
		local -i _WINDOW_Y=$(tput cols)
		# echo "\n[resize] ${_WINDOW_X}x${_WINDOW_Y}"
	}
fi

# need to shrink \u@\h at low col number.
function return_info() {
	if [[ -n $COLUMNS && $COLUMNS -lt 50 ]]; then
		INFO=":"
	else
		INFO="$USER@$HOSTNAME"
	fi
}

if [[ $TERM =~ color ]] || [[ -n $ncolor && $ncolor -ge 8 ]]; then
	# Ansi color space
	# If using real green & red, not easy to look at with white background.
	# Also emacs ansi-term only has 8 colors.
	_RED="1" # real red 196
	_GREEN="2" # real green 46

	#fn_sgr_output
	_STANDOUT="3"
	_RMSO="23"
	_BLINK="5"

	COLOR_PS1='${INFO} \[$(fn_sgr_fg $_GREEN)\]${PS1X}\[$(fn_sgr_fg $_RED)\]\
$(git_branch)$(nonzero_return)\[$(fn_sgr_end)\]${ARROW} '
	COLOR_PS2='\[$(fn_sgr_fg $_GREEN)\]${ARROW}\[$(fn_sgr_end)\] '
	COLOR_PS4='\[$(fn_sgr_fg $_GREEN)\]+\[$(fn_sgr_end)\] '

	export PS1=$COLOR_PS1
	export PS2=$COLOR_PS2
	export PS4=$COLOR_PS4
else
	export PS1='\u@\h ${PS1X}$(git_branch)$(nonzero_return)\$ '
	export PS2='\$ '
	export PS4='+ '
fi

# Update Atom in Fedora
function update_atom() {
	local ATOM_INSTALLED_VERSION=$(rpm -qi atom | grep "Version" | cut -d ':' -f 2 | cut -d ' ' -f 2)
	local ATOM_LATEST_VERSION=$(curl -sL "https://api.github.com/repos/atom/atom/releases/latest" \
					| grep -E "https.*atom-amd64.tar.gz" | cut -d '"' -f 4 | cut -d '/' -f 8 | sed 's/v//g')

	if [[ $ATOM_INSTALLED_VERSION < $ATOM_LATEST_VERSION ]]; then
		sudo dnf install -y https://github.com/atom/atom/releases/download/v${ATOM_LATEST_VERSION}/atom.x86_64.rpm
	fi
}

# Diff two zip
function zipdiff() {
	diff -y <(unzip -l $1) <(unzip -l $2) --suppress-common-lines;
}

# Inside Emacs
function inside_ansi_term() {
	if [[ -v INSIDE_EMACS ]]; then
		local version=$(echo $INSIDE_EMACS | tr ',' '\n' | head -n 1)
		if command_exists toilet; then
			toilet -f pagga -Skt 'Emacs ' $version
		elif command_exists figlet; then
			figlet -f pagga -Skp 'Emacs ' $version
		else
			echo 'Emacs ' $version
		fi
	fi
}
inside_ansi_term

function inside_asciinema() {
	if [[ -v ASCIINEMA_REC ]]; then
		local BG="$(tput setab 1)" # red
		local FG="$(tput setaf 7)" # white
		local BOLD="$(tput bold)"
		local RESET="$(tput sgr0)"
		local BLINK="$(tput blink)"
		local BUTTON="*"

		[ "${BASH_VERSINFO[0]}" -ge 4 ] && \
			BUTTON="$(echo -ne '\U23fa\Ufe0e')"

		export PS1=$BG$FG$BOLD$BUTTON$BLINK" REC "$RESET" "$PS1
	fi
}
inside_asciinema

function random() {
	# For integer random use $RANDOM
	# 2 byte binary from urandom
	head -c 2 /dev/urandom | xxd -b -g 2 | awk '{print $2; exit}'
}

function ibeam_cursor() {
	printf '\033[5 q'
}

function less_colors() {
	export MANPAGER='less -s -M +Gg'
	export LESS_TERMCAP_mb=$'\e[1;31m'     # begin bold
	export LESS_TERMCAP_md=$'\e[1;33m'     # begin blink
	export LESS_TERMCAP_so=$'\e[01;44;37m' # begin reverse video
	export LESS_TERMCAP_us=$'\e[01;37m'    # begin underline
	export LESS_TERMCAP_me=$'\e[0m'        # reset bold/blink
	export LESS_TERMCAP_se=$'\e[0m'        # reset reverse video
	export LESS_TERMCAP_ue=$'\e[0m'        # reset underline
	export GROFF_NO_SGR=1                  # for konsole and gnome-terminal
}

# PATH Manipulation; taken from /etc/profile
function pathmunge() {
	case ":${PATH}:" in
		*:"$1":*)
		;;
		*)
			if [ "$2" = "after" ] ; then
				PATH=$PATH:$1
			else
				PATH=$1:$PATH
			fi
	esac
}

function remove_dups_path() {
	# https://unix.stackexchange.com/a/338737
	local D=${2:-:} path= dir=
	while IFS= read -d$D dir; do
		[[ $path$D =~ .*$D$dir$D.* ]] || path+="$D$dir"
	done <<< "$1$D"
	printf %s "${path#$D}"
}

function path() {
	local old=$IFS
	IFS=:
	printf "%s\n" $PATH
	IFS=$old
}
