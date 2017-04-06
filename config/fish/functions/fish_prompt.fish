function fish_prompt --description 'Write out the prompt'
	# Just calculate these once, to save a few cycles when displaying the prompt
   if not set -q __fish_prompt_hostname
       set -g __fish_prompt_hostname (hostname -s)
   end

   if not set -q __fish_prompt_normal
       set -g __fish_prompt_normal (set_color normal)
   end

   if not set -q __fish_prompt_cwd
       set -g __fish_prompt_cwd (set_color $fish_color_cwd)
   end

   set -g __info ''

   if test -n "$ASCIINEMA_REC"
     set -g __info $__info(set_color red)"(rec)"(set_color normal)
   end
   if test -n "$SSH_CLIENT" -a "$SSH_TTY"
     set -g __info $__info(set_color yellow)"ssh:"(set_color normal)
   end

   if test 72 -gt (tput cols)
     echo -n -s "$__fish_prompt_cwd" (prompt_pwd) "$__fish_prompt_normal" '> '
     return
   end

   echo -n -s "$__info" "$USER" @ "$__fish_prompt_hostname" ' ' "$__fish_prompt_cwd" (prompt_pwd) (set_color brred) (__fish_vcs_prompt) "$__fish_prompt_normal" '> '

end
