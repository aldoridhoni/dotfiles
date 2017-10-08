function base16 --description 'Set terminal base16'
    set -g BASE16_THEME "tomorrow-night"
    if test -n "$argv[1]"
        set BASE16_THEME "$argv[1]"
    end
    set -l SCRIPT "$HOME/.config/base16-shell/scripts/base16-$BASE16_THEME.sh"
    if test -e $SCRIPT; and status --is-interactive; and not inside_emacs
        eval sh '"'$SCRIPT'"'
    else
        # fish port of base16-shell
        # tomorrow-night
        set -l color00 "1d/1f/21" # Base 00 - Black
        set -l color01 "cc/66/66" # Base 08 - Red
        set -l color02 "b5/bd/68" # Base 0B - Green
        set -l color03 "f0/c6/74" # Base 0A - Yellow
        set -l color04 "81/a2/be" # Base 0D - Blue
        set -l color05 "b2/94/bb" # Base 0E - Magenta
        set -l color06 "8a/be/b7" # Base 0C - Cyan
        set -l color07 "c5/c8/c6" # Base 05 - White
        set -l color08 "96/98/96" # Base 03 - Bright Black
        set -l color09 $color01 # Base 08 - Bright Red
        set -l color10 $color02 # Base 0B - Bright Green
        set -l color11 $color03 # Base 0A - Bright Yellow
        set -l color12 $color04 # Base 0D - Bright Blue
        set -l color13 $color05 # Base 0E - Bright Magenta
        set -l color14 $color06 # Base 0C - Bright Cyan
        set -l color15 "ff/ff/ff" # Base 07 - Bright White
        set -l color16 "de/93/5f" # Base 09
        set -l color17 "a3/68/5a" # Base 0F
        set -l color18 "28/2a/2e" # Base 01
        set -l color19 "37/3b/41" # Base 02
        set -l color20 "b4/b7/b4" # Base 04
        set -l color21 "e0/e0/e0" # Base 06
        set -l color_foreground "c5/c8/c6" # Base 05
        set -l color_background "1d/1f/21" # Base 00

        if test -n "$TMUX"
            # Tell tmux to pass the escape sequences through
            # (Source: http://permalink.gmane.org/gmane.comp.terminal-emulators.tmux.user/1324)
            function printf_template; builtin printf '\ePtmux;\e\e]4;%d;rgb:%s\e\e\\\\\e\\' $argv; end
            function printf_template_var; builtin printf '\ePtmux;\e\e]%d;rgb:%s\e\e\\\\\e\\' $argv; end
            function printf_template_custom; builtin printf '\ePtmux;\e\e]%s%s\e\e\\\\\e\\' $argv; end
        else if string match --quiet "screen*" "$TERM"
            # GNU screen (screen, screen-256color, screen-256color-bce)
            function printf_template; builtin printf '\eP\e]4;%d;rgb:%s\e\\' $argv; end
            function printf_template_var; builtin printf '\eP\e]%d;rgb:%s\e\\' $argv; end
            function printf_template_custom; builtin printf '\eP\e]%s%s\e\\' $argv; end
        else if string match --quiet "linux*" "$TERM"
            function printf_template
                if test $argv[1] -le 15
                    builtin printf '\e]P%x%s' $argv[1] (echo $argv[2] | sed 's/\///g')
                end
            end
            alias printf_template_var=true
            alias printf_template_custom=true
        else
            function printf_template; builtin printf '\e]4;%d;rgb:%s\e\\' $argv; end
            function printf_template_var; builtin printf '\e]%d;rgb:%s\e\\' $argv; end
            function printf_template_custom; builtin printf '\e]%s%s\e\\' $argv; end
        end

        # 16 color space
        printf_template 0  $color00
        printf_template 1  $color01
        printf_template 2  $color02
        printf_template 3  $color03
        printf_template 4  $color04
        printf_template 5  $color05
        printf_template 6  $color06
        printf_template 7  $color07
        printf_template 8  $color08
        printf_template 9  $color09
        printf_template 10 $color10
        printf_template 11 $color11
        printf_template 12 $color12
        printf_template 13 $color13
        printf_template 14 $color14
        printf_template 15 $color15

        # 256 color space
        printf_template 16 $color16
        printf_template 17 $color17
        printf_template 18 $color18
        printf_template 19 $color19
        printf_template 20 $color20
        printf_template 21 $color21

        # foreground / background / cursor color
        if test -n "$ITERM_SESSION_ID"
            # iTerm2 proprietary escape codes
            printf_template_custom Pg c5c8c6 # foreground
            printf_template_custom Ph 1d1f21 # background
            printf_template_custom Pi c5c8c6 # bold color
            printf_template_custom Pj 373b41 # selection color
            printf_template_custom Pk c5c8c6 # selected text color
            printf_template_custom Pl c5c8c6 # cursor
            printf_template_custom Pm 1d1f21 # cursor text
        else
            printf_template_var 10 $color_foreground
            if test      "$BASE16_SHELL_SET_BACKGROUND" != "false"
                printf_template_var 11 $color_background
                if string match --quiet "rxvt*" "$TERM"
                    printf_template_var 708 $color_background # internal border (rxvt)
                end
            end
            printf_template_custom 12 ";7" # cursor (reverse video)
        end

        functions --erase printf_template
        functions --erase printf_template_custom
        functions --erase printf_template_var
    end
end
