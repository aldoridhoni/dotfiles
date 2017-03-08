if test -e '/etc/lsb-release' -o -e '/etc/os-release'
# GNU/Linux
    set -l dist (command cat /etc/*release 2>/dev/null | head -n1)
    switch $dist
        case "Fedora*"
            # Default blueish
        case 'NAME="Arch Linux"'
            # Set color theme RED
            set fish_color_autosuggestion 969896
            set fish_color_command d70000
            set fish_color_comment e7c547
            set fish_color_cwd red
            set fish_color_end c397d8
            set fish_color_error d75f00
            set fish_color_operator green
            set fish_color_param d75f00
            set fish_color_quote b9ca4a
            set fish_color_redirection 70c0b1
            set fish_color_status red
            set fish_color_user red
        case 'NAME="openSUSE*'
            # Set color theme Green
            set fish_color_cwd 73ba25
            set fish_color_command 00a489
            set fish_color_error 35b9ab
            set fish_color_redirection 21a4df
    end
end
# Mac

# Debian
if test -e '/etc/debian_version'
    set fish_color_autosuggestion 555
    set fish_color_command 005fd7
    set fish_color_comment 990000
    set fish_color_cwd e78c45
    set fish_color_end 009900
    set fish_color_error ff0000
    set fish_color_escape 'bryellow' '--bold'
    set fish_color_param 00afff
    set fish_color_quote 999900
    set fish_color_redirection 00afff
    set fish_color_status e78c45
    set fish_color_user e78c45

    # ls color output
    set LS_COLORS $LS_COLORS'di=38;5;172:'
end

# ungu = c397d8
# jingga = e78c45
