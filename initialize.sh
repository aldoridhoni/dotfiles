#!/usr/bin/env bash

# Usage info
show_help() {
    cat << EOF
Usage: ${0##*/} [-hcb] [PROGRAM NAME]...
Do the required step to initialize program.

   -h          display this help and exit
   -c          copy file instead of symbolic link
   -b          backup file
EOF
}

if [[ $# -lt 1 ]]; then
    #    show_help
    exit 0
fi

OPTIND=1
COPY=0
BACKUP=0
DISTRO=""

while getopts "hcb" opt; do
    case $opt in
        h)
            show_help
            exit 0
            ;;
        c)  COPY=$((COPY+1))
            ;;
        b)  BACKUP=$((BACKUP+1))
            ;;
        *)
            show_help >&2
            exit 1
            ;;
    esac
done


shift "$((OPTIND-1))"

main() {
    for program in "$@"
    do
        case $program in
            atom)
                init_atom
                break
                ;;
            emacs|spacemacs)
                init_spacemacs
                break
                ;;
            tmux)
                init_tmux
                break
                ;;
            vim)
                init_vim
                break
                ;;
            git)
                init_git
                break
                ;;
            rust)
                init_rust
                break
                ;;
            fish)
                init_fish
                break
                ;;
            *)
                echo "Program name is not in list" >&2
                echo "(atom, emacs/spacemacs, tmux, vim, git, rust)" >&2
                exit 1
                ;;
        esac
    done
}

action() {
    source="$1"
    dest="$2"
    echo "$source -> $dest"
    if [[ "$COPY" == "1" ]]; then
        cp -r "$source" "$dest" &&
        echo "Copying" "$source" "$dest"
    else
        ln -s "$source" "$dest" &&
        echo "Linking" "$source" "$dest"
    fi
}

function check_distro {
    platform=$(uname)

    if [[ "$platform" == 'Linux' ]]; then
        echo "Linux"
    elif [[ "$platform" == 'Darwin' ]]; then
        echo "Mac OS or iOS"
    elif [[ "$platform" == 'FreeBSD' ]]; then
        echo "BSD"
    fi
}

install_package() {
    check_distro
    package="$1"

    echo "Installing $1"
    echo "Installing package not yet implemented, consider installing yourself"
}

install_packages() {
    for package in "$@"
    do
        install_package $package
    done
}

function abs() {
    # $1 : relative filename
    if [ -d "$(dirname "$1")" ]; then
        echo "$(cd "$(dirname "$1")" && pwd)/$(basename "$1")"
    fi
}

function init_spacemacs {
    install_packages git emacs
    action "$(abs emacs/spacemacs.d)" $HOME/.spacemacs.d
    cd $HOME
    if [[ "$BACKUP" == "1" ]]; then
        mv .emacs.d .emacs.d.bak
        mv .emacs .emacs.bak
    fi
    git clone https://github.com/syl20bnr/spacemacs .emacs.d
    sudo pip install flake8 autoflake isort anaconda yapf
    sudo npm install -g tern vmd js-beautify eslint tide typescript tslint
}

function init_vim {
    install_packages git curl vim
    mkdir -p $HOME/.vim/bundle
    if [[ "$BACKUP" == "1" ]]; then
        mv $HOME/.vimrc $HOME/.vimrc.bak
    fi
    action "$(abs vim/vimrc)" $HOME/.vimrc
    action "$(abs vim/colors)" $HOME/.vim/colors
    git clone https://github.com/VundleVim/Vundle.vim.git $HOME/.vim/bundle/Vundle.vim
    echo "===="
    echo "Starting vim"
    vim +PluginInstall +qall
}

function init_tmux {
    install_packages tmux
    if [[ "$BACKUP" == "1" ]]; then
        mv $HOME/.tmux.conf $HOME/.tmux.conf.bak
    fi
    action "$(abs tmux)" $HOME/.tmux
    action $HOME/.tmux/main.conf $HOME/.tmux.conf
}

function init_fish {
    install_packages fish
    mkdir -p $HOME/.config/fish/conf.d
    if [[ "$BACKUP" == "1" ]]; then
        echo "Backup config.fish"
        mv $HOME/.config/fish/config.fish $HOME/.config/fish/config.fish.bak
    fi
    for file in config/fish/conf.d/*.fish; do
        action "$(abs $file)" $HOME/.config/fish/conf.d/
    done
    action "$(abs config/fish/config.fish)" $HOME/.config/fish/config.fish
    echo /usr/local/bin/fish | sudo tee -a /etc/shells
}

main $@
