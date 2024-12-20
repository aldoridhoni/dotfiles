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

selector() {
    select program in bash zsh emacs fish readline
    do
        echo "Initializing" $program
        main $program
    done
}

OPTIND=1
TARGET_DIR=${TARGET:-$HOME}
CONFIG_DIR=".config"
SCRIPTPATH="$(cd "$(dirname "$0")" ; pwd -P)"

while getopts "hcb" opt; do
    case $opt in
        h)
            show_help
            exit 0
            ;;
        c)  COPY=true
            ;;
        b)  BACKUP=true
            ;;
        *)
            show_help >&2
            exit 1
            ;;
    esac
done


shift "$((OPTIND-1))"


main() {
    check_distro
    # TODO : still not iterate over
    for program in "$@"
    do
        case $program in
            bash)
                init_bash
                ;;
            zsh)
                init_zsh
                ;;
            emacs|spacemacs)
                init_spacemacs
                ;;
            fish)
                init_fish
                ;;
            tmux)
                init_tmux
                ;;
            vim)
                init_vim
                ;;
            pip)
                init_pip
                ;;
            readline)
                init_readline
                ;;
            bin)
                init_bin
                ;;
            git)
                init_git
                ;;
            termux)
                init_termux
                ;;
            atom)
                init_atom
                ;;
            code|vscode)
                init_vscode
                ;;
            rust)
                init_rust
                ;;
            all)
                all
                ;;
            clean)
                clean_backup
                ;;
            *)
                echo "Program name is not in list:" >&2
                echo "[bash, zsh, spacemacs, fish, tmux]" >&2
                echo "[vim, pip, readline, bin, git, termux]" >&2
                echo "[atom, vscode, all, clean]" >&2
                exit 1
                ;;
        esac
    done
}


function check_distro() {
    echo "[checking distro]"
    platform=$(uname)
    declare -a pkg_mgrs=("apk" \
                             "apt-get" \
                             "apt" \
                             "yum" \
                             "dnf" \
                             "pkg" \
                             "zypper" \
                             "pacman")

    if [[ $platform == Linux ]]; then
        echo "Linux"
        for cmd in ${pkg_mgrs[@]}; do
            command -v $cmd && pkg_mgr="$cmd"
        done
    elif [[ $platform == Darwin ]]; then
        echo "Mac OS or iOS"
        command -v brew && pkg_mgr="brew"
        CONFIG_DIR="Library/Application Support"
    elif [[ $platform =~ BSD ]]; then
        echo "BSD"
        command -v pkg && pkg_mgr="pkg"
    fi
}

function install_package() {
    package="$1"

    if [[ -n $pkg_mgr ]]; then
        case $pkg_mgr in
            pacman)
                install="-Syu"
                ;;
            apk)
                install="add"
                ;;
            *)
                install="install"
                ;;
        esac


        echo "============"
        echo "Installing $package"
        # installing nedd previlages
        # unless it was brew
        if [[ $(id -u) -eq 0 ]] || [[ $pkg_mgr == brew ]]; then
            $pkg_mgr $install $package
        else
            command -v sudo \
                && sudo $pkg_mgr $install $package
        fi
    else
        echo "No package manager recognized." >&2
    fi
}

function install_packages() {
    for package in "$@"; do
        if hash $package &>/dev/null; then
            # FIXME Some packages use --version
            $package -V
        else
            install_package $package
        fi
    done
}

function abs() {
    # $1 : relative filename
    echo "${SCRIPTPATH}/$1"
}

function action() {
    # $1 : relative to this file
    # $2 : relative to target dir

    source="$(abs "$1")"
    dest="$TARGET_DIR/$2"

    check_dest "$dest"

    if [[ $COPY == true ]]; then
        cp -R "$source" "$dest" &&
        echo ">> Copying $source -> $dest"
    else
        ln -s "$source" "$dest" &&
        echo ">> Linking $source -> $dest"
    fi
}

function check_dest() {
    # $1 : filename path
    if [[ -e "$1" ]]; then
        if [[ $BACKUP == true ]]; then
            mv -n "$1"{,~}
        elif [[ -L "$1" ]]; then
            rm "$1"
        fi
    elif [[ -L "$1" ]]; then
        # broken link file
        rm "$1"
    fi

    if [[ "$1" == */ ]]; then
        mkdir -p "$1"
    fi
}

function clean_backup() {
    find $TARGET_DIR -type l -name "*~" -delete
}

function inject_shell_script() {
    # Similar to action but insert "source filepath"

    source="$(abs "$1")"
    dest="$TARGET_DIR/$2"

    echo "source \"$source\"" | tee -a "$dest"
}

#################### ####
#### INIT FUNCTIONS ####

function init_bash {
    inject_shell_script bash/bashrc .bashrc
    inject_shell_script bash/bash_profile .bash_profile
    curl -fLo "${SCRIPTPATH}/bash/git-prompt.sh" \
         https://github.com/git/git/raw/master/contrib/completion/git-prompt.sh
}

function init_zsh {
    action zsh/zshrc .zshrc
}

function init_spacemacs {
    install_packages git emacs
    action emacs/spacemacs.d .spacemacs.d
    git clone https://github.com/syl20bnr/spacemacs $TARGET_DIR/.emacs.d
    python3 -m pip install --user flake8 autoflake isort anaconda yapf
    sudo npm install -g tern vmd js-beautify eslint tide typescript tslint
}

function init_fish {
    install_packages fish
    action fish/config.fish $CONFIG_DIR/fish/config.fish

    for file in fish/conf.d/*.fish; do
        action $file $CONFIG_DIR/fish/conf.d/
    done

    for file in fish/functions/*.fish; do
        action $file $CONFIG_DIR/fish/functions/
    done

    which fish | sudo tee -a /etc/shells
}

function init_tmux {
    install_packages tmux
    for file in tmux/*.conf; do
        action $file .tmux/
    done
    action tmux/main.conf .tmux.conf
}

function init_vim {
    install_packages git curl vim
    for file in vim/*.vim; do
        action $file .vim/
    done
    action vim/vimrc .vimrc
    curl -fLo "$TARGET_DIR/.vim/autoload/plug.vim" --create-dirs \
         https://github.com/junegunn/vim-plug/raw/master/plug.vim

    curl -fLo "$TARGET_DIR/.vim/colors/molokai.vim" --create-dirs \
         https://github.com/tomasr/molokai/raw/master/colors/molokai.vim

    echo "============"
    echo "Starting vim"
    HOME=$TARGET_DIR vim -u "$TARGET_DIR/.vimrc" -X -c PlugInstall -c quit -c quit!
    echo "Done"
    echo "============"
}

function init_pip {
    action pip/pip.conf $CONFIG_DIR/pip/
}

function init_readline {
    action readline/inputrc .inputrc
}

function init_bin {
    for file in bin/*; do
        action $file .local/bin/
    done
    echo 'export PATH="$HOME/.local/bin/:$PATH"' >> $TARGET_DIR/.profile
}

function init_git {
    install_packages git
    action git/config .gitconfig
    action git/gitignore .gitignore_global
}

function init_termux {
    action termux .termux
}

function init_atom {
    action Atom/config.cson $CONFIG_DIR/Atom/
    action Atom/keymap.cson $CONFIG_DIR/Atom/
}

function init_vscode {
    action Code/User/settings.json $CONFIG_DIR/Code/User/
}

function init_rust {
    install_packages rust
    curl https://sh.rustup.rs -sSf | sh
}

function all {
    init_bash
    init_zsh
    init_tmux
    init_vim
    init_pip
    init_readline
    init_bin
    init_git
}



if [[ $# -lt 1 ]]; then
    echo "Argument required!"
    show_help
    echo "Select One"
    echo "========="
    selector
else
    main $@
fi
