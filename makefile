# makefile
PREFIX 		:= $(HOME)/test

# GNU ln
GNULINK 	:= ln -s --backup=numbered
GNULINKDIR 	:= $(GNULINK) --no-target-directory

# BusyBox ln
BBLINK		:= ln -s

# BSD ln
BSDLINK		:= ln -s

LINK 		:= $(GNULINK)
LINKDIR 	:= $(GNULINKDIR)

default: all

# bash shell
bash:
	$(LINK) "$(realpath bash/bashrc)" "$(PREFIX)/.bashrc"

# zsh shell
zsh:
	$(LINK) "$(realpath zsh/zshrc)" "$(PREFIX)/.zshrc"

# spacemacs
spacemacs:
	$(LINK) "$(realpath emacs/spacemacs.d)" "$(PREFIX)/.spacemacs.d"

# fish shell
FISH_CONFD = $(wildcard fish/conf.d/*.fish)
FISH_FUNCTIONS = $(wildcard fish/functions/*.fish)

.PHONY: fish_confd $(FISH_CONFD)
fish_confd: $(FISH_CONFD)

$(FISH_CONFD):
	@mkdir -p "$(PREFIX)/.config/fish/conf.d"
	@echo $@
	@echo "$(realpath ${@})"
	$(LINK) "$(realpath ${@})" "$(PREFIX)/.config/fish/conf.d/"

.PHONY: fish_functions $(FISH_FUNCTIONS)
fish_functions: $(FISH_FUNCTIONS)

$(FISH_FUNCTIONS):
	@mkdir -p "$(PREFIX)/.config/fish/functions"
	@echo $@
	@echo "$(realpath ${@})"
	$(LINK) "$(realpath ${@})" "$(PREFIX)/.config/fish/functions/"

fish: fish_confd fish_functions
	$(LINK) "$(realpath fish/config.fish)" "$(PREFIX)/.config/fish/config.fish"

# tmux
tmux:
	$(LINKDIR) "$(realpath tmux)" "$(PREFIX)/.tmux"
	$(LINK) "$(realpath tmux/main.conf)" "$(PREFIX)/.tmux.conf"

# vim
vim:
	$(LINKDIR) "$(realpath vim)" "$(PREFIX)/.vim"
	$(LINK) "$(realpath vim/vimrc)" "$(PREFIX)/.vimrc"

# pip
pip:
	mkdir -p "$(PREFIX)/.config/pip"
	$(LINK) "$(realpath pip/pip.conf)" "$(PREFIX)/.config/pip/"

# readline
readline:
	$(LINK) "$(realpath readline/inputrc)" "$(PREFIX)/.inputrc"

# shell-scripts
BIN_SCRIPTS = $(wildcard bin/*)
.PHONY: bin_scripts $(BIN_SCRIPTS)
bin_scripts: $(BIN_SCRIPTS)

$(BIN_SCRIPTS):
	mkdir -p "$(PREFIX)/.local/bin"
	$(LINK) "$(realpath ${@})" "$(PREFIX)/.local/bin/"

bin: bin_scripts

# git
git:
	$(LINK) "$(realpath git/config)" "$(PREFIX)/.gitconfig"
	$(LINK) "$(realpath git/gitignore)" "$(PREFIX)/.gitignore_global"

# termux
termux:
	$(LINKDIR) "$(realpath termux)" "$(PREFIX)/.termux"

# atom
atom:
	$(LINK) "$(realpath Atom/config.cson)" "$(PREFIX)/.config/Atom/"
	$(LINK) "$(realpath Atom/keymap.cson)" "$(PREFIX)/.config/Atom/"

# vscode
vscode:
	$(LINK) "$(realpath Code/User/settings.json)" "$(PREFIX)/.config/Code/User/"

# clean
clean:
	@echo "Cleaning link backup at" "$(PREFIX)"
	-@find "$(PREFIX)" -type l -name "*~" -delete

directory:
	$(MAKE) --directory directory

.PHONY: clean directory all bash zsh fish tmux vim pip readline bin termux
all: bash zsh fish tmux vim pip readline bin git
