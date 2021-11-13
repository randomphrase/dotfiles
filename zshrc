# -*- mode: shell-script -*-

# Path to your oh-my-zsh configuration.
ZSH=$HOME/.zsh.d/oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="jreese"

# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Comment this out to disable bi-weekly auto-update checks
# DISABLE_AUTO_UPDATE="true"

# Uncomment to change how often before auto-updates occur? (in days)
# export UPDATE_ZSH_DAYS=13

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want to disable command autocorrection
# DISABLE_CORRECTION="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
# COMPLETION_WAITING_DOTS="true"

# Uncomment following line if you want to disable marking untracked files under
# VCS as dirty. This makes repository status check for large repositories much,
# much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"


# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(
    colored-man-pages
    command-not-found
    git
    pipenv
    pyenv
    tmux
    vterm
)

path+=~/bin
path+=~/.local/bin

# bin subdirs (if any) should also be added - useful for script repos
# / means accept dirs, N means enable NULL_GLOB
path+=(~/{,.local/}bin/*(/N))

# local ruby gems are installed here
path+=(~/.gem/ruby/*/bin(/N))

# rust lives here
path+=~/.cargo/bin

# virtualenvwrapper support
# plugins+=virtualenvwrapper
# export VIRTUALENVWRAPPER_PYTHON=/usr/bin/python3

# Setup emacs as the editor of choice
export EDITOR=emacsclient
export ALTERNATE_EDITOR=emacs
alias e='emacsclient'

# Use ccache if it's available
# if [[ -x $(which ccache) ]]; then
#     export CC="ccache ${CC-gcc}"
#     export CXX="ccache ${CXX-g++}"
# fi

# just save everything
HISTSIZE=100000
SAVEHIST=100000

source $ZSH/oh-my-zsh.sh

# vterm plugin will set PROMPT but it gets clobbered by the omz theme - so we need to put it back :(
if type vterm_prompt_end >/dev/null; then
    PROMPT=$PROMPT'%{$(vterm_prompt_end)%}'
fi
