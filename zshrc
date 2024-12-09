# -*- mode: shell-script -*-

# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="fishy"

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

# my terminals are all good
ZSH_TMUX_FIXTERM="false"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(
    colored-man-pages
    command-not-found
    git
    tmux
    vterm
)

path+=~/bin
path+=~/.local/bin

# bin subdirs (if any) should also be added - useful for script repos
# / means accept dirs, N means enable NULL_GLOB
# use [^_] to exclude __pycache__ dir, sigh
path+=(~/{,.local/}bin/[^_]*(/N))

# macOS pip installs binaries here (n for numerical sorting, On for reversed low->hi)
path+=(~/Library/Python/*/bin(NnOn))

# local ruby gems are installed here
path+=(~/.local/share/gem/ruby/*/bin(/N))

# rust lives here
path+=(~/.cargo/bin(N))

# go lives here
path+=(~/go/bin(N))

# nix home-manager
for d in ~/.nix-profile/etc/profile.d/hm-session-vars.sh ; do
    [[ -e $d ]] || continue
    source $d
done

if (( $+commands[fzf] )); then
    plugins+=fzf
    FZF_TMUX_OPTS="-p -w 80% -h 80%"
fi

if (( $+commands[brew] )); then
    plugins+=brew
    FPATH="$(brew --prefix)/share/zsh/site-functions:${FPATH}"
fi

if (( $+commands[direnv] )); then
    plugins+=direnv
fi

if (( $+commands[docker] )); then
    plugins+=docker
fi

# add zsh-completions
fpath+=${ZSH_CUSTOM:-${ZSH:-~/.oh-my-zsh}/custom}/plugins/zsh-completions/src

# Setup emacs as the editor of choice
# TODO: use omz emacs plugin instead?
export EDITOR=emacsclient
export ALTERNATE_EDITOR=emacs
alias e='emacsclient'

# just save everything
HISTSIZE=100000
SAVEHIST=100000

source $ZSH/oh-my-zsh.sh

# vterm plugin will set PROMPT but it gets clobbered by the omz theme - so we need to put it back :(
if type vterm_prompt_end >/dev/null; then
    PROMPT=$PROMPT'%{$(vterm_prompt_end)%}'
fi
