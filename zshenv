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
plugins=(git colored-man-pages command-not-found tmux)

path+=~/bin

# bin subdirs (if any) should also be added - useful for script repos
# - means accept files, / means accept dirs, N means enable NULL_GLOB
path+=~/bin/*(-/N)
path+=~/.local/bin(/N)
path+=~/.gem/ruby/*/bin(/N)

VIRTUALENVWRAPPER_PYTHON=python3
plugins+=virtualenvwrapper

# MacPorts lives here:
if [[ -d /opt/local/bin ]]; then
    path=( /opt/local/bin /opt/local/sbin $path )
    manpath=( /opt/local/share/man /usr/share/man /usr/X11/man )
    plugins+=(macports)
fi

# Use Emacs.app emacsclient in preference to built-in emacsclient if found:
for eapp in /Applications/Emacs.app /Applications/MacPorts/Emacs.app; do
    if [[ -d $eapp/Contents/MacOS/bin ]]; then
        path=( $eapp/Contents/MacOS/bin $path )
        export EMACS=$eapp/Contents/MacOS/Emacs
        break
    fi
done

# just save everything
HISTSIZE=100000

source $ZSH/oh-my-zsh.sh