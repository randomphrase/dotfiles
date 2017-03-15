# -*- mode: shell-script -*-

# Setup emacs as the editor of choice
export EDITOR=emacsclient
export ALTERNATE_EDITOR=emacs
alias e='emacsclient'

# Mac-specific stuff:
alias plcat="plutil -convert xml1 -o - "

# Use ccache if it's available
# if [[ -x $(which ccache) ]]; then
#     export CC="ccache ${CC-gcc}"
#     export CXX="ccache ${CXX-g++}"
# fi

# pbcopy / pbpaste emulation on linux
alias pbcopy="xsel --clipboard"
alias pbpaste="xsel --clipboard"

# Use sshrc, enable ssh completion
compdef sshrc=ssh
