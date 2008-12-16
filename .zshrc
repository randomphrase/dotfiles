#
# Alastair's .zshrc file
#
# .zshrc is sourced in interactive shells.  It
# should contain commands to set up aliases, functions,
# options, key bindings, etc.
#

# set up path to include my stuff (possibly move this elsewhere?)
[[ -d ~/bin ]] && path=( ~/bin $path )

# MacPorts lives here:
[[ -d /opt/local/bin ]] && path=( /opt/local/bin $path ) \
    && manpath=( /opt/local/share/man /usr/share/man /usr/X11/man )

# Search path for the cd command
cdpath=(.. ~)

# Use hard limits, except for a smaller stack and no core dumps
#unlimit
#limit stack 8192
#limit core 0
#limit -s

umask 022

# Set up aliases
alias mv='nocorrect mv'       # no spelling correction on mv
alias cp='nocorrect cp'       # no spelling correction on cp
alias mkdir='nocorrect mkdir' # no spelling correction on mkdir
alias j=jobs
alias pu=pushd
alias po=popd
alias d='dirs -v'
alias h=history
alias grep=egrep
alias ll='ls -l'
alias la='ls -a'

# List only directories and symbolic
# links that point to directories
alias lsd='ls -ld *(-/DN)'

# List only file beginning with "."
alias lsa='ls -ld .*'

# Lists one argument per line:
alias showargs="print -l"

# Edit environment variables with one line per item, w00t!
alias lvared="IFS=\$'\n' vared"

# Shell functions
#setenv() { typeset -x "${1}${1:+=}${(@)argv[2,$#]}" }  # csh compatibility
#freload() { while (( $# )); do; unfunction $1; autoload -U $1; shift; done }

if [[ -d ~/.zfunc ]]; then
    # Where to look for autoloaded function definitions
    fpath+=~/.zfunc

    # Autoload shell functions in .zfunc. Ignores files beginning with an
    # underscore, because compinit will autoload them. This glob also
    # strips the directory part, because that's all we need.
    autoload -- ~/.zfunc/[^_]*(:t)
fi

# automatically remove duplicates from these arrays
declare -U path cdpath fpath manpath

# Global aliases -- These do not have to be
# at the beginning of the command line.
alias -g M='|more'
alias -g H='|head'
alias -g T='|tail'

# Mac-specific stuff:
alias plcat="plutil -convert xml1 -o - "

#manpath=($X11HOME/man /usr/man /usr/lang/man /usr/local/man)
#export MANPATH

# Hosts to use for completion (see later zstyle)
#hosts=(`hostname` ftp.math.gatech.edu prep.ai.mit.edu wuarchive.wustl.edu)

# Set up prompt using a theme (I like oliver's theme)
autoload -U promptinit
promptinit
prompt oliver bold normal

# Set the window title bar
header() {
    [[ -t 1 ]] || return
    case $TERM in
        *xterm*|rxvt|(dt|k|E)term|cygwin) print -Pn "\e]2;$*\a"
        ;;
        screen) print -Pn "\e_$*\e\\"
        ;;
    esac
}

# Update the title bar when the wd changes
chpwd() {
    [[ -t 1 ]] && header "%n@%m : %~"
}

# Some environment variables
#export MAIL=/var/spool/mail/$USERNAME
#export LESS=-cex3M
#export HELPDIR=/usr/local/lib/zsh/help  # directory for run-help function to find docs

#MAILCHECK=300
HISTSIZE=1000
SAVEHIST=1000
#DIRSTACKSIZE=20

# Save history here
HISTFILE=~/.history

# Setup emacs as the editor of choice
EDITOR=emacsclient
alias em='emacsclient -n'

# Watch for my friends
#watch=( $(<~/.friends) )       # watch for people in .friends file
#watch=(notme)                   # watch for everybody but me
#LOGCHECK=300                    # check every 5 min for login/logout activity
#WATCHFMT='%n %a %l from %m at %t.'

# Set/unset  shell options
setopt auto_cd                  # type directory name, cd's to that directory
setopt extended_glob            # more filename globbing options
setopt list_ambiguous
setopt noclobber                # output redirections don't overwrite existing files
setopt hist_allow_clobber       # ... but override if we're executing from history
setopt no_flow_control
#setopt   notify globdots correct pushdtohome cdablevars autolist
#setopt   correctall autocd recexact longlistjobs
#setopt   autoresume histignoredups pushdsilent noclobber
#setopt   autopushd pushdminus extendedglob rcquotes mailwarning
#unsetopt bgnice autoparamslash

# Autoload zsh modules when they are referenced
#zmodload -a zsh/stat stat
#zmodload -a zsh/zpty zpty
#zmodload -a zsh/zprof zprof
#zmodload -ap zsh/mapfile mapfile

# Some nice key bindings
#bindkey '^X^Z' universal-argument ' ' magic-space
#bindkey '^X^A' vi-find-prev-char-skip
#bindkey '^Xa' _expand_alias
#bindkey '^Z' accept-and-hold
#bindkey -s '\M-/' \\\\
#bindkey -s '\M-=' \|

bindkey -e                 # emacs key bindings
bindkey ' ' magic-space    # also do history expansion on space
bindkey '^I' complete-word # complete on tab, leave expansion to _expand

# Favourite key bindings, used in MacOS Terminal.app and other places probably.
bindkey '\e[3~' delete-char        # delete
bindkey '\e[H' beginning-of-line   # home
bindkey '\e[F' end-of-line         # end
bindkey '\e[5D' backward-word      # ctrl-left
bindkey '\e[5C' forward-word       # ctrl-right

# Setup new style completion system. To see examples of the old style (compctl
# based) programmable completion, check Misc/compctl-examples in the zsh
# distribution.
autoload -U compinit
compinit

# Completion Styles

# list of completers to use
zstyle ':completion:*::::' completer _expand _complete _ignored _approximate

# allow one error for every three characters typed in approximate completer
zstyle -e ':completion:*:approximate:*' max-errors \
    'reply=( $(( ($#PREFIX+$#SUFFIX)/3 )) numeric )'
    
# insert all expansions for expand completer
zstyle ':completion:*:expand:*' tag-order all-expansions

# formatting and messages
zstyle ':completion:*' verbose yes
zstyle ':completion:*:descriptions' format '%B%d%b'
zstyle ':completion:*:messages' format '%d'
zstyle ':completion:*:warnings' format 'No matches for: %d'
zstyle ':completion:*:corrections' format '%B%d (errors: %e)%b'
zstyle ':completion:*' group-name ''

# match uppercase from lowercase
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'

# offer indexes before parameters in subscripts
zstyle ':completion:*:*:-subscript-:*' tag-order indexes parameters

# Ignore completions in the current directory if we typed ..
zstyle ':completion:*' ignore-parents parent pwd

# command for process lists, the local web server details and host completion
#zstyle ':completion:*:processes' command 'ps -o pid,s,nice,stime,args'
#zstyle ':completion:*:urls' local 'www' '/var/www/htdocs' 'public_html'
#zstyle '*' hosts $hosts

# Filename suffixes to ignore during completion (except after rm command)
zstyle ':completion:*:*:(^rm):*:*files' ignored-patterns '*?.o' '*?.c~' \
    '*?.old' '*?.pro'
# the same for old style completion
#fignore=(.o .c~ .old .pro)

# ignore completion functions (until the _ignored completer)
zstyle ':completion:*:functions' ignored-patterns '_*'

# Menu selection
zmodload zsh/complist

# Enables menu selection on this many matches (ie always)
zstyle ':completion:*:default' menu 'select=0'

# Use this key to accept the selection and start another
bindkey -M menuselect '\C-o' accept-and-menu-complete

# Local Variables:
# mode: shell-script
# End: