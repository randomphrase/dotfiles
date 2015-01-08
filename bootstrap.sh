#!/bin/bash

shopt -s nocasematch nullglob    # using Bash

dotfiles=${0%/*}
dotfiles_abs=$(cd $dotfiles && pwd -L)

# run command but only show output if an error occurrs
output_on_error() {
    log=$(mktemp ${0##*/}_log.XXXXXXXX) || exit 1
    trap 'rm "$log"' EXIT INT QUIT TERM

    $* >$log 2>$log || {
        echo "ERROR:"
        [[ -f $log ]] && cat $log
    }
}

check_environment() {
    echo "** checking environment"

    required_exes=(git make)
    
    for e in ${required_exe[@]}; do
        hash $e || {
            echo "!! Missing: $e"
            exit 1
        }
    done

    # Use MacPorts emacs if available
    for e in /Applications/MacPorts/Emacs.app/Contents/MacOS/Emacs ; do
        [[ -x $e ]] || continue
        export EMACS=$e
    done
}
check_environment

git_config() {
    echo "** setting up git config"

    # Don't use work email name/address for this repo...
    git config --file "$dotfiles/.git/config" user.name "Alastair Rankine"
    git config --file "$dotfiles/.git/config" user.email "alastair@girtby.net"
}
git_config

symlink_dotfiles() {
    echo "** setting up symlinks to dotfiles in: $dotfiles_abs"
    
    skipfiles=(bootstrap.sh readme.org)
    
    case "$(uname -s)" in
        "Linux"   )   statinode() { stat -L -c '%i' "$@"; return 0; };;
        "Darwin"  )   statinode() { stat -L -f '%i' "$@"; return 0; };;
        "FreeBSD" )   statinode() { stat -L -f '%i' "$@"; return 0; };;
        * )           statinode() { ls -id "$@" | cut -d ' ' -f 1; return 0; };;
    esac
    
    for dst in $dotfiles/* ; do
        nm=${dst##*/}
        
        # TODO: is this a reliable test?
        if [[ ${skipfiles[*]} =~ $nm ]]; then
            continue
        fi
        
        src=$HOME/.$nm
        
        if [[ -e $src ]]; then
            if [[ -L $src ]]; then
                linkinode=$(statinode $src)
                dstinode=$(statinode $dst)
                if (( linkinode != dstinode )); then
                    echo "warning: $src is a symlink but doesn't point to desired $dst ($linkinode != $dstinode)"
                fi
            else
                echo "warning: $src exists, cannot create symlink"
            fi
            
            continue
        fi
        
        echo "$src -> $dst"
        (
            cd $HOME
            ln -s ${dotfiles_abs#$HOME/}/$nm .$nm
        )
    done
}
symlink_dotfiles

clone_git_repo() {
    path=$1
    repo=$2

    # Need this because some submodules (like oh-my-zsh) are corrupted on github :(
    opts="-c transfer.fsckobjects=false"

    if [[ ! -d "$HOME/$path" ]]; then
        echo -n "** Clone git repo: $path"
        (
            dir="$HOME/${path%/*}"
            mkdir -p $dir
            cd $dir
            git ${opts} clone -q ${repo} ${path##*/}
        )
        echo " ... done"
    fi
}
# Some tools are self-updating, so we don't import them as submodules, instead just clone
clone_git_repo ".zsh.d/oh-my-zsh" "https://github.com/robbyrussell/oh-my-zsh.git"
clone_git_repo ".emacs.d/extern/cask" "https://github.com/cask/cask.git"

build_lib() {
    echo -n "** Building: $1"

    # Use ginstall-info if available
    hash ginstall-info 2>/dev/null && install_info_arg="INSTALL-INFO=ginstall-info"

    # Use my Emacs
    [[ $EMACS ]] && emacs_arg="EMACS=$EMACS"

    (
        cd "$HOME/$1"
        output_on_error make $emacs_arg $install_info_arg
    ) || exit 1
    echo " ... done"
}
build_lib ".emacs.d/extern/cedet"
build_lib ".emacs.d/extern/cedet/contrib"

run_cask() {
    echo -n "** Updating cask"
    (
        cd "$HOME/.emacs.d"
        output_on_error $HOME/.emacs.d/extern/cask/bin/cask upgrade
    ) || exit 1
    echo " ... done"

    echo -n "** Updating cask packages"
    (
        cd "$HOME/.emacs.d"
        output_on_error $HOME/.emacs.d/extern/cask/bin/cask update
    ) || exit 1
    echo " ... done"
}
run_cask
