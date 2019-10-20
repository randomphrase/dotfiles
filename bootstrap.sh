#!/bin/bash -eu

shopt -s nocasematch nullglob    # using Bash

dotfiles=${0%/*}
dotfiles_abs=$(cd $dotfiles && pwd -L)

# portable statinode function
case "$(uname -s)" in
    "Linux"   )   statinode() { stat -L -c '%i' "$@"; return 0; };;
    "Darwin"  )   statinode() { stat -L -f '%i' "$@"; return 0; };;
    "FreeBSD" )   statinode() { stat -L -f '%i' "$@"; return 0; };;
    * )           statinode() { ls -id "$@" | cut -d ' ' -f 1; return 0; };;
esac

# checks for available commands
require() {
    for e in $*; do
        hash $e 2>/dev/null || {
            echo "skipped (missing: $e)"
            exit 1
        }
    done
}

start_task() {
    echo -n "** $1 ... "
    shift
    for e in $*; do
        require "$e" || exit 1
    done
}

end_task() {
    echo ${1:-"done"}
}

# run command but only show output if an error occurrs
output_on_error() {
    local log=$(mktemp ${0##*/}_log.XXXXXXXX) || exit 1
    trap 'rm "$log"' RETURN

    $* >$log 2>$log || {
        echo "ERROR:"
        [[ -f $log ]] && cat $log
    }
    # TODO return result or exit on error?
}

# Make a relative path from src -> dst. Stolen from http://stackoverflow.com/a/12498485/31038
make_relative_path() {
    # both $1 and $2 are absolute paths beginning with /
    # returns relative path to $2/$target from $1/$source
    local source=$1
    local target=$2

    local common_part=$source # for now
    local result="" # for now

    while [[ "${target#$common_part}" == "${target}" ]]; do
        # no match, means that candidate common part is not correct
        # go up one level (reduce common part)
        common_part="$(dirname $common_part)"
        # and record that we went back, with correct / handling
        if [[ -z $result ]]; then
            result=".."
        else
            result="../$result"
        fi
    done

    if [[ $common_part == "/" ]]; then
        # special case for root (no common path)
        result="$result/"
    fi

    # since we now have identified the common part,
    # compute the non-common part
    local forward_part="${target#$common_part}"

    # and now stick all parts together
    if [[ -n $result ]] && [[ -n $forward_part ]]; then
        result="$result$forward_part"
    elif [[ -n $forward_part ]]; then
        # extra slash removal
        result="${forward_part:1}"
    fi

    echo $result
}

# Make a symbolic link $1 -> $2
make_symlink() {
    local src=$1
    local dst=$2

    if [[ -e $src ]]; then
        if [[ -L $src ]]; then
            local linkinode=$(statinode $src)
            local dstinode=$(statinode $dst)
            if (( linkinode != dstinode )); then
                echo "warning: $src is a symlink but doesn't point to desired $dst ($linkinode != $dstinode)"
            fi
        else
            echo "warning: $src exists, cannot create symlink"
        fi

        return
    fi

    local srcdir=${src%/*}
    local srcnm=${src##*/}
    local dstpath=$(make_relative_path $srcdir $dst)
    echo "   ${srcdir}/${srcnm} -> ${dstpath}"
    (
        cd ${srcdir}
        ln -s ${dstpath} ${srcnm}
    )
}

check_environment() {
    start_task "checking environment" || exit 1

    # Use MacPorts emacs if available
    for e in /Applications/MacPorts/Emacs.app/Contents/MacOS/Emacs ; do
        [[ -x $e ]] || continue
        export EMACS=$e
    done

    end_task
}

git_config() {
    start_task "setting up git config" "git" || exit 1

    # Don't use work email name/address for this repo...
    git config --file "$dotfiles/.git/config" user.name "Alastair Rankine"
    git config --file "$dotfiles/.git/config" user.email "alastair@girtby.net"

    end_task
}

symlink_dotfiles() {
    start_task "setting up symlinks: ~/.* -> dotfiles/*" || exit 1

    local skipfiles=(bootstrap.sh readme.org bin)

    for dst in $dotfiles_abs/* ; do
        local nm=${dst##*/}

        # Todo: is this a reliable test?
        if [[ ${skipfiles[*]} =~ $nm ]]; then
            continue
        fi

        make_symlink $HOME/.$nm $dst
    done

    end_task
}

symlink_bindirs() {
    start_task "setting up symlinks: ~/bin/* -> dotfiles/bin/*" || exit 1

    [[ -d $HOME/bin ]] || mkdir $HOME/bin

    for dst in $dotfiles_abs/bin/* ; do
        make_symlink $HOME/bin/${dst##*/} $dst
    done

    end_task
}

clone_git_repo() {
    # Need to disable fsckobjects because some submodules (like oh-my-zsh) are corrupted on github :(
    local opts="-c transfer.fsckobjects=false"
    local copts="-q"

    local path=$1
    shift
    local repo=$1
    shift
    copts="${copts} $*"

    start_task "clone git repo: $path" "git" || exit 1

    if [[ ! -d "$HOME/$path" ]]; then
        (
            dir="$HOME/${path%/*}"
            mkdir -p $dir
            cd $dir
            git ${opts} clone ${copts} ${repo} ${path##*/}
        )
        end_task
    else
        end_task "skipped"
    fi
}

rebuild_font_cache() {
    start_task "rebuilding font cache: $1" "fc-cache" || exit 1

    fc-cache -f $HOME/$1

    end_task
}

# main

check_environment
git_config
symlink_dotfiles
symlink_bindirs

# some tools are self-updating, so we don't import them as submodules, instead just clone
clone_git_repo ".zsh.d/oh-my-zsh" "https://github.com/robbyrussell/oh-my-zsh.git"

# This one can't be added as a submodule, see http://stackoverflow.com/q/34456530/31038
clone_git_repo ".fonts/source-code-pro" "https://github.com/adobe-fonts/source-code-pro.git" "--depth 1" "-b release"

clone_git_repo ".tmux/plugins/tpm" "https://github.com/tmux-plugins/tpm"

rebuild_font_cache ".fonts/source-code-pro/OTF"
