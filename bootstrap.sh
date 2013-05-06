#!/bin/bash -x

shopt -s nocasematch nullglob    # using Bash

dotfiles=${0%%/*}
dotfiles_abs=$(cd $dotfiles && pwd -L)

echo "setting up symlinks to dotfiles in: $dotfiles_abs"

skipfiles=(bootstrap.sh)

case "$(uname -s)" in
    "Linux"   )   statinode() { stat -c '%i' "$@"; return 0; };;
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
            if [[ $linkinode != $dstinode ]]; then
                echo "warning: $src is a symlink but doesn't point to desired $dst"
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

declare -A bzr_repos
bzr_repos[".emacs.d/extern/cedet"]="bzr://cedet.bzr.sourceforge.net/bzrroot/cedet/code/trunk/"
bzr_repos[".emacs.d/extern/ede-cmake"]="lp:~arankine/+junk/ede-cmake"

for i in ${!bzr_repos[@]}; do
    if [[ ! -d "$HOME/$i" ]]; then
	(
	    cd "$HOME/${i%/*}"
	    bzr checkout --lightweight ${bzr_repos[$i]} ${i##*/}
	)
    else
	(
	    cd "$HOME/$i"
	    bzr update
	)
    fi
done

build=(
    ".emacs.d/extern/cedet"
    ".emacs.d/extern/cedet/contrib"
    )
for i in ${build[@]}; do
    (
	cd "$HOME/$i"
	# TODO: how to select which install-info to use?
	make INSTALL-INFO=ginstall-info
    )
done
