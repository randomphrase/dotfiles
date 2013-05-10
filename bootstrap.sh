#!/bin/bash

shopt -s nocasematch nullglob    # using Bash

dotfiles=${0%%/*}
dotfiles_abs=$(cd $dotfiles && pwd -L)

echo "** checking environment"

required_exes=(git bzr make)

for e in ${required_exe[@]}; do
    hash $e || {
        echo "!! Missing: $e"
        exit 1
    }
done

if (( BASH_VERSINFO[0] < 4 )); then
    echo "!! Bash version < 4: $BASH_VERSION"
    exit 1
fi

echo "** setting up git config"

# Don't use work email name/address for this repo...
git config --file "$dotfiles/.git/config" user.name "Alastair Rankine"
git config --file "$dotfiles/.git/config" user.email "alastair@girtby.net"


echo "** setting up symlinks to dotfiles in: $dotfiles_abs"

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

checkout_libs() {
    echo "** Checking out bzr libs"

    # FIXME: can't do this in bash 3 :(
    declare -A bzr_repos
    bzr_repos[".emacs.d/extern/cedet"]="bzr+ssh://alastair@cedet.bzr.sourceforge.net/bzrroot/cedet/code/trunk/"
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
}
checkout_libs

build_libs() {
    echo -n "** Building libs:"

    # Use ginstall-info if available
    hash ginstall-info 2>/dev/null && install_info_arg="INSTALL-INFO=ginstall-info"

    # Use latest emacs if available
    for e in /Applications/MacPorts/Emacs.app/Contents/MacOS/Emacs ; do
        [[ -x $e ]] || continue
        emacs_arg="EMACS=$e"
    done

    build=(
        ".emacs.d/extern/cedet"
        ".emacs.d/extern/cedet/contrib"
    )
    for i in ${build[@]}; do
        (
            echo -n " $i"
	        cd "$HOME/$i"

            # build to a tmp file, display it only if err
            buildlog=$(mktemp -t) || exit 1
            trap 'rm "$buildlog"' EXIT INT QUIT TERM

	        make $emacs_arg $install_info_arg >$buildlog 2>$buildlog || {
                echo "ERROR:"
                [[ -f $buildlog ]] && cat $buildlog
            }
        ) || exit 1
    done
    echo " ... done"
}
build_libs
