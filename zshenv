# -*- mode: sh; sh-shell: zsh; -*-

# Environment shared by interactive and non-interactive zsh invocations.

for d in ~/.nix-profile/etc/profile.d/hm-session-vars.sh \
         ~/.cargo/env ; do
    [[ -e $d ]] || continue
    source $d
done
