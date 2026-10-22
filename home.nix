{ config, pkgs, lib, ... }:
let
  withGui = config._module.args.withGui or false;
  # Disk-constrained hosts: skip the heavyweight toolchains. quarto drags in a
  # full R + jupyter + texlive (~2.8GB) and clang-tools is ~1.5GB of LLVM.
  slim = config._module.args.slim or false;
  isLinux = pkgs.stdenv.hostPlatform.isLinux;
  isDarwin = pkgs.stdenv.hostPlatform.isDarwin;
  unsupported = builtins.abort "Unsupported platform";

  # Determine the path based on the system type
  pinentryPath = if isDarwin
    then "${pkgs.pinentry_mac}/Applications/pinentry-mac.app/Contents/MacOS/pinentry-mac"
    else "${pkgs.pinentry-emacs}/bin/pinentry-emacs";

  # TEMPORARY: tmux 3.7c's configure aborts on darwin unless jemalloc is
  # explicitly chosen (upstream's workaround for a macOS calloc(3) bug that
  # fails to zero allocations). Mirrors nixpkgs PR #555604, merged to master
  # 2026-08-23 but not yet in the nixpkgs-unstable channel. Once a flake update
  # picks up the fix, drop this and go back to plain `tmux` in home.packages.
  tmuxPkg = if isDarwin
    then pkgs.tmux.overrideAttrs (old: {
      buildInputs = old.buildInputs ++ [ pkgs.jemalloc ];
      configureFlags = old.configureFlags ++ [ "--enable-jemalloc" ];
    })
    else pkgs.tmux;

  # Lazy: the fetchGit (and its ~3GB clone in ~/.cache/nix/gitv3) only happens
  # if something below actually references pinnedNixpkgs, i.e. non-slim hosts.
  pinnedNixpkgs = import (builtins.fetchGit {
    url = "https://github.com/NixOS/nixpkgs.git";
    # 1. Provide the explicit branch name so GitHub allows the download handshake
    ref = "refs/heads/nixos-24.11";
    # 2. Provide the exact pinned commit hash that bypasses the Quarto option bug
    rev = "50ab793786d9de88ee30ec4e4c24fb4236fc2674";
  }) { system = pkgs.stdenv.hostPlatform.system; };
in {
  imports = [
    ## Modularize your home.nix by moving statements into other files
  ];

  nixpkgs.config.allowUnfree = true;

  # Home Manager needs a bit of information about you and the paths it should
  # manage.

  # Default to alastair for berry and other standard hosts
  home.username = lib.mkDefault "alastair";

  # Handle path differences between Linux and macOS
  home.homeDirectory = if pkgs.stdenv.hostPlatform.isLinux
    then "/home/${config.home.username}"
    else "/Users/${config.home.username}";

  # This value determines the Home Manager release that your configuration is
  # compatible with. This helps avoid breakage when a new Home Manager release
  # introduces backwards incompatible changes.
  #
  # You should not change this value, even if you update Home Manager. If you do
  # want to update the value, then make sure to first check the Home Manager
  # release notes.
  home.stateVersion = "24.11"; # Please read the comment before changing.

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # The home.packages option allows you to install Nix packages into your
  # environment.
  home.packages = with pkgs; ([
    # # Adds the 'hello' command to your environment. It prints a friendly
    # # "Hello, world!" when run.
    # pkgs.hello

    # # It is sometimes useful to fine-tune packages, for example, by applying
    # # overrides. You can do that directly here, just don't forget the
    # # parentheses. Maybe you want to install Nerd Fonts with a limited number of
    # # fonts?
    # (pkgs.nerdfonts.override { fonts = [ "FantasqueSansMono" ]; })

    # # You can also create simple shell scripts directly inside your
    # # configuration. For example, this adds a command 'my-hello' to your
    # # environment:
    # (pkgs.writeShellScriptBin "my-hello" ''
    #   echo "Hello, ${config.home.username}!"
    # '')

    oh-my-zsh
    fzf
    direnv
    bat
    ripgrep
    dust
    tmuxPkg
    nodejs
    gh
    uv
    shellcheck
    gnupg
    pandoc
    go-grip
    editorconfig-checker
    devcontainer

    python3 # use a nix-installed python for compatibility

    claude-code
    codex
    antigravity-cli

  ] ++ lib.optionals (!slim) [
    pinnedNixpkgs.quarto

  ] ++ lib.optionals isLinux [
    # Add Linux-specific packages here
    (if withGui then emacs31 else emacs31-nox)
    pinentry-emacs

  ] ++ lib.optionals (isLinux && !slim) [
    clang-tools # for clangd

  ] ++ lib.optionals isDarwin [
    # Add macOS-specific packages here
    emacs31
    pinentry_mac
    git-credential-manager
    coreutils-prefixed # for dired in emacs

  ] ++ lib.optionals (withGui || isDarwin) [
    # Fonts for the Emacs GUI. CaskaydiaCove is Cascadia Code patched with the
    # Nerd Font glyphs; symbols-only provides "Symbols Nerd Font Mono", the
    # family nerd-icons (and through it doom-modeline) asks for by default.
    # Both family names are what ~/.emacs.d/lisp/mod-ui.el looks for.
    nerd-fonts.caskaydia-cove
    nerd-fonts.symbols-only

  ]);

  # Generations accumulate silently and each one pins its whole closure; on
  # us01 twelve of them had grown /nix/store to 22GB against a 50GB quota.
  nix.gc = {
    automatic = true;
    dates = "weekly";
    options = "--delete-older-than 30d";
  };

  # Make GUI apps installed via Nix (e.g. emacs.desktop) show up in the
  # desktop environment's app launcher on non-NixOS Linux distros.
  targets.genericLinux.enable = isLinux;

  # Nix-installed fonts are invisible to fontconfig -- and so to Emacs, GTK and
  # everything else -- on non-NixOS until this writes the matching config into
  # ~/.config/fontconfig and refreshes the cache on activation.
  fonts.fontconfig.enable = true;

  # Home Manager is pretty good at managing dotfiles. The primary way to manage
  # plain files is through 'home.file'.
  home.file = {
    # # Building this configuration will create a copy of 'dotfiles/screenrc' in
    # # the Nix store. Activating the configuration will then make '~/.screenrc' a
    # # symlink to the Nix store copy.
    # ".screenrc".source = dotfiles/screenrc;
    ".zshrc".source = ./zshrc;
    ".zshenv".source = ./zshenv;
    ".tmux.conf".source = ./tmux.conf;
    ".gdbinit".source = ./gdbinit;

    # Symlink the Oh My Zsh framework to ~/.oh-my-zsh
    ".oh-my-zsh".source = "${pkgs.oh-my-zsh}/share/oh-my-zsh";

    # Native zsh completions from zsh-users/zsh-completions (kept current via
    # flake inputs). Provides _cmake and ~180 others; added to fpath in zshrc.
    ".zsh/completions".source =
      "${pkgs.zsh-completions}/share/zsh/site-functions";

    # Fetch TPM directly from GitHub and symlink it to the expected directory
    ".tmux/plugins/tpm".source = pkgs.fetchFromGitHub {
      owner = "tmux-plugins";
      repo = "tpm";
      rev = "master";
      hash = "sha256-oRKUZNyJYQXlkeQfbEYiltUEBpvdwn2SoEBWHVUNmrA=";
    };

    # # You can also set the file content immediately.
    # ".gradle/gradle.properties".text = ''
    #   org.gradle.console=verbose
    #   org.gradle.daemon.idletimeout=3600000
    # '';
  };

  # Copy .app bundles into ~/Applications/Home Manager Apps as real
  # directories instead of store symlinks (the default from stateVersion
  # 25.11). Gives a stable path the Dock and Spotlight can use directly,
  # avoiding stale Dock pins and duplicate Dock icons.
  targets.darwin.copyApps.enable = isDarwin;
  targets.darwin.linkApps.enable = false;

  # Home Manager can also manage your environment variables through
  # 'home.sessionVariables'. These will be explicitly sourced when using a
  # shell provided by Home Manager. If you don't want to manage your shell
  # through Home Manager then you have to manually source 'hm-session-vars.sh'
  # located at either
  #
  #  ~/.nix-profile/etc/profile.d/hm-session-vars.sh
  #
  # or
  #
  #  ~/.local/state/nix/profiles/profile/etc/profile.d/hm-session-vars.sh
  #
  # or
  #
  #  /etc/profiles/per-user/alastair/etc/profile.d/hm-session-vars.sh
  #
  home.sessionVariables = {
    # EDITOR = "emacs";
  };

  programs.git = {
    enable = true;
    settings = {
      user = {
        name = "Alastair Rankine";
        email = "alastair@girtby.net";
        signingKey = "DD583CE0F90A58F0A445EBDA35095B01B376ADF1";
      };
      github = {
	user = "randomphrase";
      };
      pull = {
	rebase = true;
      };
      credential = {
	helper = if isLinux then "!gh auth git-credential" else "manager";
      };
      commit = {
        gpgSign = true;
      };
    };
    signing = {
      format = null;
    };
  };
  programs.delta = {
    enable = true;
    enableGitIntegration = true;
  };

  services.gpg-agent = {
    enable = true;

    pinentry = {
      package = if isDarwin then pkgs.pinentry_mac else pkgs.pinentry-emacs;
    };

    extraConfig = ''
      # Explicitly point to the binary in the Nix store
      pinentry-program ${pinentryPath}

      # Allow Emacs to grab the focus if you DO happen to be in Magit
      allow-emacs-pinentry
      allow-loopback-pinentry
    '';

    # Optional: keep the agent alive longer so you don't re-type often
    defaultCacheTtl = 34560000;
    maxCacheTtl = 34560000;
  };
}
