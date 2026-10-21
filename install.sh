#!/bin/sh
# Run by the devcontainer CLI in each new dev container (--dotfiles-repository).
set -e
cd "$(dirname "$0")"
ln -sf "$PWD/zshrc" ~/.zshrc
ln -sf "$PWD/zshenv" ~/.zshenv
sudo chsh -s /usr/bin/zsh "$(id -un)"
# Only in projects whose image provides a venv, like Harmonist's /opt/venv.
if command -v uv >/dev/null && [ -n "${VIRTUAL_ENV:-}" ]; then
  uv pip install basedpyright
fi
