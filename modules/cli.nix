# ~/nixos-config/modules/cli.nix
{ config, pkgs, ... }:

{
  # Custom package installations
  environment.systemPackages = with pkgs; [
    # tooling
    neovim
    forgejo-cli
    yazi
    stow
    zoxide
    fzf
    ffmpeg
    imagemagick
    btop
    fastfetch
    tmux
    # libraries
    gum
    _7zz
    unzip
    # dependencies
    bat
    tree
    jq
    fd
  ];
}

