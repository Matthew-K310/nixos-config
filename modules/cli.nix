# ~/nixos-config/cli.nix
{ config, pkgs, ... }:

{
  # Custom package installations
  environment.systemPackages = with pkgs; [
    # tooling
    neovim
    yazi
    stow
    zoxide
    fzf
    ffmpeg
    imagemagick
    btop
    fastfetch
    tmux
    profanity
    gurk-rs 
    # libraries
    gum
    libnotify
    file
    _7zz
    unzip
    # dependencies
    bat
    tree
    jq
    fd
    ueberzug
    resvg
    unar
    poppler
    poppler-utils
    ffmpegthumbnailer
  ];
}

