# ~/nixos-config/graphics.nix
{ config, pkgs, ... }:

{
  # Custom package installations
  environment.systemPackages = with pkgs; [
    gimp
    darktable
    obs-studio
    mpv
    yt-dlp
  ];
}
