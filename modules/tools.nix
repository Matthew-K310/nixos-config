# ~/nixos-config/modules/tools.nix
{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
  ueberzug
    libnotify
    file
    dnsutils
    dig
    resvg
    unar
    poppler
    poppler-utils
    vorbis-tools
    ffmpegthumbnailer
	  ddcutil
    brightnessctl
  ];
}
