# ~/nixos-config/wm.nix
{ config, pkgs, ... }:

{
  programs.hyprland = {
    enable = true;
    xwayland.enable = true;
  };

  environment.systemPackages = with pkgs; [
    hyprlock
    hypridle
    hyprnotify
    hyprutils
    hyprcursor
    swww
    waybar
    swaybg
    swaynotificationcenter
    swayimg
    wmenu
    wofi
    grim
    slurp
    brightnessctl
    nordzy-icon-theme
    nordzy-cursor-theme
    wl-clipboard
  ];
}
