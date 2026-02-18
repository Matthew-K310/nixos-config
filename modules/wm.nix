# ~/nixos-config/wm.nix
{ config, pkgs, ... }:

{
  programs.hyprland = {
    enable = true;
    xwayland.enable = true;
  };

  xdg.portal = {
	enable = true;
	extraPortals = [ pkgs.xdg-desktop-portal-hyprland pkgs.xdg-desktop-portal-gtk ];
	config.hyprland.default = [ "hyprland" "gtk" ]
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
