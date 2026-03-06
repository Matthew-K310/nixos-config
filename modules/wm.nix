# ~/nixos-config/modules/wm.nix
{ config, pkgs, ... }:

{
  # programs.hyprland = {
  #   enable = true;
  #   xwayland.enable = true;
  # };

  programs.niri = {
    enable = true;
    xwayland.enable = true;
  };

  xdg.portal = {
	  enable = true;
	  # extraPortals = [ pkgs.xdg-desktop-portal-hyprland pkgs.xdg-desktop-portal-gtk ];
	  # config.hyprland.default = [ "hyprland" "gtk" ];
	  # config.hyprland.default = [ "hyprland" "gtk" ];
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
    nordzy-icon-theme
    nordzy-cursor-theme
    wl-clipboard
	  wl-kbptr
	  wlrctl
  ];

  # for managing the brightness of displayport monitors
  services.udev.extraRules = ''
    SUBSYSTEM=="i2c-dev", TAG+="uaccess"
  '';

  hardware.i2c.enable = true;

}
