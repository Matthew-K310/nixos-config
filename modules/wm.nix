# ~/nixos-config/modules/wm.nix
{ config, pkgs, ... }:

{
#  wayland
	programs.hyprland = {
		enable = true;
		xwayland.enable = true;
	};

	xdg.portal = {
		enable = true;
		extraPortals = [ pkgs.xdg-desktop-portal-hyprland pkgs.xdg-desktop-portal-gtk ];
		config.hyprland.default = [ "hyprland" "gtk" ];
	};

	environment.systemPackages = with pkgs; [
# niri
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

# x11
	services.xserver.enable = true;
	services.xserver.windowManager.oxwm.enable = true;

	environment.systemPackages = with pkgs; [
		xclip
			xrandr
			xset
			dunst
			picom
			dmenu
			nsxiv
			scrot
			xwallpaper
			slock
			xinit
			wpctl
			pactl
			playerctl
	];

# for managing the brightness of displayport monitors
	services.udev.extraRules = ''
		SUBSYSTEM=="i2c-dev", TAG+="uaccess"
		'';

	hardware.i2c.enable = true;

}
