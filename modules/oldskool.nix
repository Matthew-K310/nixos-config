# ~/nixos-config/modules/oldskool.nix
{ config, pkgs, ... }:

{
  services.xserver = {
    enable = true;
    desktopManager = {
      xterm.enable = false;
      xfce.enable = true;
    };
  };

  services.displayManager.defaultSession = "xfce";
  xdg.portal = {
	enable = true;
  };

  environment.systemPackages = with pkgs; [
	gtk3
	gtk2
	# old windows vibes
    chicago95
	# xfce panel packages
	xfce4-panel
	xfce4-mpc-plugin
	xfce4-alsa-plugin
	xfce4-timer-plugin
	xfce4-battery-plugin
	xfce4-panel-profiles
  ];
}
