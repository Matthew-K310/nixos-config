# ~/nixos-config/graphics.nix
{ config, pkgs, ... }:

{

  # for managing the brightness of displayport monitors
  services.udev.extraRules = ''
    SUBSYSTEM=="i2c-dev", TAG+="uaccess"
  '';

  hardware.i2c.enable = true;

  # Custom package installations
  environment.systemPackages = with pkgs; [
    gimp
    darktable
    obs-studio
    mpv
    yt-dlp
	ddcutil
    brightnessctl
  ];
}
