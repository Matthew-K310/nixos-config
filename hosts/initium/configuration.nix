{ config, pkgs, ... }:

{
  imports = [
    ./hardware-configuration.nix
    ../../common.nix
    ../../modules/cli.nix
    ../../modules/comms.nix
    ../../modules/dev.nix
    ../../modules/git.nix
    ../../modules/emacs.nix
    ../../modules/media.nix
    # ../../modules/oldskool.nix
    ../../modules/services.nix
    ../../modules/tools.nix
    ../../modules/wm.nix
  ];

  networking.hostName = "initium";

  # Use systemd-boot instead of GRUB
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  
  # Use latest kernel
  boot.kernelPackages = pkgs.linuxPackages_latest;

  users.users.matthewkennedy = {
    isNormalUser = true;
    extraGroups = [ "wheel" "networkmanager" ];
  };

  system.stateVersion = "24.11";
}
