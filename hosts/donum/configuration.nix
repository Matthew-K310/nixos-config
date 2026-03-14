{ config, lib, pkgs, ... }:

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
			../../modules/network.nix
# ../../modules/oldskool.nix
			../../modules/services.nix
			../../modules/tools.nix
			../../modules/wm.nix
	];

	networking.hostName = "donum";

# Use systemd-boot instead of GRUB
	boot.loader.systemd-boot.enable = true;
	boot.loader.efi.canTouchEfiVariables = true;

# Use latest kernel
	boot.kernelPackages = pkgs.linuxPackages_latest;

# Keyboard layout
	services.xserver = {
		xkb.layout = "us";
		xkb.variant = "colemak_dh";
	};
	console.keyMap = "colemak";

	users.users.matthewkennedy = {
		isNormalUser = true;
		extraGroups = [ "wheel" "networkmanager" ];
	};

	nixpkgs.config.allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) [
		"vintagestory"
	];

	system.stateVersion = "24.11";
}
