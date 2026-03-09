# ~/nixos-config/modules/network.nix
{ config, pkgs, ... }:

{
  # Custom package installations
  environment.systemPackages = with pkgs; [
	cups
	system-config-printer
	hplip
	mullvad-vpn # for gui vpn management
  ];

  networking.networkmanager.enable = true;

  services.mullvad-vpn.enable = true;

  services.tailscale = {
    enable = true;
    useRoutingFeatures = "client"; # or "both" if this machine routes traffic
  };

  # Enable CUPS to print documents.
  services.printing.enable = true;

  services.avahi = {
    enable = true;
    nssmdns4 = true;
    # openFirewall = true;
  };

  services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  networking.firewall.enable = false;

}

