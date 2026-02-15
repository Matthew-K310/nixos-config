# ~/nixos-config/services.nix
{ config, pkgs, ... }:

{
  # Custom package installations
  environment.systemPackages = with pkgs; [
    kanata
    bluez
    blueman
    msmtp
    isync
    (pass.withExtensions (ext: with ext; [
      pass-otp
      pass-import
      pass-genphrase
    ]))
    pass
    age
    pinentry-gnome3
    pinentry-tty
  ];

  hardware.bluetooth.enable = true;
  services.blueman.enable = true;

  networking.networkmanager.enable = true;

  services.tailscale = {
    enable = true;
    useRoutingFeatures = "client"; # or "both" if this machine routes traffic
  };

  # Enable CUPS to print documents.
  services.printing.enable = true;

  services.kanata.enable = true;
  systemd.services.kanata = {
    description = "Kanata Service";

    requires = [ "local-fs.target" ];
    after = [ "local-fs.target" ];

    serviceConfig = {
      ExecStartPre = "${pkgs.kmod}/bin/modprobe uinput";
      ExecStart = "${pkgs.kanata}/bin/kanata -c /etc/kanata/kanata.kbd";
      Restart = "no";
    };

    wantedBy = [ "sysinit.target" ];
  };

  # programs.mtr.enable = true;
  programs.gnupg.agent = {
    enable = true;
    pinentryPackage = pkgs.pinentry-gnome3;
    enableSSHSupport = true;
  };

  services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  networking.firewall.enable = false;

}

