# ~/nixos-config/modules/services.nix
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
}

