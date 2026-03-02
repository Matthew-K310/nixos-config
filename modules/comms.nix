# ~/nixos-config/modules/comms.nix
{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    profanity
    gurk-rs 
    signal-desktop
  ];
}
