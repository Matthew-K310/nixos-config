{ config, lib, pkgs, ... }:

let
  dotfilesDir = "/home/${username}/nixos-config/dotfiles";
in
{
  programs.git {
    enable = true;
    userName = "Matthew-K310";
    userEmail = "it@matthew-kennedy.com";

    extraConfig = {
      init.defaultBranch = "master";
      pull.rebase = true;
    }
  }
}
