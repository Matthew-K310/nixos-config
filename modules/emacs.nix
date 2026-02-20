# ~/nixos-config/eamcs.nix
{ config, pkgs, ... }:

{
  # Custom package installations
  environment.systemPackages = with pkgs; [
    emacs
    ripgrep
    autoconf
    mu
    msmtp
    isync
    emacsPackages.mu4e
    w3m
    ispell
    pinentry-emacs
    texlive.combined.scheme-full
    vips
    wmctrl
    texlive.combined.scheme-full
    sqlite
    aspell
    aspellDicts.en
    aspellDicts.en-computers
    aspellDicts.en-science
    nodejs
    nodePackages.js-beautify
    shellcheck
    html-tidy
    stylelint
    ledger
    shfmt
    graphviz
    emacs-all-the-icons-fonts
    auctex
    pandoc
  ];
}

