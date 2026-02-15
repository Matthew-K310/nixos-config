# ~/nixos-config/dev.nix
{ config, pkgs, ... }:

{
  # Custom package installations
  environment.systemPackages = with pkgs; [
    # tooling
    git
    delta
    hugo
    just
    gnumake
    libtool
    # go
    go
    templ
    # zig
    zig
    # rust
    cargo
    rustc
    rustup
    rust-analyzer
    # c
    cmake
    gcc
    clang
    gcc
    # haskell
    ghc
    haskell-language-server
    cabal-install
    # misc
    lua
    pnpm
    dino
    nodejs
    python3
  ];
}

