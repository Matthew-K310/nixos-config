# ~/nixos-config/modules/dev.nix
{ config, pkgs, ... }:

{
  # Custom package installations
  environment.systemPackages = with pkgs; [
    # tooling
    git
	  gh
	  forgejo-cli
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
    clang-tools
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
	  prettier
  ];

  virtualisation.docker.enable = true;
}

