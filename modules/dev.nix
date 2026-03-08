# ~/nixos-config/modules/dev.nix
{ config, pkgs, ... }:

{
  # Custom package installations
  environment.systemPackages = with pkgs; [
    # tooling
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
    godot
    # lib dependencies
    xorg.libX11
    xorg.libXcursor
    xorg.libXrandr
    xorg.libXinerama
    xorg.libXi
    libGL
    pkg-config
	android-tools
  ];

  virtualisation.docker.enable = true;
}

