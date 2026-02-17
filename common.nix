{ config, pkgs, ... }:
{
  # Shared settings for all hosts
  time.timeZone = "America/Chicago";
  
  i18n.defaultLocale = "en_US.UTF-8";
  i18n.extraLocaleSettings = {
    LC_ADDRESS = "en_US.UTF-8";
    LC_IDENTIFICATION = "en_US.UTF-8";
    LC_MEASUREMENT = "en_US.UTF-8";
    LC_MONETARY = "en_US.UTF-8";
    LC_NAME = "en_US.UTF-8";
    LC_NUMERIC = "en_US.UTF-8";
    LC_PAPER = "en_US.UTF-8";
    LC_TELEPHONE = "en_US.UTF-8";
    LC_TIME = "en_US.UTF-8";
  };
  
  users.users.matthewkennedy = {
    isNormalUser = true;
    description = "Matthew Kennedy";
    extraGroups = [ "networkmanager" "wheel" ];
    shell = pkgs.zsh;
  };
  
  programs.zsh.enable = true;
  users.defaultUserShell = pkgs.zsh;
  
  nixpkgs.config.allowUnfree = true;
  
  environment.systemPackages = with pkgs; [
    vim
    wget
    alacritty
    kitty
    ungoogled-chromium
    git-crypt
    killall
    rsync
    libappimage
    appimage-run
  ];
  
  fonts.packages = with pkgs; [
    nerd-fonts.iosevka
    nerd-fonts.jetbrains-mono
    nerd-fonts.geist-mono
    font-awesome
    alegreya
    noto-fonts-color-emoji
    noto-fonts-cjk-sans
  ];
  
  nix.settings.experimental-features = [ "nix-command" "flakes" ];
}
