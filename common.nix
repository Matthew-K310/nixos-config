{ config, pkgs, ... }:
# In your configuration.nix
let
  blockyProfile = "default"; # Change this to "default", "focus", etc.
  profiles = {
    default = [ "ads" "adult" "smut" "ai" ];
    focus = [ "ads" "adult" "smut" "ai" "wasters" ];
  };

  smutList = pkgs.writeText "smut-blocklist.txt" ''
    reddit.com
    literotica.com
    deviantart.com
    tumblr.com
  '';

  aiList = pkgs.writeText "ai-blocklist.txt" ''
    claude.ai
    chatgpt.com
    gemini.google.com
  '';

  wastersList = pkgs.writeText "wasters-blocklist.txt" ''
    youtube.com
    floatplane.com
    redlib.catsearch.com
    nitter.net
  '';
in
{
  nixpkgs.config.allowUnfree = true;

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
    extraGroups = [ "networkmanager" "wheel" "i2c" ];
    shell = pkgs.zsh;
  };
  
  programs.zsh.enable = true;
  users.defaultUserShell = pkgs.zsh;

  environment.systemPackages = with pkgs; [
    vim
    wget
    alacritty
    kitty
    # ungoogled-chromium
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
    nerd-fonts.terminess-ttf
    font-awesome
    alegreya
    noto-fonts-color-emoji
    noto-fonts-cjk-sans
  ];

  networking.nameservers = [ "127.0.0.1" ];
  networking.networkmanager.dns = "none"; # to prevent /etc/resolv.conf from being overwritten

  # networking.stevenblack = {
  #   enable = true;
  #   block = [ "fakenews" "gambling" "porn" "social" ];
  # };

  services.blocky = {
    enable = true;
    settings = {
      ports.dns = 53; # Port for incoming DNS Queries.
      upstreams.groups.default = [
        "https://one.one.one.one/dns-query" # Using Cloudflare's DNS over HTTPS server for resolving queries.
      ];
      # For initially solving DoH/DoT Requests when no system Resolver is available.
      bootstrapDns = {
        upstream = "https://one.one.one.one/dns-query";
        ips = [ "1.1.1.1" "1.0.0.1" ];
      };
      #Enable Blocking of certain domains.
      blocking = {
        denylists = {
          ads    = [ "https://raw.githubusercontent.com/StevenBlack/hosts/master/hosts" ];
          adult  = [ "https://blocklistproject.github.io/Lists/porn.txt" ];
          smut      = [ "${smutList}" ];
          ai      = [ "${aiList}" ];
          wasters = [ "${wastersList}" ];
        };
        clientGroupsBlock.default = profiles.${blockyProfile};
      };
    };
  };
  
  nix.settings.experimental-features = [ "nix-command" "flakes" ];
}
