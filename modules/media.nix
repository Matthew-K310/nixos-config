# ~/nixos-config/modules/media.nix
{ config, pkgs, ... }:

{
  services.mpd = {
    enable = true;
    user = "matthewkennedy";

    settings = {
      music_directory = "/home/matthewkennedy/Music";
      playlist_directory = "/home/matthewkennedy/.config/mpd/playlists";
      db_file = "/home/matthewkennedy/.config/mpd/db";
      log_file = "/home/matthewkennedy/.config/mpd/log";
      sticker_file = "/home/matthewkennedy/.config/mpd/sticker.sql";

      bind_to_address = "localhost";
      port = 6600;

      auto_update = "yes";
      metadata_to_use = "+comment";

      audio_output = [
        {
          type = "pipewire";
          name = "PipeWire Audio";
        }
        {
          type = "fifo";
          name = "album_art";
          path = "/tmp/mpd.fifo";
          format = "44100:16:2";
        }
        {
          type = "httpd";
          name = "HTTP Stream";
          encoder = "vorbis";
          port = "8000";
          bind_to_address = "127.0.0.1";
          quality = "5.0";
          format = "44100:16:2";
        }
      ];
    };
  };

  # Enable sound with pipewire.
  # services.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    # If you want to use JACK applications, uncomment this
    jack.enable = true;

    # use the example session manager (no others are packaged yet so this is enabled by default,
    # no need to redefine it in your config for now)
    #media-session.enable = true;
  };

  hardware.bluetooth = {
    enable = true;
    powerOnBoot = true;
    settings = {
      General = {
        # Shows battery charge of connected devices on supported
        # Bluetooth adapters. Defaults to 'false'.
        Experimental = true;
        # When enabled other devices can connect faster to us, however
        # the tradeoff is increased power consumption. Defaults to
        # 'false'.
        FastConnectable = true;
      };
      Policy = {
        # Enable all controllers when they are found. This includes
        # adapters present on start as well as adapters that are plugged
        # in later on. Defaults to 'true'.
        AutoEnable = true;
      };
    };
  };

  # Custom package installations
  environment.systemPackages = with pkgs; [
    # video
    mpv
    yt-dlp
    # music
    mpd
    mpd-mpris
    mpc
    playerctl
	  pulseaudio
    pulsemixer
    rmpc
    jellyfin-tui
    picard
    # production
    gimp
    darktable
    obs-studio
	reaper
	bitwig-studio
	lsp-plugins
  ];
}
