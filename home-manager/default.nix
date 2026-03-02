{ config, pkgs, lib, username, ... }:

let
  dotfilesDir = "/home/${username}/nixos-config/dotfiles";
  localDir = "/home/${username}/nixos-config/.local";
in
{
  home.stateVersion = "24.11";
  home.username = username;
  home.homeDirectory = "/home/${username}";

  home.file = {
    ".config/doom".source = config.lib.file.mkOutOfStoreSymlink "${dotfilesDir}/doom";
    ".config/emacs".source = config.lib.file.mkOutOfStoreSymlink "${dotfilesDir}/emacs";
    ".config/git".source = config.lib.file.mkOutOfStoreSymlink "${dotfilesDir}/git";
    ".config/hypr".source = config.lib.file.mkOutOfStoreSymlink "${dotfilesDir}/hypr";
    ".config/isyncrc".source = config.lib.file.mkOutOfStoreSymlink "${dotfilesDir}/mbsync/config";
    ".config/kanata".source = config.lib.file.mkOutOfStoreSymlink "${dotfilesDir}/kanata";
    ".config/kitty".source = config.lib.file.mkOutOfStoreSymlink "${dotfilesDir}/kitty";
    ".config/mbsync".source = config.lib.file.mkOutOfStoreSymlink "${dotfilesDir}/mbsync";  # Fixed typo: mbysnc -> mbsync
    ".config/mpd".source = config.lib.file.mkOutOfStoreSymlink "${dotfilesDir}/mpd";
    ".config/mpv".source = config.lib.file.mkOutOfStoreSymlink "${dotfilesDir}/mpv";
    ".config/msmtp".source = config.lib.file.mkOutOfStoreSymlink "${dotfilesDir}/msmtp";
    ".config/nvim".source = config.lib.file.mkOutOfStoreSymlink "${dotfilesDir}/nvim";
    ".config/oxwm".source = config.lib.file.mkOutOfStoreSymlink "${dotfilesDir}/oxwm";
    ".config/profanity".source = config.lib.file.mkOutOfStoreSymlink "${dotfilesDir}/profanity";
    ".config/rmpc".source = config.lib.file.mkOutOfStoreSymlink "${dotfilesDir}/rmpc";
    ".config/scripts".source = config.lib.file.mkOutOfStoreSymlink "${dotfilesDir}/scripts";
    ".config/shell".source = config.lib.file.mkOutOfStoreSymlink "${dotfilesDir}/shell";
    ".config/swaync".source = config.lib.file.mkOutOfStoreSymlink "${dotfilesDir}/swaync";
    ".config/tmux".source = config.lib.file.mkOutOfStoreSymlink "${dotfilesDir}/tmux";
    ".config/waybar".source = config.lib.file.mkOutOfStoreSymlink "${dotfilesDir}/waybar";
    ".config/zsh".source = config.lib.file.mkOutOfStoreSymlink "${dotfilesDir}/zsh";
    ".local/bin".source = config.lib.file.mkOutOfStoreSymlink "${localDir}/bin";
    ".local/share/chars".source = config.lib.file.mkOutOfStoreSymlink "${localDir}/share/chars";
    ".zshrc".source = config.lib.file.mkOutOfStoreSymlink "${dotfilesDir}/zsh/.zshrc";
    ".zprofile".source = config.lib.file.mkOutOfStoreSymlink "${dotfilesDir}/shell/profile";
  };

  home.activation.fixDotfilesPermissions = lib.hm.dag.entryAfter ["writeBoundary"] ''
    $DRY_RUN_CMD chown -R $USER:users ${dotfilesDir}
    $DRY_RUN_CMD chmod -R u+w ${dotfilesDir}
    $DRY_RUN_CMD chown -R $USER:users ${localDir}
    $DRY_RUN_CMD chmod -R u+w ${localDir}
  '';

  xdg.mimeApps = {
    enable = true;
    defaultApplications = {
      "text/html" = "helium.desktop";
      "x-scheme-handler/http" = "helium.desktop";
      "x-scheme-handler/https" = "helium.desktop";
      "x-scheme-handler/about" = "helium.desktop";
      "x-scheme-handler/unknown" = "helium.desktop";
    };
  };
}
