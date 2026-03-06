{ config, lib, pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    gh
    forgejo-cli
    delta
  ];
  programs.git = {
    enable = true;
    config = {
      user = {
        name  = "Matthew-K310";
        email = "it@matthew-kennedy.com";
        signingKey = "E18A4E6A498526625185E1DC598BDE7E73DEB741";
      };
      core = {
        fileMode       = true;
        ignoreCase     = false;
        symlinks       = true;
        compression    = 9;
        excludesFile   = "~/.config/git/ignore";
        attributesFile = "~/.config/git/attributes";
        hooksPath      = "~/.config/git/hooks";
        pager          = "delta";
        editor         = "nvim";
      };
      format.signOff = true;
      gpg = {
        program = "gpg";
        format  = "openpgp";
      };
      init.defaultBranch = "master";
      commit = {
        verbose  = true;
        gpgSign  = true;
      };
      fetch = {
        prune    = true;
        parallel = 3;
      };
      submodule.fetchJobs = 3;
      pull.rebase = false;
      push = {
        gpgSign         = "false";
        default         = "simple";
        autoSetupRemote = true;
        followTags      = true;
      };
      help.autoCorrect = "prompt";
    };
  };
}
