{ pkgs, ... }:
let

  my = import ./myPkgs.nix { inherit pkgs; };

in

{
  boot.tmp.useTmpfs = true;
  console.keyMap = ./keyboard/loadkeys/kfr.map;
  environment = {
    systemPackages = (with pkgs; [
      age

      alsa-utils

      borgbackup
      bibata-cursors #configure with dconf-editor
      btop

      cabal2nix

      firefox

      git
      gimp

      haskellPackages.bench
      haskellPackages.cabal-install
      haskellPackages.ghc

      libreoffice
      lilypond

      pandoc
      passage

      vlc

      zip
      unzip
    ]);
    variables.EDITOR = "${my.emacs}/bin/emacsclient -c --alternate-editor=";

  };
  fonts = {
    fontconfig = {
      defaultFonts = {
        monospace = ["Iosevka"];
        sansSerif = ["Libertinus Sans"];
        serif     = ["EB Garamond"];
      };
    };
    packages = (with pkgs; [
      libertinus
      my.iosevka
      (iosevka-bin.override {variant = "Aile";})
      (iosevka-bin.override {variant = "Etoile";})
      (iosevka-bin.override {variant = "Slab";})
      sarasa-gothic
      eb-garamond
    ]);
  };
  i18n.defaultLocale = "de_DE.UTF-8";
  networking.networkmanager.enable = true;
  nix = {
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 30d";
    };
    optimise = {
      automatic = true;
      dates = ["weekly"];
    };
    settings.experimental-features = [ "nix-command" "flakes" ];
  };
  programs = {
    foot = {
      enable = true;
      settings = {
        main.font = "monospace:size=14";
        colors.background = "000000";
      };
    };
    light = {
      enable = true;
      brightnessKeys.enable = true;
    };
    sway = {
      enable = true;
      extraOptions = ["--config=${./sway/config}"];
      extraPackages = [pkgs.wmenu pkgs.alsa-utils pkgs.swayidle pkgs.waylock my.yambar my.emacs];
    };
  };
  services = {
    libinput.enable = true;
    ollama.enable = true;
    power-profiles-daemon.enable = false;
    printing = {
      enable = true;
      drivers = [ pkgs.epson-escpr ];
    };
    tlp.enable = true;
  };
  security.pam.services.waylock = {};
  services.xserver.xkb.extraLayouts.knu = {
    description = "My custom xkb layouts.";
    languages = [ "de" ];
    symbolsFile = ./keyboard/xkb/knu;
  };
  time.timeZone = "Europe/Berlin";
  users.mutableUsers = false;
}
