{ pkgs, ... }:
let

  dotfiles = ./.;


  myPdf2svg = pkgs.writeShellApplication {
    name = "pdf2svg";
    runtimeInputs = with pkgs; [ gnused pdf2svg cairosvg ];
    text =
''
#!/usr/bin/env bash
pdf2svg "$1" "$2"
sed -i "s/rgb(0%, 0%, 0%)/rgb(100%, 0%, 100%)/" "$2"
cairosvg -f svg -s 3 -o "$2" "$2"
'';};

  lilypond-mode = melpa: melpa.trivialBuild rec {
    pname = "lilypond-mode";
    version = pkgs.lilypond.version;
    src = "${pkgs.lilypond}/share/emacs/site-lisp";
  };

  tex = pkgs.texliveBasic.withPackages (ps: with ps;
    [ dvisvgm ulem microtype siunitx xkeyval libertinus-otf
      wrapfig capt-of babel-german collection-langgerman
      titling fancyhdr
      ocgx2 media9
      garamond-math
      ebgaramond
      pgfornament pgf pgfopts
      fontspec unicode-math lualatex-math # lualatex
    ] );

  myEmacs = pkgs.symlinkJoin {
    name = "emacs";
    paths = [ (pkgs.emacs30-pgtk.pkgs.withPackages (melpa: with melpa;
      [ (lilypond-mode melpa) org-inline-pdf avy bbdb flycheck gptel haskell-mode ledger-mode ligature magit markdown-mode nix-mode ] )) ];
    buildInputs = [ pkgs.makeWrapper ];
    postBuild = ''
    $out/bin/emacs --batch \
      --eval "(native-compile \"${dotfiles}/emacs/init.el\" \"$out/share/emacs/native-lisp/init.eln\")" \

    wrapProgram $out/bin/emacs \
      --prefix PATH : ${pkgs.lib.makeBinPath [ tex pkgs.mupdf myPdf2svg ]} \
      --add-flags "--load $out/share/emacs/native-lisp/init.eln"
    '';
  };

  myYambar = pkgs.symlinkJoin {
    name = "yambar";
    paths = [ pkgs.yambar ];
    buildInputs = [ pkgs.makeWrapper ];
    postBuild = ''
    wrapProgram $out/bin/yambar \
      --add-flags "--config=${dotfiles}/sway/yambar.conf --backend=wayland"
    '';
  };

in

{
  boot.tmp.useTmpfs = true;
  console.keyMap = "${dotfiles}/keyboard/loadkeys/kfr.map";
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
    variables.EDITOR = "${myEmacs}/bin/emacsclient -c --alternate-editor=";
  };
  fonts = {
    fontconfig = {
      defaultFonts = {
        monospace = ["Iosevka Term"];
        sansSerif = ["Libertinus Sans"];
        serif     = ["Libertinus Serif"];
      };
    };
    packages = (with pkgs; [
      libertinus
      iosevka
      (iosevka-bin.override {variant = "Aile";})
      (iosevka-bin.override {variant = "Etoile";})
      (iosevka-bin.override {variant = "Slab";})
      (iosevka-bin.override {variant = "SGr-IosevkaTerm";})
      sarasa-gothic
      eb-garamond
    ]);
  };
  i18n.defaultLocale = "de_DE.UTF-8";
  networking.networkmanager.enable = true;
  nix.settings.experimental-features = [ "nix-command" "flakes" ];
  programs = {
    foot = {
      enable = true;
      settings = {
        main.font = "monospace:size=12";
        colors.background = "000000";
      };
    };
    light = {
      enable = true;
      brightnessKeys.enable = true;
    };
    sway = {
      enable = true;
      extraOptions = ["--config=${dotfiles}/sway/config"];
      extraPackages = [pkgs.wmenu pkgs.alsa-utils pkgs.swayidle pkgs.waylock myYambar myEmacs];
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
    symbolsFile = "${dotfiles}/keyboard/xkb/knu";
  };
  time.timeZone = "Europe/Berlin";
  users.mutableUsers = false;
}
