{ pkgs, ... }:
let

  dotfiles = ./.;

  myEmacs = let tex = (pkgs.texlive.combine {inherit (pkgs.texlive) iwona scheme-basic dvipng ulem;}); in pkgs.symlinkJoin {
    name = "emacs";
    paths = [ ((pkgs.emacs.override { withPgtk = true; }).pkgs.withPackages (melpa: with melpa;
      [ avy bbdb flycheck gptel haskell-mode ledger-mode ligature magit markdown-mode nix-mode ] )) ];
    buildInputs = [ pkgs.makeWrapper ];
    postBuild = ''
    $out/bin/emacs --batch \
      --eval "(native-compile \"${dotfiles}/emacs/init.el\" \"$out/share/emacs/native-lisp/init.eln\")" \

    wrapProgram $out/bin/emacs \
      --prefix PATH : ${pkgs.lib.makeBinPath [ tex ]} \
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
  environment.variables.EDITOR = "${myEmacs}/bin/emacsclient -c --alternate-editor=";
  fonts = {
    fontconfig = {
      defaultFonts = {
        monospace = ["Iosevka"];
        sansSerif = ["Iosevka Aile"];
        serif     = ["Iosevka Etoile"];
      };
    };
    packages = (with pkgs; [
      libertine
      iosevka
      (iosevka-bin.override {variant = "Aile";})
      (iosevka-bin.override {variant = "Etoile";})
      (iosevka-bin.override {variant = "Slab";})
      sarasa-gothic
    ]);
  };
  i18n.defaultLocale = "de_DE.UTF-8";
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
}
