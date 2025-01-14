{ pkgs, ... }:
let

  dotfiles = ./.;

  myEmacs = pkgs.symlinkJoin {
    name = "emacs";
    paths = [ ((pkgs.emacs.override { withPgtk = true; }).pkgs.withPackages (melpa: with melpa;
      [ avy bbdb flycheck gptel haskell-mode ledger-mode ligature magit markdown-mode nix-mode ] )) ];
    buildInputs = [ pkgs.makeWrapper ];
    postBuild = ''

    $out/bin/emacs --batch \
      --eval "(native-compile \"${dotfiles}/emacs/init.el\" \"$out/share/emacs/native-lisp/init.eln\")" \

    wrapProgram $out/bin/emacs \
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

  myFoot = pkgs.symlinkJoin {
    name = "foot";
    paths = [ pkgs.foot ];
    buildInputs = [ pkgs.makeWrapper ];
    postBuild = ''
    wrapProgram $out/bin/foot \
      --add-flags "--font=monospace:size=12 --override=colors.background=000000"
    '';
  };

in

{
  environment.variables.EDITOR = "${myEmacs}/bin/emacsclient -c --alternate-editor=";
  console.keyMap = "${dotfiles}/keyboard/loadkeys/kfr.map";
  programs = {
    sway = {
      enable = true;
      extraOptions = ["--config=${dotfiles}/sway/config"];
      extraPackages = [pkgs.wmenu pkgs.alsa-utils pkgs.swayidle pkgs.waylock myYambar myFoot myEmacs];
    };
  };
  services.xserver.xkb.extraLayouts.knu = {
    description = "My custom xkb layouts.";
    languages = [ "de" ];
    symbolsFile = "${dotfiles}/keyboard/xkb/knu";
  };
}
