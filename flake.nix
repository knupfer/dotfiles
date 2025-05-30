{

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { self, nixpkgs }:

    let
      pkgs = import nixpkgs { system = "x86_64-linux"; };
    in

      {

        packages.x86_64-linux = rec {
          tex = pkgs.texliveBasic.withPackages (ps: with ps;
            [ dvisvgm ulem microtype siunitx xkeyval
              wrapfig capt-of babel-german collection-langgerman
              titling fancyhdr
              newtx iftex fontaxes xstring
              ocgx2 media9
              garamond-math
              ebgaramond
              pgfornament pgf pgfopts
              fontspec unicode-math lualatex-math
              minted upquote lineno
              pgfplots
            ] );

          emacs = pkgs.symlinkJoin {
            name = "emacs";
            paths = [ (pkgs.emacs30-pgtk.pkgs.withPackages (melpa: with melpa;
              [ org-inline-pdf avy bbdb flycheck gptel haskell-mode ledger-mode ligature magit markdown-mode nix-mode ] )) ];
            buildInputs = [ pkgs.makeWrapper ];
            postBuild = ''
    $out/bin/emacs --batch \
      --eval "(native-compile \"${./emacs/init.el}\" \"$out/share/emacs/native-lisp/init.eln\")" \

    wrapProgram $out/bin/emacs \
      --prefix PATH : ${pkgs.lib.makeBinPath [ tex pkgs.mupdf pdf2svg ]} \
      --add-flags "--load $out/share/emacs/native-lisp/init.eln"
    '';
          };

          pdf2svg = pkgs.writeShellApplication {
            name = "pdf2svg";
            runtimeInputs = [ pkgs.gnused pkgs.pdf2svg pkgs.cairosvg ];
            text =
              ''
#!/usr/bin/env bash
pdf2svg "$1" "$2"
sed -i "s/rgb(0%, 0%, 0%)/rgb(100%, 0%, 100%)/" "$2"
cairosvg -f svg -s 3 -o "$2" "$2"
'';};

          yambar = pkgs.symlinkJoin {
            name = "yambar";
            paths = [ pkgs.yambar ];
            buildInputs = [ pkgs.makeWrapper ];
            postBuild = ''
    wrapProgram $out/bin/yambar \
      --add-flags "--config=${./sway/yambar.conf} --backend=wayland"
    '';
          };
        };

        nixosModules.default = let my = self.packages.x86_64-linux; in {
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
              iosevka
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
            daemonCPUSchedPolicy = "idle";
            daemonIOSchedClass = "idle";
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
        };
      };
}
