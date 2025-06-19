{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";
  };
  outputs = { self, nixos-hardware, nixpkgs }:

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

          i3status-rs = pkgs.symlinkJoin {
            name = "i3status-rs";
            paths = [ pkgs.i3status-rust ];
            buildInputs = [ pkgs.makeWrapper ];
            postBuild = ''
    wrapProgram $out/bin/i3status-rs \
      --add-flags "${./sway/i3status-rs.toml}"
    '';
          };
        };

        nixosModules.knupfer = {
          console.keyMap = ./keyboard/loadkeys/kfr.map;
          services.displayManager = {
            ly = {
              enable = true;
              settings = {
                hide_borders = true;
                hide_key_hints = true;
                hide_version_string = true;
              };
              x11Support = false;
            };
            gdm.enable = false;
          };
        };

        nixosModules.ramirez = {
          services = {
            displayManager = {
              autoLogin = {
                enable = true;
                user = "ramirez";
              };
            };
          };
        };

        nixosModules.s440 = {
          imports = [ self.nixosModules.default ];
          boot = {
            loader.grub = {
              device      = "/dev/sda";
              enable      = true;
              splashImage = null;
            };
            loader.timeout = 1;
            initrd.luks.devices.root = {
              device = "/dev/sda3";
              preLVM = true;
            };
          };
          networking.hostName = "s440";
        };

        nixosModules.mipro = {
          imports =
            [ self.nixosModules.default
              nixos-hardware.nixosModules.common-gpu-intel-kaby-lake
              nixos-hardware.nixosModules.common-cpu-intel
            ];
          boot = {
            loader.grub = {
              configurationLimit = 15;
              device = "/dev/nvme0n1";
              enable = true;
              splashImage = null;
              extraConfig = ''
        set color_normal=dark-gray/black
        set menu_color_highlight=red/black
        set timeout_style=hidden
        set cmdline_linux=noht
      '';
              extraEntries = ''
        menuentry "Windows" {
          chainloader (hd0,1)+1
        }
      '';
            };
            loader.timeout = 1;
            initrd = {
              luks.devices.root = {
                device = "/dev/disk/by-uuid/3d0b7d6c-12ec-4683-8ca0-4949d5656cf0";
                preLVM = true;
                allowDiscards = true;
              };
            };
          };
          hardware = {
            graphics = {
              extraPackages = [
                pkgs.intel-media-driver
                pkgs.intel-compute-runtime
              ];
            };
          };
          networking.hostName = "mipro";
          services.logind.lidSwitch = "suspend";
        };

        nixosModules.default = let my = self.packages.x86_64-linux; in {
          boot = {
            kernelPackages = pkgs.linuxPackages_latest;
            tmp.useTmpfs = true;
          };
          environment = {
            systemPackages = (with pkgs; [
              age

              alsa-utils

              borgbackup #borg create --exclude-caches /mnt/borgcube::$HOSTNAME-$USER-{now:%Y-%m-%d} ~
              bibata-cursors #configure with dconf-editor
              btop

              cabal2nix

              evince

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

              signal-desktop

              vlc

              zip
              unzip
            ]);
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
          hardware = {
            enableRedistributableFirmware = true;
            graphics.enable = true;
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
            registry.nixpkgs.flake = nixpkgs;
            settings.download-buffer-size = 524288000;
            settings.experimental-features = [ "nix-command" "flakes" ];
          };
          programs = {
            foot = {
              enable = true;
              settings = {
                main.font = "monospace:semibold:size=14";
                colors.background = "000000";
              };
            };
            light = {
              enable = true;
              brightnessKeys = {
                enable = true;
                minBrightness = 2;
                step = 5;
              };
            };
            sway = {
              enable = true;
              extraOptions = ["--config=${./sway/config}"];
              extraPackages = [pkgs.wmenu pkgs.alsa-utils pkgs.swayidle pkgs.waylock my.emacs my.i3status-rs];
            };
          };
          services = {
            desktopManager.gnome.enable = true;
            displayManager.gdm.enable = pkgs.lib.mkDefault true;
            emacs = {
              defaultEditor = true;
              enable = true;
              package = my.emacs;
            };
            libinput.enable = true;
            logind.lidSwitch = pkgs.lib.mkDefault "hibernate";
            ollama.enable = true;
            openssh = {
              enable = true;
              settings = {
                PermitRootLogin = "no";
                PasswordAuthentication = false;
              };
            };
            power-profiles-daemon.enable = false;
            printing = {
              enable = true;
              drivers = [ pkgs.epson-escpr ];
            };
            tlp.enable = true;
            xserver.xkb.extraLayouts.knu = {
              description = "My custom xkb layouts.";
              languages = [ "de" ];
              symbolsFile = ./keyboard/xkb/knu;
            };
          };
          security.pam.services.waylock = {};
          time.timeZone = "Europe/Berlin";
          users = {
            mutableUsers = false;
            users = {
              knupfer = {
                extraGroups = [ "wheel" "networkmanager" "video" ];
                isNormalUser = true;
                uid=1000;
              };
              ramirez = {
                extraGroups  = [ "wheel" "networkmanager" "video" ];
                isNormalUser = true;
                uid=1001;
              };
              gast = {
                password = "gast";
                isNormalUser = true;
                uid=1002;
              };
            };
          };
        };
      };
}
