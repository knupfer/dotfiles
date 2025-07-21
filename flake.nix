{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    agenix.url = "github:ryantm/agenix";
  };
  outputs = { self, nixpkgs, agenix }:

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
            text = ''
              #!/usr/bin/env bash
              pdf2svg "$1" "$2"
              sed -i "s/rgb(0%, 0%, 0%)/rgb(100%, 0%, 100%)/" "$2"
              cairosvg -f svg -s 3 -o "$2" "$2"
            '';
          };

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

        nixosModules.powerManagement = {
          boot.blacklistedKernelModules = [ "bluetooth" "pcspkr" ];
          boot.kernelParams = [
            "pcie_aspm=force"
            "nvme_core.default_ps_max_latency_us=5500"
            "nowatchdog"
          ];
          fileSystems."/".options = ["noatime" "nodiratime"];
          hardware.bluetooth.enable = false;
          networking.wireless.scanOnLowSignal = false;
          powerManagement.powertop.enable = true;
          security.polkit.extraConfig = ''
            polkit.addRule(function(action, subject) {
              if (action.id == "org.freedesktop.systemd1.manage-units" && action.lookup("unit") == "toggle-performance.service") {
                return polkit.Result.YES;
              }
            });
          '';
          services = {
            fstrim.enable = true;
            pipewire.extraConfig.pipewire."10-low-power" = {
              context.properties = {
                default.clock.allowed-rates = [ 44100 48000 ];
                default.clock.max-quantum = 2048;
                default.clock.min-quantum = 1024;
                default.clock.power-saving = true;
                default.clock.quantum = 1024;
                default.clock.rate = 44100;
                session.suspend-timeout-seconds = 2;
              };
            };
            pulseaudio.enable = false;
            udev.extraRules = ''
              SUBSYSTEM=="power_supply", ACTION=="change", ATTR{online}=="1", RUN+="${pkgs.systemd}/bin/systemctl start powerManagement"
              SUBSYSTEM=="power_supply", ACTION=="change", ATTR{online}=="0", RUN+="${pkgs.systemd}/bin/systemctl start powerManagement"
            '';
          };
          systemd = {
            oomd.enable = false;
            services = {
              powerManagement = {
                wantedBy = [ "multi-user.target" ];
                script = ''
                  if [ "$(cat /sys/class/power_supply/AC/online)" -eq 1 ]; then
                    EPP=balance_performance
                  else
                    EPP=power
                  fi
                  echo $EPP | tee /sys/devices/system/cpu/cpufreq/policy*/energy_performance_preference
                '';
                serviceConfig = {
                  Type = "oneshot";
                  Restart = "on-failure";
                };
              };
              toggle-performance = {
                serviceConfig = {
                  RemainAfterExit = "yes";
                  Type = "oneshot";
                  ExecStart = pkgs.writeShellScript "performance" ''
                    echo performance | tee /sys/devices/system/cpu/cpufreq/policy*/scaling_governor
                    echo performance >     /sys/firmware/acpi/platform_profile
                  '';
                  ExecStop = pkgs.writeShellScript "balanced" ''
                    echo powersave | tee /sys/devices/system/cpu/cpufreq/policy*/scaling_governor
                    echo balanced      > /sys/firmware/acpi/platform_profile
                  '';
                };
              };
            };
          };
        };

        nixosModules.epsonET2550 = {
          environment.systemPackages = [ pkgs.simple-scan ];
          hardware.sane.enable = true;
          services = {
            avahi.enable = true;
            printing = {
              enable = true;
              drivers = [ pkgs.epson-escpr ];
            };
          };
        };

        nixosModules.e14 = {
          imports = [ self.nixosModules.default
                      self.nixosModules.powerManagement
                    ];
          boot.loader.systemd-boot.enable = true;
          boot.loader.efi.canTouchEfiVariables = true;
          boot.initrd.luks.devices.luks-56e161a7-0533-4a2e-8842-f0ffadc0db74.device = "/dev/disk/by-uuid/56e161a7-0533-4a2e-8842-f0ffadc0db74";
          console.keyMap = ./keyboard/loadkeys/kfr.map;
          hardware.graphics = {
            extraPackages = [
              pkgs.intel-media-driver
              pkgs.intel-compute-runtime
            ];
          };
          networking.hostName = "e14";
          services = {
            displayManager.gdm.enable = pkgs.lib.mkForce false;
            displayManager.ly.enable = true;
            desktopManager.gnome.enable = pkgs.lib.mkForce false;
            openssh.enable = false;
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
          hardware = {
            graphics = {
              extraPackages = [
                pkgs.intel-media-driver
                pkgs.intel-compute-runtime-legacy1
              ];
            };
          };
          networking.hostName = "s440";
        };

        nixosModules.mipro = {
          imports =
            [ self.nixosModules.default ];
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
                pkgs.intel-compute-runtime-legacy1
              ];
            };
          };
          networking.hostName = "mipro";
          services.logind.lidSwitch = "suspend";
        };

        nixosModules.default = {config, ...}: let my = self.packages.x86_64-linux; in {

          imports = [
            self.nixosModules.epsonET2550
            agenix.nixosModules.default
          ];

          age = {
            identityPaths = [ "/etc/nixos/secret.agekey" ];
            secrets = {
              knupferHashedPassword.file = secrets/knupferHashedPassword.age;
              ramirezHashedPassword.file = secrets/ramirezHashedPassword.age;
            };
          };

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

              git

              haskellPackages.cabal-install
              haskellPackages.ghc

              libreoffice

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
            settings.download-buffer-size = 524288000;
            settings.experimental-features = [ "nix-command" "flakes" ];
          };
          programs = {
            firefox = {
              enable = true;
              policies = {
                DisableAccounts = true;
                DisableFeedbackCommands = true;
                DisableFirefoxAccounts = true;
                DisableFirefoxStudies = true;
                DisableProfileImport = true;
                DisableTelemetry = true;
                DisplayMenuBar = "never";
                ExtensionSettings = {
                  "*".installation_mode = "blocked"; # blocks all addons except uBlock
                  "uBlock0@raymondhill.net" = {
                    install_url = "https://addons.mozilla.org/firefox/downloads/latest/ublock-origin/latest.xpi";
                    installation_mode = "force_installed";
                  };
                };
                FirefoxHome = {
                  Search = false;
                  TopSites = false;
                  SponsoredTopSites = false;
                  Highlights = false;
                  Snippets = false;
                };
                Homepage.URL = "about:blank";
                NewTabPage = false;
                NoDefaultBookmarks = true;
                SearchEngines.Default = "DuckDuckGo";
                TranslateEnabled = false;
                UserMessaging = {
                  ExtensionRecommendations = false;
                  FeatureRecommendations = false;
                  SkipOnboarding = true;  # Key setting to skip onboarding
                  UrlbarInterventions = false;
                  WhatsNew = false;
                };
                Preferences = {
                  "browser.tabs.opentabfor.middleclick" = false;
                  "browser.theme.content-theme" = 0;
                  "browser.theme.toolbar-theme" = 0;
                  "browser.translations.enable" = false;
                  "browser.link.open_newwindow" = 2;
                  "browser.link.open_newwindow.restriction" = 0;
                  "gfx.webrender.all" = true;
                  "layers.acceleration.force-enabled" = true;
                  "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
                  # add a userChrom.css to remove tabs
                };
              };
            };
            foot = {
              enable = true;
              settings = {
                main.font = "Iosevka:size=13";
                colors.background = "000000";
              };
            };
            light = {
              enable = true;
              brightnessKeys = {
                enable = true;
                step = 5;
              };
            };
            sway = {
              enable = true;
              extraOptions = ["--config=${./sway/config}"];
              extraPackages = [pkgs.wmenu pkgs.alsa-utils pkgs.swayidle pkgs.grim pkgs.slurp my.emacs my.i3status-rs];
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
              enable = pkgs.lib.mkDefault true;
              settings = {
                PermitRootLogin = "no";
                PasswordAuthentication = false;
              };
            };
            power-profiles-daemon.enable = false;
            xserver.xkb.extraLayouts.knu = {
              description = "My custom xkb layouts.";
              languages = [ "de" ];
              symbolsFile = ./keyboard/xkb/knu;
            };
          };
          time.timeZone = "Europe/Berlin";
          users = {
            mutableUsers = false;
            users = {
              knupfer = {
                extraGroups = [ "wheel" "networkmanager" "video" ];
                hashedPasswordFile = config.age.secrets.knupferHashedPassword.path;
                isNormalUser = true;
                uid=1000;
              };
              ramirez = {
                extraGroups = [ "wheel" "networkmanager" "video" ];
                hashedPasswordFile = config.age.secrets.ramirezHashedPassword.path;
                isNormalUser = true;
                openssh.authorizedKeys.keys = [ "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIO4E7bro2Z1/yphVTjPv6R4Se8pMtRrMZYwUXL2u5yh6" ];
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
