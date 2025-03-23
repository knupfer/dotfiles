{ pkgs }:
rec {
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

  iosevka = pkgs.iosevka.override {
    privateBuildPlan = {
      family = "Iosevka";
      spacing = "term";
      serifs = "sans";
      nocCvSs = false;
      exportGlyphNames = false;
      variants.design.asterisk = "penta-low";
    };
    set = "";
  };

  examina = {src}: let

    fontsConf = pkgs.makeFontsConf {
      fontDirectories = [ iosevka pkgs.eb-garamond pkgs.libertinus ];
    };

  in pkgs.stdenv.mkDerivation {
    name = "examina";
    src = pkgs.lib.fileset.toSource {
      root = src;
      fileset = pkgs.lib.fileset.union (src + /examina.org) (src + /img);
    };
    buildInputs = [
      emacs
      tex
      pkgs.git
      pkgs.lilypond
    ];

    FONTCONFIG_FILE= fontsConf;
    OSFONTDIR = "${iosevka}";

    buildPhase = ''
    export TEXMFVAR="$TMPDIR/texmf-var"
    mkdir -p "$TEXMFVAR"
    export HOME=$(mktemp -d)
    mkdir -p ~/.emacs.d/lilypond

    emacs --batch examina.org --eval "(org-latex-export-to-pdf)"
  '';

    installPhase = ''
    mkdir -p $out
    cp examina.pdf $out/
  '';
  };
}
