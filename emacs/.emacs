(require 'package)
(package-initialize)

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'load-path "~/.emacs.d/predict/")
(add-to-list 'load-path "~/git/indentation-tree.el/")
(add-to-list 'load-path "~/git/indentation-tree/")
(add-to-list 'load-path "~/git/macro-type/")
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(require 'hl-defined)
(require 'predictive)
(require 'indentation-tree)
(require 'pretty-symbols)
(require 'yasnippet)
(require 'macro-type)
(yas-global-mode 1)

(defvar hcz-set-cursor-color-color "")
(defvar hcz-set-cursor-color-buffer "")

(defalias 'yes-or-no-p 'y-or-n-p)

(defun knu/publish ()
  "Runs my script, which does a bit cosmetic and cleanup."
  (shell-command "sh ~/git/knupfer.github.io/_org/publish.sh"))

(defun knu/git-auto-fetch ()
  "Runs a script to fetch every directory in ~/git"
  (interactive)
  (async-start
   ;; What to do in the child process
   (lambda ()
     (shell-command "sh ~/git/dotfiles/emacs/.emacs.d/scripts/knu-git-fetch.sh"))
   'ignore))

(defun knu/org-archive ()
  "Moves archived trees to the bottom of the father."
  (interactive)
  (org-toggle-archive-tag)
  (unless (org-at-heading-p) (error "Not at an headline"))
  (save-excursion (while (ignore-errors (org-move-subtree-down)))))

(defun org-summary-todo (n-done n-not-done)
  "Switch entry do DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states) ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(defun dmj/org-remove-redundant-tags ()
  "Remove redundant tags of headlines in current buffer.
A tag is considered redundant if it is local to a headline and
inherited by a parent headline."
  (interactive)
  (when (eq major-mode 'org-mode)
    (save-excursion
      (org-map-entries
       '(lambda ()
          (let ((alltags (split-string
                          (or (org-entry-get (point) "ALLTAGS") "") ":"))
                local inherited tag)
            (dolist (tag alltags)
              (if (get-text-property 0 'inherited tag)
                  (push tag inherited) (push tag local)))
            (dolist (tag local)
              (if (member tag inherited) (org-toggle-tag tag 'off)))))
       t nil))))

(defun hcz-set-cursor-color-according-to-mode ()
  "change cursor color according to some minor modes."
  ;; set-cursor-color is somewhat costly, so we only call it when needed:
  (let ((color (if (and (boundp 'knu-org-mode-map) (org-inside-LaTeX-fragment-p)) "#2a6"
                 (if buffer-read-only "white"
                   (if overwrite-mode "#fa0"
                     "#909")))))
    (unless (and
             (string= color hcz-set-cursor-color-color)
             (string= (buffer-name) hcz-set-cursor-color-buffer))
      (set-cursor-color (setq hcz-set-cursor-color-color color))
      (setq hcz-set-cursor-color-buffer (buffer-name)))))

(defun switch-to-previous-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

;; Fetches git repos after an hour idle time.
(run-with-idle-timer 3600 t 'knu/git-auto-fetch)
(run-with-idle-timer 1 nil 'knu/git-auto-fetch)

(define-key global-map "\C-cm" 'magit-status)
(define-key global-map (kbd "`") 'switch-to-previous-buffer)
(define-key global-map (kbd "S-SPC") 'ace-jump-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LilyPond-indent-level 4)
 '(Man-notify-method (quote pushy))
 '(Man-width 80)
 '(blink-cursor-mode nil)
 '(c-default-style (quote ((c-mode . "stroustrup") (java-mode . "java") (awk-mode . "awk") (other . "gnu"))))
 '(column-number-mode t)
 '(cua-mode t nil (cua-base))
 '(cua-normal-cursor-color "black")
 '(custom-enabled-themes (quote (deeper-blue)))
 '(display-time-24hr-format t)
 '(display-time-mode t)
 '(ediff-split-window-function (quote split-window-horizontally))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(erc-nick "quxbam")
 '(erc-prompt ">>>")
 '(erc-prompt-for-password nil)
 '(erc-system-name "foobar")
 '(eshell-banner-message "")
 '(eshell-cmpl-compare-entry-function (quote string-lessp))
 '(eshell-modules-list (quote (eshell-alias eshell-banner eshell-basic eshell-cmpl eshell-dirs eshell-glob eshell-hist eshell-ls eshell-pred eshell-prompt eshell-script eshell-smart eshell-term eshell-unix)))
 '(eshell-plain-grep-behavior t)
 '(ess-default-style (quote C++))
 '(font-use-system-font nil)
 '(fringe-mode (quote (0)) nil (fringe))
 '(gnus-default-adaptive-word-score-alist (quote ((82 . 1) (67 . -1) (75 . -2) (114 . -1))))
 '(gnus-init-file "~/.emacs.d/gnus.el")
 '(gnus-treat-fill-article t)
 '(gnus-treat-leading-whitespace t)
 '(gnus-treat-strip-multiple-blank-lines t)
 '(gnus-treat-strip-trailing-blank-lines t)
 '(gnus-treat-unsplit-urls t)
 '(hl-paren-colors (quote ("#05ffff" "#e07fef" "#f0cf05" "#ee5555" "#ffffff" "#00ff00")))
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(ispell-highlight-face (quote flyspell-incorrect))
 '(ispell-local-dictionary "de_DE")
 '(ispell-program-name "aspell")
 '(keyfreq-autosave-mode t)
 '(keyfreq-mode t)
 '(kill-do-not-save-duplicates t)
 '(mail-user-agent (quote gnus-user-agent))
 '(menu-bar-mode nil)
 '(message-insert-canlock nil)
 '(org-babel-load-languages (quote ((python . t) (ditaa . t) (sh . t) (lilypond . t) (R . t) (emacs-lisp . t))))
 '(org-confirm-babel-evaluate nil)
 '(org-ditaa-eps-jar-path "~/.emacs.d/DitaaEps.jar")
 '(org-ditaa-jar-path "~/.emacs.d/ditaa0_9.jar")
 '(org-drawers (quote ("PROPERTIES" "CLOCK" "LOGBOOK" "RESULTS" "INIT")))
 '(org-edit-src-content-indentation 4)
 '(org-export-headline-levels 4)
 '(org-export-run-in-background t)
 '(org-format-latex-header "\\documentclass{article}
\\usepackage[usenames]{color}
\\usepackage{etoolbox}
\\usepackage{mdframed}
[PACKAGES]
[DEFAULT-PACKAGES]
\\pagestyle{empty}             % do not remove
% The settings below are copied from fullpage.sty
\\setlength{\\textwidth}{\\paperwidth}
\\addtolength{\\textwidth}{-13cm}
\\setlength{\\oddsidemargin}{1.5cm}
\\addtolength{\\oddsidemargin}{-2.54cm}
\\setlength{\\evensidemargin}{\\oddsidemargin}
\\setlength{\\textheight}{\\paperheight}
\\addtolength{\\textheight}{-\\headheight}
\\addtolength{\\textheight}{-\\headsep}
\\addtolength{\\textheight}{-\\footskip}
\\addtolength{\\textheight}{-3cm}
\\setlength{\\topmargin}{1.5cm}
\\addtolength{\\topmargin}{-2.54cm}
\\definecolor{bg}{rgb}{0,0.1,0.1}\\definecolor{fg}{rgb}{0.2,1,0.7}
\\BeforeBeginEnvironment{align*}{\\begin{mdframed}[backgroundcolor=bg, innertopmargin=-0.2cm]\\color{fg}}
\\AfterEndEnvironment{align*}{\\end{mdframed}}
\\BeforeBeginEnvironment{align}{\\begin{mdframed}[backgroundcolor=bg, innertopmargin=-0.2cm]\\color{fg}}
\\AfterEndEnvironment{align}{\\end{mdframed}}
\\BeforeBeginEnvironment{gather*}{\\begin{mdframed}[backgroundcolor=bg, innertopmargin=-0.2cm]\\color{fg}}
\\AfterEndEnvironment{gather*}{\\end{mdframed}}
\\BeforeBeginEnvironment{gather}{\\begin{mdframed}[backgroundcolor=bg, innertopmargin=-0.2cm]\\color{fg}}
\\AfterEndEnvironment{gather}{\\end{mdframed}}
\\newenvironment{definition}{\\begin{mdframed}[backgroundcolor=bg]\\color{fg} \\textbf{\\textsc{Definition:}} }{\\end{mdframed}}
\\newenvironment{note}{\\begin{mdframed}[backgroundcolor=bg]\\color{fg} \\textbf{\\textsc{Bemerkung:}} }{\\end{mdframed}}
\\newenvironment{example}{\\begin{mdframed}[backgroundcolor=bg]\\color{fg} \\textbf{\\textsc{Beispiel:}} }{\\end{mdframed}}")
 '(org-format-latex-options (quote (:foreground "#0da" :background default :scale 3.5 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers ("begin" "$1" "$" "$$" "\\(" "\\["))))
 '(org-hierarchical-todo-statistics nil)
 '(org-highlight-latex-and-related (quote (latex script entities)))
 '(org-image-actual-width 200)
 '(org-latex-classes (quote (("article" "\\documentclass[11pt]{scrartcl}" ("\\section{%s}" . "\\section*{%s}") ("\\subsection{%s}" . "\\subsection*{%s}") ("\\subsubsection{%s}" . "\\subsubsection*{%s}") ("\\paragraph{%s}" . "\\paragraph*{%s}") ("\\subparagraph{%s}" . "\\subparagraph*{%s}")) ("report" "\\documentclass[11pt]{report}" ("\\part{%s}" . "\\part*{%s}") ("\\chapter{%s}" . "\\chapter*{%s}") ("\\section{%s}" . "\\section*{%s}") ("\\subsection{%s}" . "\\subsection*{%s}") ("\\subsubsection{%s}" . "\\subsubsection*{%s}")) ("book" "\\documentclass[11pt]{book}" ("\\part{%s}" . "\\part*{%s}") ("\\chapter{%s}" . "\\chapter*{%s}") ("\\section{%s}" . "\\section*{%s}") ("\\subsection{%s}" . "\\subsection*{%s}") ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))))
 '(org-latex-inactive-timestamp-format "\\\\\\hfill\\textcolor{gray}{\\textbf{%s}}\\\\")
 '(org-latex-preview-ltxpng-directory "~/ltxpreview/")
 '(org-list-allow-alphabetical t)
 '(org-list-empty-line-terminates-plain-lists t)
 '(org-log-done (quote time))
 '(org-publish-project-alist (quote (("Homepage" :base-directory "~/git/knupfer.github.io/_org/" :base-extension "org" :publishing-directory "~/git/knupfer.github.io/_processing/" :recursive t :publishing-function org-html-publish-to-html :headline-levels 5 :body-only t :completion-function knu/publish))))
 '(org-replace-disputed-keys t)
 '(org-src-fontify-natively t)
 '(org-startup-align-all-tables t)
 '(org-startup-folded (quote content))
 '(org-startup-indented t)
 '(org-startup-truncated nil)
 '(org-startup-with-inline-images t)
 '(org-support-shift-select (quote always))
 '(org-todo-keyword-faces (quote (("FAILED" . "#f00") ("CANCELED" . "#ee3"))))
 '(org-todo-keywords (quote ((sequence "TODO" "|" "DONE" "CANCELED" "FAILED"))))
 '(pretty-symbol-categories (quote (knu-custom)))
 '(proced-format-alist (quote ((short pid tree pcpu time (args comm)) (medium user pid tree pcpu pmem vsize rss ttname state start time (args comm)) (long user euid group pid tree pri nice pcpu pmem vsize rss ttname state start time (args comm)) (verbose user euid group egid pid ppid tree pgrp sess pri nice pcpu pmem state thcount vsize rss ttname tpgid minflt majflt cminflt cmajflt start time utime stime ctime cutime cstime etime (args comm)))))
 '(python-shell-interpreter "python3")
 '(scroll-bar-mode nil)
 '(scroll-conservatively 101)
 '(sml/hidden-modes (quote (" hl-p" " hs+" " WS" " ws")))
 '(sml/mode-width (quote full))
 '(sml/name-width 15)
 '(sml/replacer-regexp-list (quote (("^~/Org/" ":Org:") ("^~/\\.emacs\\.d/" ":ED:") ("^/sudo:.*:" ":SU:") ("^~/Documents/" ":Doc:") ("^~/Dropbox/" ":DB:") ("^:\\([^:]*\\):Documento?s/" ":\\1/Doc:") ("^~/[Gg]it/" ":G:") ("^~/[Gg]it[Hh]ub/" ":Git:") ("^~/[Gg]it\\([Hh]ub\\|\\)-?[Pp]rojects/" ":Git:"))))
 '(split-height-threshold nil)
 '(split-width-threshold 80)
 '(tool-bar-mode nil)
 '(tramp-default-method "ssh")
 '(tramp-default-method-alist (quote (("80\\.240\\.140\\.83#50683" "quxbar" "scpc") (nil "%" "smb") ("\\`\\(127\\.0\\.0\\.1\\|::1\\|localhost6?\\)\\'" "\\`root\\'" "su") (nil "\\`\\(anonymous\\|ftp\\)\\'" "ftp") ("\\`ftp\\." nil "ftp"))))
 '(tramp-default-proxies-alist (quote (("80.240.140.83#50683" "root" "/ssh:quxbar@80.240.140.83#50683:"))))
 '(visual-line-fringe-indicators (quote (nil right-curly-arrow)))
 '(volume-amixer-default-channel "Speaker")
 '(volume-backend (quote volume-amixer-backend))
 '(volume-electric-mode t)
 '(w3m-enable-google-feeling-lucky nil)
 '(w3m-home-page "about:blank")
 '(w3m-search-default-engine "duckduckgo")
 '(w3m-search-engine-alist (quote (("duckduckgo" "https://duckduckgo.com/lite/?q=%s" undecided) ("yahoo" "https://search.yahoo.com/bin/search?p=%s" nil) ("blog" "https://blogsearch.google.com/blogsearch?q=%s&oe=utf-8&ie=utf-8" utf-8) ("blog-en" "https://blogsearch.google.com/blogsearch?q=%s&hl=en&oe=utf-8&ie=utf-8" utf-8) ("google" "https://www.google.com/search?q=%s&ie=utf-8&oe=utf-8" utf-8) ("google-en" "https://www.google.com/search?q=%s&hl=en&ie=utf-8&oe=utf-8" utf-8) ("google news" "https://news.google.com/news?q=%s&ie=utf-8&oe=utf-8" utf-8) ("google news-en" "https://news.google.com/news?q=%s&hl=en&ie=utf-8&oe=utf-8" nil) ("google groups" "https://groups.google.com/groups?q=%s" nil) ("All the Web" "http://www.alltheweb.com/search?q=%s&web&_sb_lang=en" nil) ("technorati" "http://www.technorati.com/search/%s" utf-8) ("technorati-ja" "http://www.technorati.jp/search/search.html?query=%s&language=ja" utf-8) ("technorati-tag" "http://www.technorati.com/tag/%s" utf-8) ("altavista" "https://altavista.com/sites/search/web?q=\"%s\"&kl=ja&search=Search" nil) ("debian-pkg" "http://packages.debian.org/cgi-bin/search_contents.pl?directories=yes&arch=i386&version=unstable&case=insensitive&word=%s" nil) ("debian-bts" "http://bugs.debian.org/cgi-bin/pkgreport.cgi?archive=yes&pkg=%s" nil) ("amazon" "https://www.amazon.com/exec/obidos/search-handle-form/250-7496892-7797857" iso-8859-1 "url=index=blended&field-keywords=%s") ("emacswiki" "http://www.emacswiki.org/cgi-bin/wiki?search=%s" nil) ("en.wikipedia" "https://en.wikipedia.org/wiki/Special:Search?search=%s" nil) ("de.wikipedia" "https://de.wikipedia.org/wiki/Spezial:Search?search=%s" utf-8) ("freshmeat" "http://freshmeat.net/search/?q=%s&section=projects" nil))))
 '(w3m-session-load-crashed-sessions nil)
 '(w3m-uri-replace-alist (quote (("\\`enwi:" w3m-search-uri-replace "en.wikipedia") ("\\`dewi:" w3m-search-uri-replace "de.wikipedia") ("\\`dd:" w3m-search-uri-replace "duckduckgo") ("\\`gg:" w3m-search-uri-replace "google") ("\\`ggg:" w3m-search-uri-replace "google groups") ("\\`ya:" w3m-search-uri-replace "yahoo") ("\\`al:" w3m-search-uri-replace "altavista") ("\\`bts:" w3m-search-uri-replace "debian-bts") ("\\`dpkg:" w3m-search-uri-replace "debian-pkg") ("\\`archie:" w3m-search-uri-replace "iij-archie") ("\\`alc:" w3m-search-uri-replace "alc") ("\\`urn:ietf:rfc:\\([0-9]+\\)" w3m-pattern-uri-replace "http://www.ietf.org/rfc/rfc\\1.txt"))))
 '(w3m-use-favicon nil)
 '(w3m-use-title-buffer-name t)
 '(whitespace-style (quote (face trailing tabs)))
 '(whitespace-tab-regexp "\\(\\\\alpha\\|\\\\beta\\|\\\\gamma\\|\\\\mu\\|\\\\nu\\|\\\\epsilon\\|\\\\lambda\\|\\\\sigma\\|\\\\tau\\|\\\\eta\\|\\\\omega\\|\\\\theta\\|\\\\rho\\|\\\\phi\\|\\\\psi\\|\\\\upsilon\\|\\\\pi\\|\\\\delta\\|\\\\kappa\\|\\\\xi\\|\\\\chi\\|\\\\Pi\\|\\\\Phi\\|\\\\Gamma\\|\\\\Omega\\|\\\\Lambda\\|\\\\nabla\\|\\\\Delta\\|\\\\int\\|\\\\oint\\|\\\\times\\|\\\\cdot\\|\\\\sum\\|\\\\pm\\|\\\\mp\\|\\\\geq\\|\\\\leq\\|\\\\neq\\|\\\\approx\\|\\\\rightarrow\\|\\\\leftarrow\\|\\\\Rightarrow\\|\\\\Leftarrow\\|\\\\mapsto\\|\\\\curvearrowright\\|\\\\leftrightarrow\\|\\\\mathrm{d}\\|\\\\infty\\|\\\\partial\\|\\\\equiv\\|\\\\ll\\|IO \\)"))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#000000" :foreground "#ffffff" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 165 :width normal :foundry "unknown" :family "Source Code Pro"))))
 '(ace-jump-face-foreground ((t (:background "black" :foreground "green" :weight bold))))
 '(cursor ((t (:background "#709"))))
 '(erc-prompt-face ((t (:background "Black" :foreground "lightBlue2" :weight bold))))
 '(flyspell-duplicate ((t (:underline (:color "gold1" :style wave)))))
 '(flyspell-incorrect ((t (:underline (:color "firebrick3" :style wave)))))
 '(fringe ((t (:background "black" :foreground "#0ff"))))
 '(hl-paren-face ((t (:weight ultra-bold))) t)
 '(magit-header ((t (:background "#044" :foreground "#5fe"))))
 '(mode-line ((t (:background "#033" :foreground "#9bb" :box nil))))
 '(mode-line-buffer-id ((t (:foreground "#99dddd" :box nil :weight bold))))
 '(mode-line-inactive ((t (:inherit mode-line :background "gray32" :foreground "black" :box nil :weight light))))
 '(num3-face-even ((t (:foreground "#fa0"))))
 '(org-archived ((t (:foreground "#254555"))))
 '(org-checkbox ((t (:inherit bold :foreground "#2f2"))))
 '(org-done ((t (:foreground "#5f5" :weight ultra-bold))))
 '(org-hide ((t (:foreground "#777"))))
 '(org-indent ((t (:background "black" :foreground "black"))) t)
 '(org-todo ((t (:foreground "#faa" :weight ultra-bold))))
 '(outline-1 ((t (:inherit font-lock-function-name-face :foreground "SkyBlue1" :weight bold))))
 '(region ((t (:background "#505"))))
 '(w3m-arrived-anchor ((t (:foreground "#8888ee"))))
 '(w3m-current-anchor ((t (:weight ultra-bold))))
 '(w3m-tab-background ((t (:background "Black" :foreground "#88dddd"))))
 '(w3m-tab-selected ((t (:background "Gray75" :foreground "Black"))))
 '(w3m-tab-selected-retrieving ((t (:background "#dd6666" :foreground "Black"))))
 '(w3m-tab-unselected ((t (:background "Gray30" :foreground "Black"))))
 '(w3m-tab-unselected-retrieving ((t (:background "#aa4444" :foreground "Black"))))
 '(w3m-tab-unselected-unseen ((t (:background "Gray90" :foreground "Black"))))
 '(whitespace-tab ((t (:background "nil" :foreground "#00eeaa" :weight ultra-bold)))))

(when (equal (getenv "USER") "root")
  (set-face-background 'mode-line "#400")
  (set-face-foreground 'mode-line "#b00")
  (set-face-foreground 'mode-line-buffer-id "#ee5555"))

(load "knu-hooks")
(load "knu-lisp")
(load "knu-pretty-symbols")
;;(sml/setup)
(flyspell-lazy-mode)
;; This file must be created and pointing to the apropriate file.
;; It may contain e.g. (load "knu-tablet.el") or (load "knu-desktop.el") etc.
(load "knu-device" t)
(eshell)
;;(w3m)

