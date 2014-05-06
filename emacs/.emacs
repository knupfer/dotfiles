(require 'package)
(package-initialize)

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'load-path "~/.emacs.d/predict/")
(add-to-list 'load-path "~/git/global-emacs/")
(add-to-list 'load-path "~/git/indentation-tree.el/")
(add-to-list 'load-path "~/git/indentation-tree/")
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

(require 'predictive)
(require 'global-emacs)
(require 'indentation-tree)
(require 'pretty-symbols)

(require 'emms-setup)
(emms-standard)
(emms-default-players)

(defvar hcz-set-cursor-color-color "")
(defvar hcz-set-cursor-color-buffer "")
(global-emacs-mode t)

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
  (let ((color
         (if buffer-read-only "white"
           (if overwrite-mode "#fa0"
             "#909"))))
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
 '(gnus-init-file "~/.emacs.d/gnus.el")
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
 '(normal-erase-is-backspace t)
 '(org-babel-load-languages (quote ((python . t) (ditaa . t) (sh . t) (lilypond . t) (R . t) (emacs-lisp . t))))
 '(org-confirm-babel-evaluate nil)
 '(org-ditaa-eps-jar-path "~/.emacs.d/DitaaEps.jar")
 '(org-ditaa-jar-path "~/.emacs.d/ditaa0_9.jar")
 '(org-edit-src-content-indentation 4)
 '(org-export-headline-levels 4)
 '(org-export-run-in-background t)
 '(org-format-latex-options (quote (:foreground "#0da" :background default :scale 3 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers ("begin" "$1" "$" "$$" "\\(" "\\["))))
 '(org-hierarchical-todo-statistics nil)
 '(org-highlight-latex-and-related (quote (latex script entities)))
 '(org-image-actual-width 200)
 '(org-latex-inactive-timestamp-format "\\textsc{%s}")
 '(org-latex-preview-ltxpng-directory "~/ltxpreview/")
 '(org-list-empty-line-terminates-plain-lists t)
 '(org-log-done (quote time))
 '(org-publish-project-alist (quote (("Homepage" :base-directory "~/git/knupfer.github.io/_org/" :base-extension "org" :publishing-directory "~/git/knupfer.github.io/_processing/" :recursive t :publishing-function org-html-publish-to-html :headline-levels 5 :body-only t :completion-function knu/publish))))
 '(org-replace-disputed-keys t)
 '(org-src-fontify-natively t)
 '(org-startup-align-all-tables t)
 '(org-startup-folded (quote content))
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
 '(visual-line-fringe-indicators (quote (nil right-curly-arrow)))
 '(volume-amixer-default-channel "Speaker")
 '(volume-backend (quote volume-amixer-backend))
 '(volume-electric-mode t)
 '(w3m-filter-configuration (quote ((t ("Strip Google's click-tracking code from link urls" "Google \343\201\256 click-tracking \343\202\263\343\203\274\343\203\211\343\202\222\343\203\252\343\203\263\343\202\257\343\201\256 url \343\201\213\343\202\211\345\217\226\343\202\212\351\231\244\343\201\215\343\201\276\343\201\231") "\\`https?://[a-z]+\\.google\\." w3m-filter-google-click-tracking) (t ("Align table columns vertically to shrink the table width in Google" "Google \346\244\234\347\264\242\347\265\220\346\236\234\343\201\256\343\203\206\343\203\274\343\203\226\343\203\253\343\202\222\347\270\246\346\226\271\345\220\221\343\201\247\346\217\203\343\201\210\343\201\246\345\271\205\343\202\222\347\213\255\343\202\201\343\201\276\343\201\231") "\\`http://\\(www\\|images\\|news\\|maps\\|groups\\)\\.google\\." w3m-filter-google-shrink-table-width) (t ("Add name anchors that w3m can handle in all pages" "\343\201\231\343\201\271\343\201\246\343\201\256\343\203\232\343\203\274\343\202\270\343\201\253 w3m \343\201\214\346\211\261\343\201\210\343\202\213 name \343\202\242\343\203\263\343\202\253\343\203\274\343\202\222\350\277\275\345\212\240\343\201\227\343\201\276\343\201\231") "" w3m-filter-add-name-anchors) (t ("Substitute disabled attr with readonly attr in forms" "\343\203\225\343\202\251\343\203\274\343\203\240\344\270\255\343\201\256 disabled \345\261\236\346\200\247\343\202\222 readonly \345\261\236\346\200\247\343\201\247\344\273\243\347\224\250\343\201\227\343\201\276\343\201\231") "" w3m-filter-subst-disabled-with-readonly) (nil ("Render <tfoot>...</tfoot> after <tbody>...</tbody>" "\343\203\206\343\203\274\343\203\226\343\203\253\345\206\205\343\201\256 <tfoot> \343\202\222 <tbody> \343\201\256\345\276\214\343\201\253\346\217\217\347\224\273\343\201\227\343\201\276\343\201\231") "" w3m-filter-fix-tfoot-rendering) (nil ("Remove garbage in http://www.geocities.co.jp/*" "http://www.geocities.co.jp/* \343\201\247\343\202\264\343\203\237\343\202\222\345\217\226\343\202\212\351\231\244\343\201\215\343\201\276\343\201\231") "\\`http://www\\.geocities\\.co\\.jp/" (w3m-filter-delete-regions "<DIV ALIGN=CENTER>
<!--*/GeoGuide/*-->" "<!--*/GeoGuide/*-->
</DIV>")) (nil ("Remove ADV in http://*.hp.infoseek.co.jp/*" "http://*.hp.infoseek.co.jp/* \343\201\247\345\272\203\345\221\212\343\202\222\345\217\226\343\202\212\351\231\244\343\201\215\343\201\276\343\201\231") "\\`http://[a-z]+\\.hp\\.infoseek\\.co\\.jp/" (w3m-filter-delete-regions "<!-- start AD -->" "<!-- end AD -->")) (nil ("Remove ADV in http://linux.ascii24.com/linux/*" "http://linux.ascii24.com/linux/* \343\201\247\345\272\203\345\221\212\343\202\222\345\217\226\343\202\212\351\231\244\343\201\215\343\201\276\343\201\231") "\\`http://linux\\.ascii24\\.com/linux/" (w3m-filter-delete-regions "<!-- DAC CHANNEL AD START -->" "<!-- DAC CHANNEL AD END -->")) (nil "A filter for Google" "\\`http://\\(www\\|images\\|news\\|maps\\|groups\\)\\.google\\." w3m-filter-google) (nil "A filter for Amazon" "\\`https?://\\(?:www\\.\\)?amazon\\.\\(?:com\\|co\\.\\(?:jp\\|uk\\)\\|fr\\|de\\)/" w3m-filter-amazon) (nil ("A filter for Mixi.jp" "\343\203\237\343\202\257\343\202\267\343\202\243\347\224\250\343\203\225\343\202\243\343\203\253\343\202\277") "\\`https?://mixi\\.jp" w3m-filter-mixi) (nil "A filter for http://eow.alc.co.jp/*/UTF-8*" "\\`http://eow\\.alc\\.co\\.jp/[^/]+/UTF-8" w3m-filter-alc) (nil ("A filter for Asahi Shimbun" "\346\234\235\346\227\245\346\226\260\350\201\236\347\224\250\343\203\225\343\202\243\343\203\253\343\202\277") "\\`http://www\\.asahi\\.com/" w3m-filter-asahi-shimbun) (nil "A filter for http://imepita.jp/NUM/NUM*" "\\`http://imepita\\.jp/[0-9]+/[0-9]+" w3m-filter-imepita) (nil "A filter for http://allatanys.jp/*" "\\`http://allatanys\\.jp/" w3m-filter-allatanys) (t "A filter for Wikipedia" "\\`http://.*\\.wikipedia\\.org/" w3m-filter-wikipedia) (nil ("Remove inline frames in all pages" "\343\201\231\343\201\271\343\201\246\343\201\256\343\203\232\343\203\274\343\202\270\343\201\247\343\202\244\343\203\263\343\203\251\343\202\244\343\203\263\343\203\225\343\203\254\343\203\274\343\203\240\343\202\222\345\217\226\343\202\212\351\231\244\343\201\215\343\201\276\343\201\231") "" w3m-filter-iframe) (t "Remove googles header noise" "\\`http://\\(www\\|images\\|news\\|maps\\|groups\\)\\.google\\." (w3m-filter-delete-regions "<b class=gb1>Search</b>" "<div id=\"topstuff\"></div>")) (t "Remove googles footer noise" "\\`http://\\(www\\|images\\|news\\|maps\\|groups\\)\\.google\\." (w3m-filter-delete-regions "<p class=\"flc\" id=\"bfl\"" ")</script>")) (t "Remove Wikipedia headers" "\\`http://.*\\.wikipedia\\.org/" (w3m-filter-delete-regions "<div id=\"siteSub\">" "class=\"mw-content-ltr\">")) (t "Remove Wikipedia footer1" "\\`http://.*\\.wikipedia\\.org/" (w3m-filter-delete-regions "<div id=\"mw-navigation\">" "<div class=\"portal\" role=\"navigation\" id='p-lang' aria-labelledby='p-lang-label'>")) (t "Remove Wikipedia footer2" "\\`http://.*\\.wikipedia\\.org/" (w3m-filter-delete-regions "<li class=\"uls-p-lang-dummy\">" "</html>")))))
 '(w3m-home-page "about:blank")
 '(w3m-session-load-crashed-sessions nil)
 '(w3m-use-title-buffer-name t)
 '(whitespace-style (quote (face lines-tail trailing tabs)))
 '(whitespace-tab-regexp "\\(\\\\alpha\\|\\\\beta\\|\\\\gamma\\|\\\\mu\\|\\\\nu\\|\\\\epsilon\\|\\\\lambda\\|\\\\sigma\\|\\\\tau\\|\\\\eta\\|\\\\omega\\|\\\\theta\\|\\\\rho\\|\\\\phi\\|\\\\psi\\|\\\\upsilon\\|\\\\pi\\|\\\\delta\\|\\\\kappa\\|\\\\xi\\|\\\\chi\\|\\\\Pi\\|\\\\Gamma\\|\\\\Omega\\|\\\\Lambda\\|\\\\nabla\\|\\\\Delta\\|\\\\int\\|\\\\oint\\|\\\\times\\|\\\\cdot\\|\\\\sum\\|\\\\pm\\|\\\\mp\\|\\\\geq\\|\\\\leq\\|\\\\neq\\|\\\\approx\\|\\\\rightarrow\\|\\\\leftarrow\\|\\\\Rightarrow\\|\\\\Leftarrow\\|\\\\mapsto\\|\\\\curvearrowright\\|\\\\leftrightarrow\\)"))

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

(load "knu-hooks.el")
(load "knu-lisp.el")
(load "knu-pretty-symbols.el")
;;(sml/setup)
(flyspell-lazy-mode)
;; This file must be created and pointing to the apropriate file.
;; It may contain e.g. (load "knu-tablet.el") or (load "knu-desktop.el") etc.
(load "knu-device.el" t)
(eshell)
;;(w3m)

