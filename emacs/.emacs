(require 'package)                                                                         
(package-initialize)                                                                       

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(add-to-list 'load-path "~/.emacs.d/lisp/")
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

(require 'org-install)
(require 'hideshow-org)

(defvar hcz-set-cursor-color-color "")
(defvar hcz-set-cursor-color-buffer "")

(defun knu/publish () 
  "Runs my script, which does a bit cosmetic and cleanup."
  (eshell-command "sh ~/git/knupfer.github.io/_org/publish.sh")
  )
(defun knu/org-archive ()
  "Moves archived trees to the bottom of the father."
  (interactive)
  (org-toggle-archive-tag)
  (unless (org-at-heading-p) (error "Not at an headline"))
  (save-excursion (while (ignore-errors (org-move-subtree-down))))
  )
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
          (let ((alltags (split-string (or (org-entry-get (point) "ALLTAGS") "") ":"))
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
(defun toggle-mode-line () 
  (interactive)
  (when (equal mode-line-format nil)
    (setq mode-line-format (default-value 'mode-line-format))
    (redraw-display)
            ))

(load "pretty-symbols.el")

(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cm" 'magit-status)
(global-set-key [f12] 'toggle-mode-line)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LilyPond-indent-level 4)
 '(blink-cursor-mode nil)
 '(c-default-style (quote ((c-mode . "stroustrup") (java-mode . "java") (awk-mode . "awk") (other . "gnu"))))
 '(column-number-mode t)
 '(cua-mode t nil (cua-base))
 '(cua-normal-cursor-color "black")
 '(custom-enabled-themes (quote (deeper-blue)))
 '(display-time-24hr-format t)
 '(display-time-mode t)
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(eshell-banner-message "
  #############
  #
  #  +----------------------------+
  #  | Welcome to the Emacs shell |
  #  +----------------------------+
  #
")
 '(eshell-cmpl-compare-entry-function (quote string-lessp))
 '(eshell-modules-list (quote (eshell-alias eshell-banner eshell-basic eshell-cmpl eshell-dirs eshell-glob eshell-hist eshell-ls eshell-pred eshell-prompt eshell-script eshell-smart eshell-term eshell-unix)))
 '(eshell-plain-grep-behavior t)
 '(ess-default-style (quote C++))
 '(ess-fancy-comments nil)
 '(font-lock-global-modes (quote (not w3m-minor-mode)))
 '(font-use-system-font nil)
 '(fringe-mode (quote (0)) nil (fringe))
 '(global-whitespace-mode t)
 '(hc-other-chars (quote ("~,.!?{}[]():;»«›‹-_/\\+&")))
 '(hc-other-chars-font-lock-override (quote keep))
 '(hfy-ignored-properties nil)
 '(hl-paren-colors (quote ("#05ffff" "#e07fef" "#f0cf05" "#ee5555" "#ffffff" "#00ff00")))
 '(hs-hide-comments-when-hiding-all nil)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(ispell-highlight-face (quote flyspell-incorrect))
 '(ispell-local-dictionary "de_DE")
 '(ispell-program-name "aspell")
 '(menu-bar-mode nil)
 '(normal-erase-is-backspace t)
 '(org-babel-load-languages (quote ((python . t) (ditaa . t) (sh . t) (lilypond . t) (R . t) (emacs-lisp . t))))
 '(org-confirm-babel-evaluate nil)
 '(org-ditaa-eps-jar-path "~/.emacs.d/DitaaEps.jar")
 '(org-ditaa-jar-path "~/.emacs.d/ditaa0_9.jar")
 '(org-edit-src-content-indentation 0)
 '(org-export-headline-levels 4)
 '(org-export-html-xml-declaration (quote (("html" . "--- ---") ("php" . "<?php echo \"<?xml version=\\\"1.0\\\" encoding=\\\"%s\\\" ?>\"; ?>"))))
 '(org-hierarchical-todo-statistics nil)
 '(org-html-doctype "xhtml-strict")
 '(org-html-head " ")
 '(org-image-actual-width 200)
 '(org-list-indent-offset 2)
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
 '(pretty-symbol-categories (lambda relational logical kdm-custom))
 '(pretty-symbol-patterns (quote ((955 lambda "\\<lambda\\>" (emacs-lisp-mode inferior-lisp-mode lisp-mode scheme-mode python-mode inferior-python-mode prog-mode)) (402 lambda "\\<function\\>" (js-mode)) (8800 relational "!=" (c-mode c++-mode go-mode java-mode js-mode perl-mode cperl-mode ruby-mode python-mode inferior-python-mode ess-mode)) (8800 relational "/=" (emacs-lisp-mode inferior-lisp-mode lisp-mode scheme-mode)) (8805 relational ">=" (c-mode c++-mode go-mode java-mode js-mode perl-mode cperl-mode ruby-mode python-mode inferior-python-mode emacs-lisp-mode inferior-lisp-mode lisp-mode scheme-mode ess-mode)) (8804 relational "<=" (c-mode c++-mode go-mode java-mode js-mode perl-mode cperl-mode ruby-mode python-mode inferior-python-mode emacs-lisp-mode inferior-lisp-mode lisp-mode scheme-mode ess-mode)) (8743 logical "&&" (c-mode c++-mode go-mode java-mode js-mode perl-mode cperl-mode ruby-mode python-mode inferior-python-mode ess-mode)) (8743 logical "\\<and\\>" (emacs-lisp-mode inferior-lisp-mode lisp-mode scheme-mode)) (8744 logical "||" (c-mode c++-mode go-mode java-mode js-mode perl-mode cperl-mode ruby-mode python-mode inferior-python-mode ess-mode)) (8744 logical "\\<or\\>" (emacs-lisp-mode inferior-lisp-mode lisp-mode scheme-mode)) (172 logical "\\<not\\>" (emacs-lisp-mode inferior-lisp-mode lisp-mode scheme-mode)))))
 '(scroll-bar-mode nil)
 '(sml/hidden-modes (quote (" hl-p" " hs+" " WS" " ws")))
 '(sml/mode-width (quote full))
 '(sml/name-width 15)
 '(sml/replacer-regexp-list (quote (("^~/Org/" ":Org:") ("^~/\\.emacs\\.d/" ":ED:") ("^/sudo:.*:" ":SU:") ("^~/Documents/" ":Doc:") ("^~/Dropbox/" ":DB:") ("^:\\([^:]*\\):Documento?s/" ":\\1/Doc:") ("^~/[Gg]it/" ":G:") ("^~/[Gg]it[Hh]ub/" ":Git:") ("^~/[Gg]it\\([Hh]ub\\|\\)-?[Pp]rojects/" ":Git:"))))
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(tool-bar-position (quote right))
 '(visual-line-fringe-indicators (quote (nil right-curly-arrow)))
 '(w3m-fontify-before-hook (quote (font-lock-mode)))
 '(w3m-session-load-crashed-sessions nil)
 '(whitespace-display-mappings (quote ((space-mark 32 [124] [46]))))
 '(whitespace-empty-at-eob-regexp "^ *\\( \\) \\{20\\}")
 '(whitespace-hspace-regexp "^ *\\(\\( \\)\\) \\{7\\}")
 '(whitespace-indentation-regexp (quote ("^a*\\(\\(a\\{%d\\}\\)+\\)" . "^ *\\( \\) \\{19\\}")))
 '(whitespace-line-column 200)
 '(whitespace-space-after-tab-regexp (quote ("^a*\\(\\( \\)\\) \\{19\\}" . "^ *\\( \\) \\{15\\}")))
 '(whitespace-space-before-tab-regexp "^ *\\(\\( \\)\\) \\{3\\}")
 '(whitespace-style (quote (face tabs space-before-tab space-after-tab tab-mark spaces space-mark trailing indentation)))
 '(whitespace-tab-regexp "^ *\\(\\( \\)\\) \\{11\\}")
 '(whitespace-trailing-regexp "\\([^ *äöüßÄÖÜA-Za-z0-9]\\|\\<and\\>\\|\\<or\\>\\|\\<und\\>\\|\\<oder\\>\\|\\<not\\>\\|\\<nicht\\>\\|\\<nil\\>\\)")
 '(word-wrap t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#000000" :foreground "#ffffff" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 165 :width normal :foundry "unknown" :family "Source Code Pro"))))
 '(cursor ((t (:background "#709"))))
 '(flyspell-duplicate ((t (:underline (:color "gold1" :style wave)))))
 '(flyspell-incorrect ((t (:underline (:color "firebrick3" :style wave)))))
 '(fringe ((t (:background "black" :foreground "#0ff"))))
 '(hc-hard-hyphen ((nil)) t)
 '(hc-hard-space ((nil)) t)
 '(hc-other-char ((t (:foreground "#22aaaa"))) t)
 '(hc-tab ((t nil)) t)
 '(hc-trailing-whitespace ((nil)) t)
 '(hl-paren-face ((t (:weight ultra-bold))) t)
 '(magit-header ((t (:background "#044" :foreground "#5fe"))))
 '(mode-line ((t (:background "#033" :foreground "#9bb" :box nil))))
 '(mode-line-buffer-id ((t (:foreground "#99dddd" :box nil :weight bold))))
 '(mode-line-inactive ((t (:inherit mode-line :background "gray32" :foreground "black" :box nil :weight light))))
 '(org-archived ((t (:foreground "#254555"))))
 '(org-checkbox ((t (:inherit bold :foreground "#2f2"))))
 '(org-done ((t (:foreground "#5f5" :weight ultra-bold))))
 '(org-hide ((t (:foreground "#777"))))
 '(org-indent ((t (:background "black" :foreground "black"))) t)
 '(org-todo ((t (:foreground "#faa" :weight ultra-bold))))
 '(outline-1 ((t (:inherit font-lock-function-name-face :foreground "SkyBlue1" :weight bold))) t)
 '(region ((t (:background "#505"))))
 '(tool-bar ((t (:background "grey95" :foreground "black"))))
 '(trailing-whitespace ((t (:background "VioletRed4"))))
 '(w3m-arrived-anchor ((t (:foreground "#8888ee"))))
 '(w3m-current-anchor ((t (:weight ultra-bold))))
 '(w3m-tab-background ((t (:background "Black" :foreground "#88dddd"))))
 '(w3m-tab-selected ((t (:background "Gray75" :foreground "Black"))))
 '(w3m-tab-selected-retrieving ((t (:background "#dd6666" :foreground "Black"))))
 '(w3m-tab-unselected ((t (:background "Gray30" :foreground "Black"))))
 '(w3m-tab-unselected-retrieving ((t (:background "#aa4444" :foreground "Black"))))
 '(w3m-tab-unselected-unseen ((t (:background "Gray90" :foreground "Black"))))
 '(whitespace-empty ((t (:background "VioletRed4" :foreground "firebrick"))))
 '(whitespace-hspace ((t (:foreground "#114444"))))
 '(whitespace-indentation ((t (:foreground "#661144"))))
 '(whitespace-line ((t (:background "#250025"))))
 '(whitespace-newline ((t (:foreground "gray15" :weight normal))))
 '(whitespace-space ((t (:foreground "black"))))
 '(whitespace-space-after-tab ((t (:foreground "#441133"))))
 '(whitespace-space-before-tab ((t (:foreground "#006666"))))
 '(whitespace-tab ((t (:foreground "#113333"))))
 '(whitespace-trailing ((t (:foreground "#22aaaa")))))


;(load "knu-testing.el" t)
(load "knu-pretty-symbol.el")
;; This file must be created and pointing to the apropriate file.
;; It may contain e.g. (load "knu-tablet.el") or (load "knu-desktop.el") etc.
(load "knu-device.el" t)
;; The hooks should be loaded at the end.
(load "knu-hooks.el")

(sml/setup)
