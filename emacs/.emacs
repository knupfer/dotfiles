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
(define-key global-map (kbd "C-ö") 'hs-toggle-hiding)

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
 '(fill-column 79)
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
 '(normal-erase-is-backspace t)
 '(org-babel-load-languages (quote ((python . t) (ditaa . t) (sh . t) (lilypond . t) (R . t) (emacs-lisp . t))))
 '(org-confirm-babel-evaluate nil)
 '(org-ditaa-eps-jar-path "~/.emacs.d/DitaaEps.jar")
 '(org-ditaa-jar-path "~/.emacs.d/ditaa0_9.jar")
 '(org-edit-src-content-indentation 4)
 '(org-export-headline-levels 4)
 '(org-export-run-in-background t)
 '(org-format-latex-options (quote (:foreground "#0da" :background default :scale 1.5 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers ("begin" "$1" "$" "$$" "\\(" "\\["))))
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
 '(w3m-filter-configuration (quote ((t ("Strip Google's click-tracking code from link urls" "Google の click-tracking コードをリンクの url から取り除きます") "\\`https?://[a-z]+\\.google\\." w3m-filter-google-click-tracking) (t ("Align table columns vertically to shrink the table width in Google" "Google 検索結果のテーブルを縦方向で揃えて幅を狭めます") "\\`http://\\(www\\|images\\|news\\|maps\\|groups\\)\\.google\\." w3m-filter-google-shrink-table-width) (t ("Add name anchors that w3m can handle in all pages" "すべてのページに w3m が扱える name アンカーを追加します") "" w3m-filter-add-name-anchors) (t ("Substitute disabled attr with readonly attr in forms" "フォーム中の disabled 属性を readonly 属性で代用します") "" w3m-filter-subst-disabled-with-readonly) (nil ("Render <tfoot>...</tfoot> after <tbody>...</tbody>" "テーブル内の <tfoot> を <tbody> の後に描画します") "" w3m-filter-fix-tfoot-rendering) (nil ("Remove garbage in http://www.geocities.co.jp/*" "http://www.geocities.co.jp/* でゴミを取り除きます") "\\`http://www\\.geocities\\.co\\.jp/" (w3m-filter-delete-regions "<DIV ALIGN=CENTER>
<!--*/GeoGuide/*-->" "<!--*/GeoGuide/*-->
</DIV>")) (nil ("Remove ADV in http://*.hp.infoseek.co.jp/*" "http://*.hp.infoseek.co.jp/* で広告を取り除きます") "\\`http://[a-z]+\\.hp\\.infoseek\\.co\\.jp/" (w3m-filter-delete-regions "<!-- start AD -->" "<!-- end AD -->")) (nil ("Remove ADV in http://linux.ascii24.com/linux/*" "http://linux.ascii24.com/linux/* で広告を取り除きます") "\\`http://linux\\.ascii24\\.com/linux/" (w3m-filter-delete-regions "<!-- DAC CHANNEL AD START -->" "<!-- DAC CHANNEL AD END -->")) (nil "A filter for Google" "\\`http://\\(www\\|images\\|news\\|maps\\|groups\\)\\.google\\." w3m-filter-google) (nil "A filter for Amazon" "\\`https?://\\(?:www\\.\\)?amazon\\.\\(?:com\\|co\\.\\(?:jp\\|uk\\)\\|fr\\|de\\)/" w3m-filter-amazon) (nil ("A filter for Mixi.jp" "ミクシィ用フィルタ") "\\`https?://mixi\\.jp" w3m-filter-mixi) (nil "A filter for http://eow.alc.co.jp/*/UTF-8*" "\\`http://eow\\.alc\\.co\\.jp/[^/]+/UTF-8" w3m-filter-alc) (nil ("A filter for Asahi Shimbun" "朝日新聞用フィルタ") "\\`http://www\\.asahi\\.com/" w3m-filter-asahi-shimbun) (nil "A filter for http://imepita.jp/NUM/NUM*" "\\`http://imepita\\.jp/[0-9]+/[0-9]+" w3m-filter-imepita) (nil "A filter for http://allatanys.jp/*" "\\`http://allatanys\\.jp/" w3m-filter-allatanys) (t "A filter for Wikipedia" "\\`http://.*\\.wikipedia\\.org/" w3m-filter-wikipedia) (nil ("Remove inline frames in all pages" "すべてのページでインラインフレームを取り除きます") "" w3m-filter-iframe) (t "Remove googles header noise" "\\`http://\\(www\\|images\\|news\\|maps\\|groups\\)\\.google\\." (w3m-filter-delete-regions "<b class=gb1>Search</b>" "<div id=\"topstuff\"></div>")) (t "Remove googles footer noise" "\\`http://\\(www\\|images\\|news\\|maps\\|groups\\)\\.google\\." (w3m-filter-delete-regions "<p class=\"flc\" id=\"bfl\"" ")</script>")) (t "Remove Wikipedia headers" "\\`http://.*\\.wikipedia\\.org/" (w3m-filter-delete-regions "<div id=\"siteSub\">" "class=\"mw-content-ltr\">")) (t "Remove Wikipedia footer1" "\\`http://.*\\.wikipedia\\.org/" (w3m-filter-delete-regions "<div id=\"mw-navigation\">" "<div class=\"portal\" role=\"navigation\" id='p-lang' aria-labelledby='p-lang-label'>")) (t "Remove Wikipedia footer2" "\\`http://.*\\.wikipedia\\.org/" (w3m-filter-delete-regions "<li class=\"uls-p-lang-dummy\">" "</html>")))))
 '(w3m-home-page "about:blank")
 '(w3m-session-load-crashed-sessions nil)
 '(w3m-use-title-buffer-name t)
 '(whitespace-style (quote (face lines-tail trailing))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#000000" :foreground "#ffffff" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 165 :width normal :foundry "unknown" :family "Source Code Pro"))))
 '(ace-jump-face-foreground ((t (:background "black" :foreground "green" :weight bold))))
 '(cursor ((t (:background "#709"))))
 '(erc-prompt-face ((t (:background "Black" :foreground "lightBlue2" :weight bold))) t)
 '(flyspell-duplicate ((t (:underline (:color "gold1" :style wave)))))
 '(flyspell-incorrect ((t (:underline (:color "firebrick3" :style wave)))))
 '(fringe ((t (:background "black" :foreground "#0ff"))))
 '(hl-paren-face ((t (:weight ultra-bold))) t)
 '(magit-header ((t (:background "#044" :foreground "#5fe"))) t)
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
 '(w3m-tab-unselected-unseen ((t (:background "Gray90" :foreground "Black")))))

(load "knu-hooks.el")
(load "knu-lisp.el")
;;(sml/setup)
(flyspell-lazy-mode)
;; This file must be created and pointing to the apropriate file.
;; It may contain e.g. (load "knu-tablet.el") or (load "knu-desktop.el") etc.
(load "knu-device.el" t)
(eshell)
;;(w3m)

