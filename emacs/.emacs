(require 'package)                                                                         
(package-initialize)                                                                       

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'load-path "~/.emacs.d/predict/")
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

(require 'org-install)
(require 'hideshow-org)
(require 'ess-site)
(require 'predictive)

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
(defun switch-to-previous-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(load "pretty-symbols.el")

(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cm" 'magit-status)
(define-key global-map (kbd "`") 'switch-to-previous-buffer)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LilyPond-indent-level 4)
 '(auto-completion-syntax-alist (quote ((t accept . word))))
 '(blink-cursor-mode nil)
 '(c-default-style (quote ((c-mode . "stroustrup") (java-mode . "java") (awk-mode . "awk") (other . "gnu"))))
 '(column-number-mode t)
 '(completion-auto-show-delay (quote ((t . 5))))
 '(completion-max-candidates (quote ((t . 5))))
 '(completion-ui-use-echo nil)
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
 '(fit-frame-crop-end-blank-flag t)
 '(fit-frame-empty-special-display-width 10)
 '(fit-frame-empty-width 10)
 '(font-lock-global-modes (quote (not w3m-minor-mode)))
 '(font-use-system-font nil)
 '(fringe-mode (quote (0)) nil (fringe))
 '(global-whitespace-mode t)
 '(gnus-init-file "~/.emacs.d/gnus.el")
 '(hc-other-chars (quote ("~,.!?{}[]():;»«›‹-_/\\+&")))
 '(hc-other-chars-font-lock-override (quote keep))
 '(hfy-ignored-properties nil)
 '(hl-paren-colors (quote ("#05ffff" "#e07fef" "#f0cf05" "#ee5555" "#ffffff" "#00ff00")))
 '(hs-hide-comments-when-hiding-all nil)
 '(icicle-top-level-key-bindings (quote (([pause] icicle-switch-to/from-minibuffer t) ("`" icicle-search-generic t) ("$" icicle-search-word t) ("^" icicle-search-keywords t) ("=" icicle-imenu t) ("\"" icicle-search-text-property t) ("/" icicle-complete-thesaurus-entry (fboundp (quote icicle-complete-thesaurus-entry))) ([24 134217829] icicle-execute-named-keyboard-macro t) (" " icicle-command-abbrev t) ("5o" icicle-select-frame t) ("" icicle-describe-option-of-type t) ([S-f4] icicle-kmacro t) (abort-recursive-edit icicle-abort-recursive-edit t) (apropos icicle-apropos t) (apropos-command icicle-apropos-command t) (apropos-value icicle-apropos-value t) (apropos-variable icicle-apropos-option (fboundp (quote icicle-apropos-option))) (apropos-variable icicle-apropos-variable (not (fboundp (quote icicle-apropos-option)))) (apropos-zippy icicle-apropos-zippy t) (bookmark-jump icicle-bookmark t) (bookmark-jump-other-window icicle-bookmark-other-window t) (bmkp-bookmark-set-confirm-overwrite icicle-bookmark-cmd (fboundp (quote bmkp-bookmark-set-confirm-overwrite))) (bookmark-set icicle-bookmark-cmd t) (customize-apropos icicle-customize-apropos t) (customize-apropos-faces icicle-customize-apropos-faces t) (customize-apropos-groups icicle-customize-apropos-groups t) (customize-apropos-options icicle-customize-apropos-options t) (customize-face icicle-customize-face t) (customize-face-other-window icicle-customize-face-other-window t) (dabbrev-completion icicle-dabbrev-completion t) ([201326639] icicle-dispatch-C-M-/ t) (delete-window icicle-delete-window t) (delete-windows-for icicle-delete-window t) (dired icicle-dired t) (dired-other-window icicle-dired-other-window t) (exchange-point-and-mark icicle-exchange-point-and-mark t) (execute-extended-command icicle-execute-extended-command t) (find-file icicle-file t) (find-file-other-window icicle-file-other-window t) (find-file-read-only icicle-find-file-read-only t) (find-file-read-only-other-window icicle-find-file-read-only-other-window t) (insert-buffer icicle-insert-buffer t) (kill-buffer icicle-kill-buffer t) (kill-buffer-and-its-windows icicle-kill-buffer t) (load-library icicle-load-library (> emacs-major-version 20)) (minibuffer-keyboard-quit icicle-abort-recursive-edit (fboundp (quote minibuffer-keyboard-quit))) (other-window icicle-other-window-or-frame t) (other-window-or-frame icicle-other-window-or-frame t) (pop-global-mark icicle-goto-global-marker-or-pop-global-mark t) (repeat-complex-command icicle-repeat-complex-command t) (set-mark-command icicle-goto-marker-or-set-mark-command t) (switch-to-buffer icicle-buffer t) (switch-to-buffer-other-window icicle-buffer-other-window t) (where-is icicle-where-is t) (yank icicle-yank-maybe-completing t) (yank-pop icicle-yank-pop-commands (featurep (quote second-sel))) (yank-pop-commands icicle-yank-pop-commands (featurep (quote second-sel))) (zap-to-char icicle-zap-to-char (fboundp (quote read-char-by-name))) ("jt" icicle-find-file-tagged (featurep (quote bookmark+))) ("4jt" icicle-find-file-tagged-other-window (featurep (quote bookmark+))) (bmkp-autofile-set icicle-bookmark-a-file (fboundp (quote bmkp-bookmark-a-file))) (bmkp-tag-a-file icicle-tag-a-file (fboundp (quote bmkp-tag-a-file))) (bmkp-untag-a-file icicle-untag-a-file (fboundp (quote bmkp-untag-a-file))) (bmkp-find-file icicle-find-file-handle-bookmark (fboundp (quote bmkp-find-file))) (bmkp-find-file-other-window icicle-find-file-handle-bookmark-other-window (fboundp (quote bmkp-find-file-other-window))) (bmkp-autofile-jump icicle-bookmark-autofile (fboundp (quote bmkp-autofile-jump))) (bmkp-autofile-jump-other-window icicle-bookmark-autofile-other-window (fboundp (quote bmkp-autofile-jump))) (bmkp-autonamed-jump icicle-bookmark-autonamed (fboundp (quote bmkp-autonamed-jump))) (bmkp-autonamed-jump-other-window icicle-bookmark-autonamed-other-window (fboundp (quote bmkp-autonamed-jump))) (bmkp-autonamed-this-buffer-jump icicle-bookmark-autonamed-this-buffer (fboundp (quote bmkp-autonamed-this-buffer-jump))) (bmkp-bookmark-file-jump icicle-bookmark-bookmark-file (fboundp (quote bmkp-bookmark-file-jump))) (bmkp-bookmark-list-jump icicle-bookmark-bookmark-list (fboundp (quote bmkp-bookmark-list-jump))) (bmkp-desktop-jump icicle-bookmark-desktop (fboundp (quote bmkp-desktop-jump))) (bmkp-dired-jump icicle-bookmark-dired (fboundp (quote bmkp-dired-jump))) (bmkp-dired-jump-other-window icicle-bookmark-dired-other-window (fboundp (quote bmkp-dired-jump))) (bmkp-file-jump icicle-bookmark-file (fboundp (quote bmkp-file-jump))) (bmkp-file-jump-other-window icicle-bookmark-file-other-window (fboundp (quote bmkp-file-jump))) (bmkp-file-this-dir-jump icicle-bookmark-file-this-dir (fboundp (quote bmkp-file-this-dir-jump))) (bmkp-file-this-dir-jump-other-window icicle-bookmark-file-this-dir-other-window (fboundp (quote bmkp-file-this-dir-jump))) (bmkp-gnus-jump icicle-bookmark-gnus (fboundp (quote bmkp-gnus-jump))) (bmkp-gnus-jump-other-window icicle-bookmark-gnus-other-window (fboundp (quote bmkp-gnus-jump))) (bmkp-image-jump icicle-bookmark-image (fboundp (quote bmkp-image-jump))) (bmkp-image-jump-other-window icicle-bookmark-image-other-window (fboundp (quote bmkp-image-jump))) (bmkp-info-jump icicle-bookmark-info (fboundp (quote bmkp-info-jump))) (bmkp-info-jump-other-window icicle-bookmark-info-other-window (fboundp (quote bmkp-info-jump))) (bmkp-local-file-jump icicle-bookmark-local-file (fboundp (quote bmkp-local-file-jump))) (bmkp-local-file-jump-other-window icicle-bookmark-local-file-other-window (fboundp (quote bmkp-local-file-jump))) (bmkp-man-jump icicle-bookmark-man (fboundp (quote bmkp-man-jump))) (bmkp-man-jump-other-window icicle-bookmark-man-other-window (fboundp (quote bmkp-man-jump))) (bmkp-non-file-jump icicle-bookmark-non-file (fboundp (quote bmkp-non-file-jump))) (bmkp-non-file-jump-other-window icicle-bookmark-non-file-other-window (fboundp (quote bmkp-non-file-jump))) (bmkp-region-jump icicle-bookmark-region (fboundp (quote bmkp-region-jump))) (bmkp-region-jump-other-window icicle-bookmark-region-other-window (fboundp (quote bmkp-region-jump))) (bmkp-remote-file-jump icicle-bookmark-remote-file (fboundp (quote bmkp-remote-file-jump))) (bmkp-remote-file-jump-other-window icicle-bookmark-remote-file-other-window (fboundp (quote bmkp-remote-file-jump))) (bmkp-specific-buffers-jump icicle-bookmark-specific-buffers (fboundp (quote bmkp-specific-buffers-jump))) (bmkp-specific-buffers-jump-other-window icicle-bookmark-specific-buffers-other-window (fboundp (quote bmkp-specific-buffers-jump))) (bmkp-specific-files-jump icicle-bookmark-specific-files (fboundp (quote bmkp-specific-files-jump))) (bmkp-specific-files-jump-other-window icicle-bookmark-specific-files-other-window (fboundp (quote bmkp-specific-files-jump))) (bmkp-temporary-jump icicle-bookmark-temporary (fboundp (quote bmkp-temporary-jump))) (bmkp-temporary-jump-other-window icicle-bookmark-temporary-other-window (fboundp (quote bmkp-temporary-jump))) (bmkp-this-buffer-jump icicle-bookmark-this-buffer (fboundp (quote bmkp-this-buffer-jump))) (bmkp-this-buffer-jump-other-window icicle-bookmark-this-buffer-other-window (fboundp (quote bmkp-this-buffer-jump))) (bmkp-url-jump icicle-bookmark-url (fboundp (quote bmkp-url-jump))) (bmkp-url-jump-other-window icicle-bookmark-url-other-window (fboundp (quote bmkp-url-jump))) (bmkp-w3m-jump icicle-bookmark-w3m (fboundp (quote bmkp-w3m-jump))) (bmkp-w3m-jump-other-window icicle-bookmark-w3m-other-window (fboundp (quote bmkp-w3m-jump))) (bmkp-find-file-all-tags icicle-find-file-all-tags (fboundp (quote bmkp-find-file-all-tags))) (bmkp-find-file-all-tags-other-window icicle-find-file-all-tags-other-window (fboundp (quote bmkp-find-file-all-tags))) (bmkp-find-file-all-tags-regexp icicle-find-file-all-tags-regexp (fboundp (quote bmkp-find-file-all-tags-regexp))) (bmkp-find-file-all-tags-regexp-other-window icicle-find-file-all-tags-regexp-other-window (fboundp (quote bmkp-find-file-all-tags-regexp-other-window))) (bmkp-find-file-some-tags icicle-find-file-some-tags (fboundp (quote bmkp-find-file-some-tags))) (bmkp-find-file-some-tags-other-window icicle-find-file-some-tags-other-window (fboundp (quote bmkp-find-file-some-tags-other-window))) (bmkp-find-file-some-tags-regexp icicle-find-file-some-tags-regexp (fboundp (quote bmkp-find-file-some-tags-regexp))) (bmkp-find-file-some-tags-regexp-other-window icicle-find-file-some-tags-regexp-other-window (fboundp (quote bmkp-find-file-some-tags-regexp-other-window))) (bmkp-autofile-all-tags-jump icicle-bookmark-autofile-all-tags (fboundp (quote bmkp-autofile-all-tags-jump))) (bmkp-autofile-all-tags-jump-other-window icicle-bookmark-autofile-all-tags-other-window (fboundp (quote bmkp-autofile-all-tags-jump))) (bmkp-autofile-all-tags-regexp-jump icicle-bookmark-autofile-all-tags-regexp (fboundp (quote bmkp-autofile-all-tags-regexp-jump))) (bmkp-autofile-all-tags-regexp-jump-other-window icicle-bookmark-autofile-all-tags-regexp-other-window (fboundp (quote bmkp-autofile-all-tags-regexp-jump))) (bmkp-autofile-some-tags-jump icicle-bookmark-autofile-some-tags (fboundp (quote bmkp-autofile-some-tags-jump))) (bmkp-autofile-some-tags-jump-other-window icicle-bookmark-autofile-some-tags-other-window (fboundp (quote bmkp-autofile-some-tags-jump))) (bmkp-autofile-some-tags-regexp-jump icicle-bookmark-autofile-some-tags-regexp (fboundp (quote bmkp-autofile-some-tags-regexp-jump))) (bmkp-autofile-some-tags-regexp-jump-other-window icicle-bookmark-autofile-some-tags-regexp-other-window (fboundp (quote bmkp-autofile-some-tags-regexp-jump))) (bmkp-all-tags-jump icicle-bookmark-all-tags (fboundp (quote bmkp-all-tags-jump))) (bmkp-all-tags-jump-other-window icicle-bookmark-all-tags-other-window (fboundp (quote bmkp-all-tags-jump))) (bmkp-all-tags-regexp-jump icicle-bookmark-all-tags-regexp (fboundp (quote bmkp-all-tags-regexp-jump))) (bmkp-all-tags-regexp-jump-other-window icicle-bookmark-all-tags-regexp-other-window (fboundp (quote bmkp-all-tags-regexp-jump))) (bmkp-some-tags-jump icicle-bookmark-some-tags (fboundp (quote bmkp-some-tags-jump))) (bmkp-some-tags-jump-other-window icicle-bookmark-some-tags-other-window (fboundp (quote bmkp-some-tags-jump))) (bmkp-some-tags-regexp-jump icicle-bookmark-some-tags-regexp (fboundp (quote bmkp-some-tags-regexp-jump))) (bmkp-some-tags-regexp-jump-other-window icicle-bookmark-some-tags-regexp-other-window (fboundp (quote bmkp-some-tags-regexp-jump))) (bmkp-file-all-tags-jump icicle-bookmark-file-all-tags (fboundp (quote bmkp-file-all-tags-jump))) (bmkp-file-all-tags-jump-other-window icicle-bookmark-file-all-tags-other-window (fboundp (quote bmkp-file-all-tags-jump))) (bmkp-file-all-tags-regexp-jump icicle-bookmark-file-all-tags-regexp (fboundp (quote bmkp-file-all-tags-regexp-jump))) (bmkp-file-all-tags-regexp-jump-other-window icicle-bookmark-file-all-tags-regexp-other-window (fboundp (quote bmkp-file-all-tags-regexp-jump))) (bmkp-file-some-tags-jump icicle-bookmark-file-some-tags (fboundp (quote bmkp-file-some-tags-jump))) (bmkp-file-some-tags-jump-other-window icicle-bookmark-file-some-tags-other-window (fboundp (quote bmkp-file-some-tags-jump))) (bmkp-file-some-tags-regexp-jump icicle-bookmark-file-some-tags-regexp (fboundp (quote bmkp-file-some-tags-regexp-jump))) (bmkp-file-some-tags-regexp-jump-other-window icicle-bookmark-file-some-tags-regexp-other-window (fboundp (quote bmkp-file-some-tags-regexp-jump))) (bmkp-file-this-dir-all-tags-jump icicle-bookmark-file-this-dir-all-tags (fboundp (quote bmkp-file-this-dir-all-tags-jump))) (bmkp-file-this-dir-all-tags-jump-other-window icicle-bookmark-file-this-dir-all-tags-other-window (fboundp (quote bmkp-file-this-dir-all-tags-jump))) (bmkp-file-this-dir-all-tags-regexp-jump icicle-bookmark-file-this-dir-all-tags-regexp (fboundp (quote bmkp-file-this-dir-all-tags-regexp-jump))) (bmkp-file-this-dir-all-tags-regexp-jump-other-window icicle-bookmark-file-this-dir-all-tags-regexp-other-window (fboundp (quote bmkp-file-this-dir-all-tags-regexp-jump))) (bmkp-file-this-dir-some-tags-jump icicle-bookmark-file-this-dir-some-tags (fboundp (quote bmkp-file-this-dir-some-tags-jump))) (bmkp-file-this-dir-some-tags-jump-other-window icicle-bookmark-file-this-dir-some-tags-other-window (fboundp (quote bmkp-file-this-dir-some-tags-jump))) (bmkp-file-this-dir-some-tags-regexp-jump icicle-bookmark-file-this-dir-some-tags-regexp (fboundp (quote bmkp-file-this-dir-some-tags-regexp-jump))) (bmkp-file-this-dir-some-tags-regexp-jump-other-window icicle-bookmark-file-this-dir-some-tags-regexp-other-window (fboundp (quote bmkp-file-this-dir-some-tags-regexp-jump))) (find-tag icicle-find-tag (fboundp (quote command-remapping))) (find-tag-other-window icicle-find-first-tag-other-window t) (pop-tag-mark icicle-pop-tag-mark (fboundp (quote command-remapping))) (eval-expression icicle-pp-eval-expression (fboundp (quote command-remapping))) (pp-eval-expression icicle-pp-eval-expression (fboundp (quote command-remapping))) ([S-f10] icicle-complete-menu-bar (fboundp (quote icicle-complete-menu-bar))) ([27 134217848] lacarte-execute-command (fboundp (quote lacarte-execute-command))) ([134217824] lacarte-execute-menu-command (fboundp (quote lacarte-execute-menu-command))) ([f10] lacarte-execute-menu-command (fboundp (quote lacarte-execute-menu-command))))))
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(ispell-highlight-face (quote flyspell-incorrect))
 '(ispell-local-dictionary "de_DE")
 '(ispell-program-name "aspell")
 '(mail-user-agent (quote gnus-user-agent))
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
 '(word-wrap t)
 '(x-gtk-use-system-tooltips nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#000000" :foreground "#ffffff" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 165 :width normal :foundry "unknown" :family "Source Code Pro"))))
 '(completion-highlight-face ((t (:background "#033" :foreground "#0ff" :weight ultra-bold))))
 '(completion-popup-tip-face ((t (:background "black" :foreground "#77d"))))
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
 '(outline-1 ((t (:inherit font-lock-function-name-face :foreground "SkyBlue1" :weight bold))))
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
(flyspell-lazy-mode)
(icicle-mode)

