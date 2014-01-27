;; (normal-erase-is-backspace-mode 1)

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(add-to-list 'load-path "~/.emacs.d/lisp/")

(require 'org-install)
(require 'hideshow-org)


(defun knu/publish () 
  "Runs my script, which does a bit cosmetic and cleanup."
  (eshell-command "sh publish.sh"))
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
          (let ((alltags (split-string (or (org-entry-get (point) "ALLTAGS") "") ":"))
                local inherited tag)
            (dolist (tag alltags)
              (if (get-text-property 0 'inherited tag)
                  (push tag inherited) (push tag local)))
            (dolist (tag local)
              (if (member tag inherited) (org-toggle-tag tag 'off)))))
       t nil))))




(load "highlight-parentheses.el")


;; Correct the layout for a tablet.
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map [?\A-k] 'cua-scroll-down)
(define-key global-map [?\A-u] [(backspace)])
(define-key global-map [?\A-ü] 'previous-line)
(define-key global-map [?\A-.] [(delete)])
(define-key global-map [?\A-ä] 'cua-scroll-up)
(define-key global-map [?\A-h] 'beginning-of-line)
(define-key global-map [?\A-i] 'backward-char)
(define-key global-map [?\A-\C-i] 'backward-word)
(define-key global-map [?\A-\C-a] 'right-word)
(define-key global-map [?\A-\C-u] 'backward-kill-word)
(define-key global-map [?\A-\C-.] 'kill-word)
(define-key global-map [?\A-\C-h] 'beginning-of-buffer)
(define-key global-map [?\A-\C-o] 'end-of-buffer)
(define-key global-map [?\A-\C-ü] 'backward-paragraph)
(define-key global-map [?\A-\C-e] 'forward-paragraph)
(define-key global-map (kbd "C-x A-i") 'previous-buffer)
(define-key global-map (kbd "C-x A-a") 'next-buffer)
(define-key global-map [?\A-e] 'next-line)
(define-key global-map [?\A-a] 'forward-char)
(define-key global-map [?\A-o] 'end-of-line)
(define-key global-map [?\A-x] (kbd "ESC"))
(define-key global-map [?\A-y] (kbd "TAB"))
(define-key global-map [?\A-Y] (lambda nil (interactive) (hs-org/hideshow-all [(shift tab)])))
(define-key global-map [?\A-ö] 'overwrite-mode)
(define-key global-map [?\A-q] 'undo)
(define-key global-map [?\A-,] (kbd "RET"))
(define-key global-map [?\A-g] "7")
(define-key global-map [?\A-c] "8")
(define-key global-map [?\A-l] "9")
(define-key global-map [?\A-z] "+")
(define-key global-map [?\A-f] "")
(define-key global-map [?\A-d] "")
(define-key global-map [?\A-t] "4")
(define-key global-map [?\A-r] "5")
(define-key global-map [?\A-n] "6")
(define-key global-map [?\A-s] "")
(define-key global-map [?\A-p] "1")
(define-key global-map [?\A-w] "2")
(define-key global-map [?\A-m] "3")
(define-key global-map [?\A-j] ";")
(define-key global-map [?\A-\s] "0")
;(define-key global-map [key-8465] 'undo)

(defvar hcz-set-cursor-color-color "")
(defvar hcz-set-cursor-color-buffer "")
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

;; hooks
(add-hook 'after-change-major-mode-hook '(lambda () 
                                           (highlight-parentheses-mode)
                                           ))
(add-hook 'ess-mode-hook '(lambda () 
                            (hs-org/minor-mode t)
                            (hs-hide-all)
                            (whitespace-mode)))
(add-hook 'LilyPond-mode-hook '(lambda () 
                                 (hs-org/minor-mode t)
                                 (hs-hide-all)
                                 (highlight-parentheses-mode)
                                 (whitespace-mode)))
(add-hook 'org-mode-hook '(lambda ()
                            (flyspell-mode)
                            (define-key org-mode-map (kbd "C-c C-x a") 'knu/org-archive)))
(add-hook 'post-command-hook 'hcz-set-cursor-color-according-to-mode)
(add-hook 'prog-mode-hook '(lambda () 
                             (hs-org/minor-mode t)
                             (hs-hide-all)))
(add-hook 'w3m-mode-hook '(lambda ()
                            (load "w3m-config.el")
                            ))
(add-hook 'python-mode-hook '(lambda ()
                               (whitespace-mode)))
(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

(setq split-height-threshold nil)
(setq split-width-threshold 0)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(setq org-log-done t)



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
 '(custom-enabled-themes (quote (deeper-blue)))
 '(display-time-mode t)
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
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(intitial-frame-alist (quote ((fullscreen . maximized))) t)
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
 '(org-publish-project-alist (quote (("Homepage" :base-directory "~/git/knupfer.github.io/_org/" :base-extension "org" :publishing-directory "~/git/knupfer.github.io/_processing/" :publishing-function org-html-publish-to-html :headline-levels 5 :body-only t :completion-function knu/publish))))
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
 '(scroll-bar-mode nil)
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
 '(whitespace-trailing-regexp "\\([^ *äöüßÄÖÜA-Za-z0-9]\\)")
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
 '(hc-hard-hyphen ((nil)))
 '(hc-hard-space ((nil)))
 '(hc-other-char ((t (:foreground "#22aaaa"))))
 '(hc-tab ((t nil)))
 '(hc-trailing-whitespace ((nil)))
 '(hl-paren-face ((t (:weight ultra-bold))) t)
 '(mode-line ((t (:background "gray38" :foreground "black" :box nil))))
 '(mode-line-buffer-id ((t (:foreground "#99dddd" :box nil :weight bold))))
 '(mode-line-inactive ((t (:inherit mode-line :background "gray32" :foreground "black" :box (:line-width 1 :color "gray30") :weight light))))
 '(org-archived ((t (:foreground "#254555"))))
 '(org-checkbox ((t (:inherit bold :foreground "#2f2"))))
 '(org-hide ((t (:foreground "#777"))))
 '(org-indent ((t (:background "black" :foreground "black"))) t)
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

(eshell)
