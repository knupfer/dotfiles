(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(add-to-list 'load-path "~/.emacs.d/lisp/")
(setq split-height-threshold nil)
(setq split-width-threshold 0)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(setq org-log-done t)
 
(require 'org-install)
(require 'hideshow-org)

(defvar hcz-set-cursor-color-color "")
(defvar hcz-set-cursor-color-buffer "")
(defvar knu/modified nil)
 
(defun knu/publish () 
  "Runs my script, which does a bit cosmetic and cleanup."
  (eshell-command "sh ~/git/knupfer.github.io/_org/publish.sh"))
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
(defun knu/mode-line ()
  (interactive)
  (setq mode-line-format nil))
(defun knu/modify () 
  (interactive)
  (if (equal knu/modified nil)
(set-face-background 'mode-line "#033")
(set-face-background 'mode-line "#500")
                            ))

(load "pretty-symbols.el")
(load "highlight-parentheses.el")

;; Correct the layout for a tablet.
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cm" 'magit-status)
(global-set-key [f12] 'toggle-mode-line)

;; hooks
(add-hook 'after-change-major-mode-hook '(lambda () 
                                           (highlight-parentheses-mode)
                                           ))
(add-hook 'find-file-hook '(lambda ()
                       (hs-hide-all)
                       ))
(add-hook 'ess-mode-hook '(lambda () 
                            (hs-org/minor-mode t)
                         ;   (hs-hide-all)
                            (whitespace-mode)
                            (pretty-symbols-mode)
                            ))
(add-hook 'LilyPond-mode-hook '(lambda () 
                                 (hs-org/minor-mode t)
                                 (highlight-parentheses-mode)
                                 (whitespace-mode)
                                 ))
(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)
(add-hook 'org-mode-hook '(lambda ()
                            (flyspell-mode)
                            (define-key org-mode-map (kbd "C-c C-x a") 'knu/org-archive)
                            ))
(add-hook 'post-command-hook 'hcz-set-cursor-color-according-to-mode)
(add-hook 'activate-mark-hook '(lambda () 
                             (set-face-attribute 'mode-line nil :height 1.0)
                             ))
(add-hook 'deactivate-mark-hook '(lambda () 
                             (set-face-attribute 'mode-line nil :height 5)))
(add-hook 'minibuffer-setup-hook '(lambda () 
                             (set-face-attribute 'mode-line nil :height 1.0)))
(add-hook 'minibuffer-exit-hook '(lambda () 
                            (set-face-attribute 'mode-line nil :height 5)))
;(add-hook 'deactivate-mark-hook '(lambda () 
;                            (set-face-attribute 'mode-line nil :height 5)
;                            (knu/modify)
;                            ))

(add-hook 'prog-mode-hook '(lambda () 
                             (hs-org/minor-mode t)
                             (pretty-symbols-mode)
                             ))
(add-hook 'python-mode-hook '(lambda ()
                               (whitespace-mode)
                               ))
(add-hook 'text-mode-hook 'pretty-symbols-mode)
(add-hook 'w3m-mode-hook '(lambda ()
                            (load "w3m-config.el")
                            ))

(add-hook 'first-change-hook '(lambda () (setq-local knu/modified t)))

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
 '(minibuffer-auto-raise t)
 '(minibuffer-frame-alist (quote ((width . 150) (height . 2) (background-color . "#303") (foreground-color . "#eee"))))
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
 '(mode-line ((t (:background "#033" :foreground "#9bb" :box nil))))
 '(mode-line-buffer-id ((t (:foreground "#99dddd" :box nil :weight bold))))
 '(mode-line-inactive ((t (:inherit mode-line :background "gray32" :foreground "black" :box nil :weight light))))
 '(org-archived ((t (:foreground "#254555"))))
 '(org-checkbox ((t (:inherit bold :foreground "#2f2"))))
 '(org-hide ((t (:foreground "#777"))))
 '(org-indent ((t (:background "black" :foreground "black"))) t)
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

;; and or etc.
(add-to-list 'pretty-symbol-patterns '(8743 kdm-custom "\\<u?U?nd\\>\\|\\<a?A?nd\\>" (text-mode prog-mode ess-mode)))  
(add-to-list 'pretty-symbol-patterns '(8744 kdm-custom "\\<o?O?r\\>\\|\\<oder\\>\\|\\<Oder\\>" (text-mode prog-mode ess-mode)))  
(add-to-list 'pretty-symbol-patterns '(172 kdm-custom "\\<nicht\\>-?\\|\\<not\\>" (text-mode prog-mode ess-mode)))
(add-to-list 'pretty-symbol-patterns '(?∀ kdm-custom "\\<a?A?lle.?\\>" (text-mode prog-mode ess-mode)))
(add-to-list 'pretty-symbol-patterns '(?∃ kdm-custom "\\<E?e?s gibt\\>\\|\\<m?M?anche\\>\\|\\<e?E?inige\\>" (text-mode prog-mode ess-mode)))
(add-to-list 'pretty-symbol-patterns '(?Ø kdm-custom "\\<d?D?urchschnittliche?.?\\>" (text-mode prog-mode ess-mode)))

;; org-mode
(add-to-list 'pretty-symbol-patterns '(?α kdm-custom "#\\+BEGIN_SRC" (org-mode)))
(add-to-list 'pretty-symbol-patterns '(?β kdm-custom "#\\+END_SRC" (org-mode)))
(add-to-list 'pretty-symbol-patterns '(?▷ kdm-custom "- \\[ ]" (org-mode)))
(add-to-list 'pretty-symbol-patterns '(?> kdm-custom "- \\[-]" (org-mode)))
(add-to-list 'pretty-symbol-patterns '(?▶ kdm-custom "- \\[X]" (org-mode)))
(add-to-list 'pretty-symbol-patterns '(?δ kdm-custom "#\\+BEGIN_COMMENT" (org-mode)))
(add-to-list 'pretty-symbol-patterns '(?ξ kdm-custom "#\\+END_COMMENT" (org-mode)))

;; functions
(add-to-list 'pretty-symbol-patterns '(402 kdm-custom "\\<function\\>\\|\\<defun\\>\\|\\<def\\>" (text-mode prog-mode ess-mode)))
(add-to-list 'pretty-symbol-patterns '(?✠ kdm-custom "\\<return\\>\\|..RESULTS" (text-mode prog-mode ess-mode)))
(add-to-list 'pretty-symbol-patterns '(?∅ kdm-custom "\\<NULL\\>\\|\\<nil\\>\\|\\<None\\>" (text-mode prog-mode ess-mode))) 
(add-to-list 'pretty-symbol-patterns '(?∀ kdm-custom "\\<for\\>" (text-mode prog-mode ess-mode))) 
(add-to-list 'pretty-symbol-patterns '(?⬊ kdm-custom "\\<if\\>" (text-mode prog-mode ess-mode))) 
(add-to-list 'pretty-symbol-patterns '(?↯ kdm-custom "\\<else\\>" (text-mode prog-mode ess-mode))) 
(add-to-list 'pretty-symbol-patterns '(?↺ kdm-custom "\\<while\\>" (text-mode prog-mode ess-mode))) 
(add-to-list 'pretty-symbol-patterns '(?⚓ kdm-custom "hook" (text-mode prog-mode ess-mode))) 
(add-to-list 'pretty-symbol-patterns '(?⛁ kdm-custom "\\<library\\>" (text-mode prog-mode ess-mode))) 
(add-to-list 'pretty-symbol-patterns '(?∵ kdm-custom "\\<weil\\>\\|\\<wegen\\>\\|\\<because\\>" (text-mode prog-mode ess-mode))) 
(add-to-list 'pretty-symbol-patterns '(?∴ kdm-custom "\\<deswegen\\>\\|\\<therefore\\>\\|\\<folglich\\>\\|\\<also\\>" (text-mode prog-mode ess-mode))) 


;U+2234 (8756) ∴      THEREFORE                       folglich                                     
;U+2235 (8757) ∵∵      BECAUSE                         weil                                         


;;  ⬀ | ⬁ | ⬂ | ⬃ | ⬄ | ⬅ | ⬆ | ⬇ | ⬈ | ⬉ | ⬊ | ⬋ | ⬌ | ⬍ | ⬎ | ⬏ | ∄

;; superscripts                                      
(add-to-list 'pretty-symbol-patterns '(?² kdm-custom "\\*\\*2" (python-mode inferior-python-mode)))      
(add-to-list 'pretty-symbol-patterns '(?³ kdm-custom "\\*\\*3" (python-mode inferior-python-mode)))      
(add-to-list 'pretty-symbol-patterns '(?ⁿ kdm-custom "\\*\\*n" (python-mode inferior-python-mode)))      

;; subscripts
(add-to-list 'pretty-symbol-patterns '(?₀ kdm-custom "_0\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode)))        
(add-to-list 'pretty-symbol-patterns '(?₁ kdm-custom "_1\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode)))        
(add-to-list 'pretty-symbol-patterns '(?₂ kdm-custom "_2\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode)))        
(add-to-list 'pretty-symbol-patterns '(?₃ kdm-custom "_3\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode)))
(add-to-list 'pretty-symbol-patterns '(?₄ kdm-custom "_4\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode)))        
;; (add-to-list 'pretty-symbol-patterns '(?₅ kdm-custom "_5\\>" (python-mode inferior-python-mode)))     
;; (add-to-list 'pretty-symbol-patterns '(?₆ kdm-custom "_6\\>" (python-mode inferior-python-mode)))     
;; (add-to-list 'pretty-symbol-patterns '(?₇ kdm-custom "_7\\>" (python-mode inferior-python-mode)))     
;; (add-to-list 'pretty-symbol-patterns '(?₈ kdm-custom "_8\\>" (python-mode inferior-python-mode)))     
;; (add-to-list 'pretty-symbol-patterns '(?₉ kdm-custom "_9\\>" (python-mode inferior-python-mode)))     
(add-to-list 'pretty-symbol-patterns '(?ᵢ kdm-custom "_i\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode)))        
(add-to-list 'pretty-symbol-patterns '(?ⱼ kdm-custom "_j\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode)))        
(add-to-list 'pretty-symbol-patterns '(?ᵣ kdm-custom "_r\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode)))        
(add-to-list 'pretty-symbol-patterns '(?ᵤ kdm-custom "_u\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode)))        
(add-to-list 'pretty-symbol-patterns '(?ᵥ kdm-custom "_v\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode)))        
(add-to-list 'pretty-symbol-patterns '(?ᵪ kdm-custom "_x\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode)))        
(add-to-list 'pretty-symbol-patterns '(?ᵧ kdm-custom "_y\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode)))        
;; python specific
(add-to-list 'pretty-symbol-patterns '(?∑ kdm-custom "\\<sum\\>" (python-mode inferior-python-mode prog-mode ess-mode text-mode)))    
(add-to-list 'pretty-symbol-patterns '(?∑ kdm-custom "\\<nansum\\>" (python-mode inferior-python-mode))) 
(add-to-list 'pretty-symbol-patterns '(?√ kdm-custom "sqrt" (python-mode inferior-python-mode prog-mode text-mode ess-mode)))         
;; Greek
(add-to-list 'pretty-symbol-patterns '(?α kdm-custom "\\<alpha\\>" (fundamental-mode inferior-python-mode text-mode prog-mode ess-mode)))  
(add-to-list 'pretty-symbol-patterns '(?Α kdm-custom "\\<Alpha\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode)))  
(add-to-list 'pretty-symbol-patterns '(?β kdm-custom "\\<beta\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode)))   
(add-to-list 'pretty-symbol-patterns '(?Β kdm-custom "\\<Beta\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode)))   
(add-to-list 'pretty-symbol-patterns '(?γ kdm-custom "\\<gamma\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode)))  
(add-to-list 'pretty-symbol-patterns '(?Γ kdm-custom "\\<Gamma\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode)))  
(add-to-list 'pretty-symbol-patterns '(?δ kdm-custom "\\<delta\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode)))  
(add-to-list 'pretty-symbol-patterns '(?Δ kdm-custom "\\<Delta\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode)))  
(add-to-list 'pretty-symbol-patterns '(?ε kdm-custom "\\<epsilon\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode)))
(add-to-list 'pretty-symbol-patterns '(?Ε kdm-custom "\\<Epsilon\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode)))
(add-to-list 'pretty-symbol-patterns '(?ζ kdm-custom "\\<zeta\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode)))   
(add-to-list 'pretty-symbol-patterns '(?Ζ kdm-custom "\\<Zeta\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode)))   
(add-to-list 'pretty-symbol-patterns '(?η kdm-custom "\\<eta\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode)))    
(add-to-list 'pretty-symbol-patterns '(?Η kdm-custom "\\<Eta\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode)))    
(add-to-list 'pretty-symbol-patterns '(?θ kdm-custom "\\<theta\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode)))  
(add-to-list 'pretty-symbol-patterns '(?Θ kdm-custom "\\<Theta\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode)))  
(add-to-list 'pretty-symbol-patterns '(?ι kdm-custom "\\<iota\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode)))   
(add-to-list 'pretty-symbol-patterns '(?Ι kdm-custom "\\<Iota\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode)))   
(add-to-list 'pretty-symbol-patterns '(?κ kdm-custom "\\<kappa\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode)))  
(add-to-list 'pretty-symbol-patterns '(?K kdm-custom "\\<Kappa\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode)))  
(add-to-list 'pretty-symbol-patterns '(?λ kdm-custom "\\<lambda\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode))) 
(add-to-list 'pretty-symbol-patterns '(?Λ kdm-custom "\\<Lambda\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode))) 
(add-to-list 'pretty-symbol-patterns '(?μ kdm-custom "\\<mu\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode)))     
(add-to-list 'pretty-symbol-patterns '(?Μ kdm-custom "\\<Mu\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode)))     
(add-to-list 'pretty-symbol-patterns '(?ν kdm-custom "\\<nu\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode)))     
(add-to-list 'pretty-symbol-patterns '(?Ν kdm-custom "\\<Nu\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode)))     
(add-to-list 'pretty-symbol-patterns '(?ν kdm-custom "\\<vega\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode)))   
(add-to-list 'pretty-symbol-patterns '(?ν kdm-custom "\\<Vega\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode)))   
(add-to-list 'pretty-symbol-patterns '(?ξ kdm-custom "\\<xi\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode)))     
(add-to-list 'pretty-symbol-patterns '(?Ξ kdm-custom "\\<Xi\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode)))     
(add-to-list 'pretty-symbol-patterns '(?ο kdm-custom "\\<omicron\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode)))
(add-to-list 'pretty-symbol-patterns '(?Ο kdm-custom "\\<Omicron\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode)))
(add-to-list 'pretty-symbol-patterns '(?π kdm-custom "\\<pi\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode)))     
(add-to-list 'pretty-symbol-patterns '(?Π kdm-custom "\\<Pi\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode)))     
(add-to-list 'pretty-symbol-patterns '(?ρ kdm-custom "\\<rho\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode)))    
(add-to-list 'pretty-symbol-patterns '(?Ρ kdm-custom "\\<Rho\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode)))    
(add-to-list 'pretty-symbol-patterns '(?σ kdm-custom "\\<sigma\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode)))  
(add-to-list 'pretty-symbol-patterns '(?Σ kdm-custom "\\<Sigma\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode)))  
(add-to-list 'pretty-symbol-patterns '(?τ kdm-custom "\\<tau\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode)))    
(add-to-list 'pretty-symbol-patterns '(?Τ kdm-custom "\\<Tau\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode)))    
(add-to-list 'pretty-symbol-patterns '(?υ kdm-custom "\\<upsilon\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode)))
(add-to-list 'pretty-symbol-patterns '(?Y kdm-custom "\\<Upsilon\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode)))
(add-to-list 'pretty-symbol-patterns '(?φ kdm-custom "\\<phi\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode)))    
(add-to-list 'pretty-symbol-patterns '(?Φ kdm-custom "\\<Phi\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode)))    
(add-to-list 'pretty-symbol-patterns '(?χ kdm-custom "\\<chi\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode)))    
(add-to-list 'pretty-symbol-patterns '(?Χ kdm-custom "\\<Chi\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode)))    
(add-to-list 'pretty-symbol-patterns '(?ψ kdm-custom "\\<psi\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode)))    
(add-to-list 'pretty-symbol-patterns '(?Ψ kdm-custom "\\<Psi\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode)))    
(add-to-list 'pretty-symbol-patterns '(?ω kdm-custom "\\<omega\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode)))  
(add-to-list 'pretty-symbol-patterns '(?Ω kdm-custom "\\<Omega\\>" (python-mode inferior-python-mode text-mode prog-mode ess-mode)))   



;;; TEST from mastering emacs

;; use setq-default to set it for /all/ modes
(setq mode-line-format
  (list
    ;; the buffer name; the file name as a tool tip
    '(:eval (propertize "%b " 'face 'font-lock-keyword-face
        'help-echo (buffer-file-name)))

    ;; line and column
    "(" ;; '%02' to set to 2 chars at least; prevents flickering
      (propertize "%02l" 'face 'font-lock-type-face) ","
      (propertize "%02c" 'face 'font-lock-type-face) 
    ") "

    ;; relative position, size of file
    "["
    (propertize "%p" 'face 'font-lock-constant-face) ;; % above top
    "/"
    (propertize "%I" 'face 'font-lock-constant-face) ;; size
    "] "

    ;; the current major mode for the buffer.
    "["

    '(:eval (propertize "%m" 'face 'font-lock-string-face
              'help-echo buffer-file-coding-system))
    "] "

    "[" ;; insert vs overwrite mode, input-method in a tooltip
    '(:eval (propertize (if overwrite-mode "Ovr" "Ins")
              'face 'font-lock-preprocessor-face
              'help-echo (concat "Buffer is in "
                           (if overwrite-mode "overwrite" "insert") " mode")))

    ;; was this buffer modified since the last save?
    '(:eval (when (buffer-modified-p)
              (concat ","  (propertize "Mod"
                             'face 'font-lock-warning-face
                             'help-echo "Buffer has been modified"))))

    ;; is this buffer read-only?
    '(:eval (when buffer-read-only
              (concat ","  (propertize "RO"
                             'face 'font-lock-type-face
                             'help-echo "Buffer is read-only"))))  
    "] "

    ;; add the time, with the date and the emacs uptime in the tooltip
    '(:eval (propertize (format-time-string "%H:%M")
              'help-echo
              (concat (format-time-string "%c; ")
                      (emacs-uptime "Uptime:%hh"))))
    " --"
    ;; i don't want to see minor-modes; but if you want, uncomment this:
    ;; minor-mode-alist  ;; list of minor modes
    "%-" ;; fill with '-'
    ))

;; This file must be created and pointing to the apropriate file.
;; It may contain e.g. (load "knu-tablet.el") or (load "knu-desktop.el")...
(load "knu-device.el")
