;;; init.el --- Summary
;;; Commentary:
;;; Code:
(require 'flycheck)
(require 'org)
(require 'org-indent)
(require 'ob-haskell)
(require 'ob-lilypond)
(require 'magit)
(require 'avy)
(require 'gnus)
(require 'gnus-sum)
(require 'gnus-topic)
(require 'ligature)
(require 'bbdb)
(require 'gptel)
(require 'gptel-ollama)
(require 'org-inline-pdf)

(bbdb-initialize 'gnus 'message)

(load-theme 'modus-vivendi)
(setq-default inhibit-startup-screen t
              truncate-lines t
	      auto-save-default nil
	      epg-pinentry-mode 'loopback)
(add-to-list 'default-frame-alist
               '(font . "Iosevka:size=16"))
(set-face-attribute 'mode-line nil
                      :background "#033"
                      :box nil)
(set-face-attribute 'mode-line-inactive nil
                      :weight 'light
                      :box nil)
(set-face-attribute 'fringe nil
                      :background "black"
                      :foreground "#0ff")

(scroll-bar-mode -1)
(fringe-mode '(0 . nil))
(column-number-mode)
(menu-bar-mode -1)
(tool-bar-mode -1)
(indent-tabs-mode -1)
(blink-cursor-mode -1)

(global-flycheck-mode)

(autoload 'LilyPond-mode "lilypond-mode" "LilyPond Editing Mode" t)
(add-to-list 'auto-mode-alist '("\\.ly\\'" . LilyPond-mode))
(add-to-list 'auto-mode-alist '("\\.ily\\'" . LilyPond-mode))
(add-hook 'LilyPond-mode-hook (lambda () (turn-on-font-lock)))
(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)
(add-hook 'org-mode-hook #'org-inline-pdf-mode)
(setq org-image-max-width 'window)
(setq org-startup-with-inline-images t)
(setq org-startup-with-latex-preview t)

(use-package gptel
  :init
  (setq
   gptel-model 'qwen2.5:14b
   gptel-backend (gptel-make-ollama "Ollama"
                   :host "localhost:11434"
                   :stream t
                   :models '(qwen2.5:14b deepseek-r1:14b))
   gptel-default-mode 'org-mode
   gptel-prompt-prefix-alist
   '((markdown-mode . "# ")
     (org-mode . "* ")
     (text-mode . "# "))
   gptel-response-prefix-alist
   '((markdown-mode . "## Response

")
     (org-mode . "** Response

")
     (text-mode . "## Response

"))))

(setq org-babel-lilypond-paper-settings (concat org-babel-lilypond-paper-settings "
\\language \"deutsch\"
\\version \"2.24.4\"
\\paper {
  left-margin=4.2\\cm
  right-margin=4.2\\cm
  #(define fonts
    (make-pango-font-tree \"EB Garamond 12\"
                          \"Libertinus Sans\"
                          \"Iosevka\"
                          (/ staff-height pt 20)))
}
\\layout {
  \\context {
    \\Score
    \\omit BarNumber
    \\numericTimeSignature
  }
}
"))

(defun lilypond-fragment-path ()
  "Calculate the sha of the source."
  (let ((elem (org-element-at-point)))
    (concat (expand-file-name user-emacs-directory)
	    "lilypond/"
	    (sha1 (concat org-babel-lilypond-paper-settings
			  (org-element-property :value elem)))
	    ".pdf")))

(defun org-babel-execute:lilypond (body params)
  "Execute LilyPond src block according to arrange mode.
See `org-babel-execute-src-block' for BODY and PARAMS.  Check if cached
result already exists."
  (org-babel-lilypond-set-header-args org-babel-lilypond-arrange-mode)
  (if org-babel-lilypond-arrange-mode
      (org-babel-lilypond-tangle)
    (unless (file-exists-p (lilypond-fragment-path))
      (org-babel-lilypond-process-basic body params))))

(setq ob-lilypond-header-args
      `((:results . "file link replace")
	(:exports . "results")
        (:file . (lambda () (lilypond-fragment-path)))))
(org-babel-lilypond-set-header-args org-babel-lilypond-arrange-mode)

(defun gptel-stream-latex
    ()
  "Create LaTeX previews in a streaming fashion."
  (let ((beg (save-excursion
	       (gptel-beginning-of-response)
	       (point))))
    (org--latex-preview-region beg (point))))

(add-hook 'gptel-mode-hook 'visual-line-mode)
(add-hook 'gptel-post-stream-hook 'gptel-stream-latex)

(gptel "*scratch*")

(add-hook 'emacs-startup-hook (lambda () (goto-char (point-max))))

(mapc (lambda (x) (define-key global-map (kbd (car x)) (cadr x)))
      '(("<M-left>" backward-sentence)
        ("<M-right>" forward-sentence)
        ("<M-backspace>" backward-kill-sentence)
        ("<M-delete>" kill-sentence)
        ("<C-prior>" beginning-of-buffer)
        ("<C-next>" end-of-buffer)
        ("C-c m" magit-status)
	("M-a" avy-goto-word-1)))

(defvar knu/keys-keymap (make-keymap)
  "Keymap for my/keys-mode.")

(define-minor-mode knu/keys-mode
  "Minor mode for knu personal keybindings."
  :group 'misc
  :init-value t
  :global t
  :keymap knu/keys-keymap)

;; The keymaps in `emulation-mode-map-alists' take precedence over
;; `minor-mode-map-alist'
(add-to-list 'emulation-mode-map-alists
             `((knu/keys-mode . ,knu/keys-keymap)))

(define-key knu/keys-keymap (kbd "C-a") 'gptel-send)

(plist-put (cdr (assoc 'dvisvgm org-preview-latex-process-alist)) :latex-compiler '("lualatex --output-format=dvi --interaction=nonstopmode --output-directory=%o %f"))
(plist-put (plist-put (plist-put org-format-latex-options :foreground "#f0f") :background "Transparent") :scale 2)

(setq org-confirm-babel-evaluate (lambda (lang body) (not (string= lang "lilypond"))))

(setq org-log-done 'time
      org-startup-folded t
      org-startup-indented t
      org-indent-mode-turns-on-hiding-stars nil
      org-indent-indentation-per-level 1
      org-preview-latex-default-process 'dvisvgm
      org-latex-compiler "lualatex"
      org-preview-latex-image-directory (concat user-emacs-directory "latex/")
      org-highlight-latex-and-related '(latex)
      org-export-default-language "de"
      org-latex-packages-alist '( ("AUTO" "babel" t ("pdflatex" "xelatex" "lualatex"))
				  ("" "microtype" nil nil)
				  "
\\usepackage{rotating}
\\usepackage{unicode-math}
\\setmainfont
    [ Numbers           = {Proportional, OldStyle},
      Ligatures         = {Rare, Contextual, Historic, TeX},
      UprightFont       = *-Regular,
      ItalicFont        = *-Italic,
      BoldFont          = *-Medium,
      BoldItalicFont    = *-Medium Italic,
      Style             = Swash,
      Contextuals       = {Inner, WordInitial, WordFinal, LineFinal}
    ] {EB Garamond}
\\setmonofont{Iosevka}
\\setsansfont{Libertinus Sans}
\\setmathfont{Garamond Math}
\\setmathfont{Libertinus Math}[range={\"25A1}]
\\usepackage{siunitx}
\\sisetup{per-mode=fraction}
\\usepackage{array}
\\makeatletter
\\def\\maxwidth#1{\\ifdim\\Gin@nat@width>#1 #1\\else\\Gin@nat@width\\fi}
\\makeatother
")
      org-latex-image-default-width "\\maxwidth{\\linewidth}"
      org-export-with-toc nil)

(defalias 'yes-or-no-p 'y-or-n-p)
(setq custom-file "/dev/null")
(server-stop-automatically 'delete-frame)

(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

(setq gnus-select-method '(nnnil nil))
(setq gnus-summary-line-format "%R%O%* %&user-date;  %U  %B%(%-23,23f%)     %s\n")

(setq gnus-sum-thread-tree-root ""
      gnus-sum-thread-tree-false-root ""
      gnus-sum-thread-tree-vertical "│ "
      gnus-sum-thread-tree-indent "  "
      gnus-sum-thread-tree-leaf-with-other "├─"
      gnus-sum-thread-tree-single-leaf "└─")

(set-face-attribute 'gnus-summary-normal-ancient nil :foreground "#777")

(setq gnus-topic-line-format "%i%(%{%n%}%)%v\n")

(setq gnus-user-date-format-alist '((t . "%d %b %y   %H:%M")))
(setq gnus-parameters '((".*"  (gcc-self . t))))
(setq gnus-thread-sort-functions '(gnus-thread-sort-by-date))
(setq gnus-group-line-format "%P%3y: %(%G%)\n")
(defvar gnus-gcc-mark-as-read t)
(setq gnus-always-read-dribble-file t)

(use-package ligature
  :config
  ;; Enable all Iosevka ligatures in programming modes
  (ligature-set-ligatures 'prog-mode
			  '("-<<" "-<" "-<-" "<--" "<---" "<<-" "<-" "->" "->>" "-->" "--->" "->-" ">-" ">>-"
			    "=<<" "=<" "=<=" "<==" "<===" "<<=" "<=" "=>" "=>>" "==>" "===>" "=>=" ">=" ">>="
			    "<->" "<-->" "<--->" "<---->" "<=>" "<==>" "<===>" "<====>" "::" ":::" "__"
			    "<~" "<~~" "</" "</>" "/>" "~~>" "~>" "=="  "/=" "<>" "===" "=/="
			    "<:" ":=" ":-" ":+" "*=" "*+" "<*" "<*>" "*>" "<|" "<|>" "|>" "<." "<.>" ".>" "+*" "=*" "+:" "-:" "=:" ":>"
			    "[|" "|]" "++" "+++" "\\/" "/\\" "|-" "-|" "<<" ">>" ">>>" "<<<"
			    ))
  (global-ligature-mode t))

(provide 'init)
;;; init.el ends here
