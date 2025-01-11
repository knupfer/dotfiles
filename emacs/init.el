;;; init.el --- Summary
;;; Commentary:
;;; Code:
(require 'flycheck)
(require 'org)
(require 'org-indent)
(require 'ob-haskell)
(require 'magit)
(require 'avy)
(require 'gnus)
(require 'gnus-sum)
(require 'gnus-topic)
(require 'ligature)
(require 'bbdb)

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

(use-package gptel
  :init
  (setq
   gptel-model 'qwen2.5:14b
   gptel-backend (gptel-make-ollama "Ollama"
                   :host "localhost:11434"
                   :stream t
                   :models '(qwen2.5:14b))))

(mapc (lambda (x) (define-key global-map (kbd (car x)) (cadr x)))
      '(("<M-left>" backward-sentence)
        ("<M-right>" forward-sentence)
        ("<M-backspace>" backward-kill-sentence)
        ("<M-delete>" kill-sentence)
        ("<C-prior>" beginning-of-buffer)
        ("<C-next>" end-of-buffer)
        ("C-c m" magit-status)
        ("C-a" gptel-send)
	("M-a" avy-goto-word-1)))

(setq org-log-done 'time
      org-startup-folded t
      org-startup-indented t
      org-indent-mode-turns-on-hiding-stars nil
      org-indent-indentation-per-level 1)

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
