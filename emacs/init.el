;;; init.el --- Summary
;;; Commentary:
;;; Code:
(require 'flycheck)
(require 'org)
(require 'org-indent)
(require 'magit)
(require 'avy)

(load-theme 'modus-vivendi)
(setq-default inhibit-startup-screen t
              truncate-lines t
	      auto-save-default nil
	      epg-pinentry-mode 'loopback)
(add-to-list 'default-frame-alist
               '(font . "Iosevka Extended:size=14"))
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

(mapc (lambda (x) (define-key global-map (kbd (car x)) (cadr x)))
      '(("<M-left>" backward-sentence)
        ("<M-right>" forward-sentence)
        ("<M-backspace>" backward-kill-sentence)
        ("<M-delete>" kill-sentence)
        ("<C-prior>" beginning-of-buffer)
        ("<C-next>" end-of-buffer)
        ("\C-cm" magit-status)
        ("\C-a" avy-goto-word-1)))

(setq org-log-done 'time
      org-startup-folded t
      org-startup-indented t
      org-indent-mode-turns-on-hiding-stars nil
      org-indent-indentation-per-level 1)

(defalias 'yes-or-no-p 'y-or-n-p)
(setq custom-file "/dev/null")
(server-stop-automatically 'delete-frame)

(provide 'init)
;;; init.el ends here
