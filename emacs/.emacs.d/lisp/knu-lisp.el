(defvar buffer-undo-list-tmp nil)

(defun auto-indent-sexps ()
  (save-excursion (paredit-indent-sexps)))

(defun paredit-del-and-join-forward (&optional arg)
  (interactive "P") 
  (if (and (eolp) (not (bolp)))
      (delete-indentation t)
    (paredit-forward-delete arg)))

(defun paredit-kill-and-join-forward (&optional arg)
  (interactive "P") 
  (if (and (eolp) (not (bolp)))
      (delete-indentation t)
    (paredit-kill arg)))

(defun paredit-del-backward-and-join (&optional arg)
  (interactive "P") 
  (if (looking-back "\\(^ *\\)")
      (delete-indentation)
    (paredit-backward-delete arg)))

(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'eshell-mode-hook 'paredit-mode)
(add-hook 'post-command-hook '(lambda () (when (or (equal major-mode 'emacs-lisp-mode)
                                             (equal major-mode 'lisp-interaction-mode)) 
                                      (when (not (equal buffer-undo-list-tmp buffer-undo-list))
                                        (auto-indent-sexps)
                                        (setq buffer-undo-list-tmp buffer-undo-list)))))
(add-hook 'paredit-mode-hook '(lambda () (define-key paredit-mode-map (kbd "C-k") 'paredit-kill-and-join-forward)
                                (define-key paredit-mode-map (kbd "<delete>") 'paredit-del-and-join-forward)
                                (define-key paredit-mode-map (kbd "<backspace>") 'paredit-del-backward-and-join)))

(define-key lisp-interaction-mode-map (kbd "<tab>") 'completion-at-point)
(define-key emacs-lisp-mode-map (kbd "<tab>") 'completion-at-point)
(define-key lisp-interaction-mode-map (kbd "<RET>") 'paredit-newline)
(define-key emacs-lisp-mode-map (kbd "<RET>") 'paredit-newline)

(define-key lisp-interaction-mode-map (kbd "<C-backspace>") 'paredit-backward-kill-word)
(define-key emacs-lisp-mode-map (kbd "<C-backspace>") 'paredit-backward-kill-word)
(define-key lisp-interaction-mode-map (kbd "<C-delete>") 'paredit-forward-kill-word)
(define-key emacs-lisp-mode-map (kbd "<C-delete>") 'paredit-forward-kill-word)

