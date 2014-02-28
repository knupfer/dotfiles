(defun auto-indent-sexps ()
  (save-excursion
    (paredit-indent-sexps)))
(add-hook 'emacs-lisp-mode-hook '(lambda () (paredit-mode)))
(add-hook 'eshell-mode-hook 'paredit-mode)
(add-hook 'post-command-hook '(lambda () (when (or (equal major-mode 'emacs-lisp-mode)
                                             (equal major-mode 'lisp-interaction-mode))
                                      (auto-indent-sexps))))
(define-key lisp-interaction-mode-map (kbd "<tab>") 'completion-at-point)
(define-key emacs-lisp-mode-map (kbd "<tab>") 'completion-at-point)
(define-key lisp-interaction-mode-map (kbd "<RET>") 'paredit-newline)
(define-key emacs-lisp-mode-map (kbd "<RET>") 'paredit-newline)
(define-key paredit-mode-map (kbd "C-k") 'paredit-kill-and-join-forward)
(define-key paredit-mode-map (kbd "<delete>") 'paredit-del-and-join-forward)

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


