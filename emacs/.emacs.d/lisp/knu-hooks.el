(add-hook 'kill-emacs-hook '(lambda () (when (fboundp 'gnus-group-exit)
                                         (defun gnus-y-or-n-p (yes) yes)
                                         (gnus-group-exit))))
(add-hook 'ibuffer-mode-hook 'ibuffer-auto-mode)
(add-hook 'after-change-major-mode-hook '(lambda ()
                                           (highlight-parentheses-mode)))
(add-hook 'LilyPond-mode-hook '(lambda () (highlight-parentheses-mode)
                                 (lilypond-pretty-beat-mode)))
(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)
(add-hook 'org-mode-hook '(lambda () (flyspell-mode)
                            (auto-fill-mode)
                            (num3-mode)
                            (whitespace-mode)
                            (pretty-symbols-mode)
                            (when (not (boundp 'knu-org-mode-map))
                              (define-key org-mode-map
                                (kbd "C-c C-x a") 'knu/org-archive)
                              (load "knu-org-mode-map.el"))))
(add-hook 'post-command-hook 'hcz-set-cursor-color-according-to-mode)
(add-hook 'prog-mode-hook '(lambda () (num3-mode)
                             (whitespace-mode)
                             (indentation-tree-mode)
			     (hs-minor-mode)))
(add-hook 'w3m-mode-hook '(lambda () (load "w3m-config.el")))
(add-hook 'emacs-lisp-mode-hook 'hdefd-highlight-mode 'APPEND)
