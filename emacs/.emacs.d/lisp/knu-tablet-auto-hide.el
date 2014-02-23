(setq default-frame-alist (append default-frame-alist '((minibuffer . nil))))
(setq minibuffer-auto-raise t)
(setq minibuffer-frame-alist (quote ((width . 150) (top . 0) (height . 1) (left-fringe . 0) (right-fringe . 0) (background-color . "#303") (foreground-color . "#eee"))))

(run-with-idle-timer 0.5 t '(lambda () (if (equal (current-message) nil) (raise-frame) ())))
(add-hook 'post-command-hook '(lambda () (when (equal (current-message) "predictive") (message "") (raise-frame))))

(run-with-idle-timer 1 nil 'select-frame-by-name "emacs@localhost")

(add-hook 'exit-minibuffer-hook 'raise-frame)
(add-hook 'echo-area-clear-hook 'raise-frame)

(add-hook 'activate-mark-hook '(lambda () 
                             (set-face-attribute 'mode-line nil :height 1.0)
                             (set-face-background 'mode-line "#022")
                             (set-face-background 'mode-line-inactive "#404045")
                             ))
(add-hook 'deactivate-mark-hook '(lambda () 
                             (set-face-attribute 'mode-line nil :height 5)                          
                             (set-face-background 'mode-line "#588")
                             (set-face-background 'mode-line-inactive "#445")
                             ))
(add-hook 'minibuffer-setup-hook '(
                             lambda () 
                             (set-face-attribute 'mode-line nil :height 1.0)
                             (set-face-background 'mode-line "#022")
                             (set-face-background 'mode-line-inactive "#404045")
                             ))
(add-hook 'minibuffer-exit-hook '(lambda () 
                            (set-face-attribute 'mode-line nil :height 5)
                            (set-face-background 'mode-line "#588")
                            (set-face-background 'mode-line-inactive "#445")
                            ))

