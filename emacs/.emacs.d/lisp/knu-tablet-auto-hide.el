(setq default-frame-alist (append default-frame-alist '((minibuffer . nil))))
(setq minibuffer-auto-raise t)
(setq minibuffer-frame-alist (quote ((width . 150) (height . 2) (background-color . "#303") (foreground-color . "#eee"))))

(run-with-idle-timer 0.5 t '(lambda () (if (equal (current-message) nil) (raise-frame) ())))
(add-hook 'post-command-hook '(lambda () (when (equal (current-message) "predictive") (message "") (raise-frame))))

(run-with-idle-timer 1 nil 'select-frame-by-name "emacs@localhost")

(add-hook 'exit-minibuffer-hook 'raise-frame)
(add-hook 'echo-area-clear-hook 'raise-frame)

