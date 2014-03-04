(define-key global-map [?\A-k] (kbd "<prior>"))
(define-key global-map [?\A-K] (kbd "<S-prior>"))
(define-key global-map [?\A-u] (kbd "<backspace>"))
(define-key global-map [?\A-ü] (kbd "<up>"))
(define-key global-map [?\A-Ü] (kbd "<S-up>"))
(define-key global-map [?\A-.] (kbd "<delete>"))
(define-key global-map [?\A-ä] (kbd "<next>"))
(define-key global-map [?\A-Ä] (kbd "<S-next>"))
(define-key global-map [?\A-h] (kbd "<home>"))
(define-key global-map [?\A-H] (kbd "<S-home>"))
(define-key global-map [?\A-i] (kbd "<left>"))
(define-key global-map [?\A-I] (kbd "<S-left>"))
(define-key global-map [?\A-\C-i] 'left-word)
(define-key global-map [?\A-\C-a] 'right-word)
(define-key global-map [?\A-\C-u] (kbd "<C-backspace>"))
(define-key global-map [?\A-\C-.] (kbd "<C-delete>"))
(define-key global-map [?\A-\C-H] (kbd "<C-S-home>"))
(define-key global-map [?\A-\C-h] (kbd "<C-home>"))
(define-key global-map [?\A-\C-O] (kbd "<C-S-end>"))
(define-key global-map [?\A-\C-o] (kbd "<C-end>"))
(define-key global-map [?\A-\C-Ü] (kbd "<C-S-up>"))
(define-key global-map [?\A-\C-ü] (kbd "<C-up>"))
(define-key global-map [?\A-\C-E] (kbd "<C-S-down>"))
(define-key global-map [?\A-\C-e] (kbd "<C-down>"))
(define-key global-map (kbd "C-x A-i") 'previous-buffer)
(define-key global-map (kbd "C-x A-a") 'next-buffer)
(define-key global-map [?\A-e] (kbd "<down>"))
(define-key global-map [?\A-E] (kbd "<S-down>"))
(define-key global-map [?\A-a] (kbd "<right>"))
(define-key global-map [?\A-A] (kbd "<S-right>"))
(define-key global-map [?\A-o] (kbd "<end>"))
(define-key global-map [?\A-O] (kbd "<S-end>"))
(define-key global-map [?\A-x] (kbd "ESC"))
(define-key global-map [?\A-y] (kbd "<tab>"))
;(define-key global-map [?\A-Y] (lambda nil (interactive) (hs-org/hideshow-all [(shift tab)])))
(define-key global-map [?\A-Y] (kbd "<S-tab>"))
(define-key global-map [?\A-ö] 'overwrite-mode)
(define-key global-map [?\A-q] 'undo)
(define-key global-map [?\A-,] (kbd "<return>"))
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
