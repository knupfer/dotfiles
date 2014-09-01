(setq gnus-select-method
      '(nnimap "gmail"
               (nnimap-address "imap.gmail.com")
               (nnimap-server-port 993)
               (nnimap-stream ssl)))

(add-to-list 'gnus-secondary-select-methods
             '(nntp "eternal september"
                    (nntp-address "reader443.eternal-september.org")
                    (nntp-authinfo-force t)))

(add-to-list 'gnus-secondary-select-methods
             '(nntp "gmane"
                    (nntp-address "news.gmane.org")))

(add-to-list 'gnus-secondary-select-methods
             '(nnimap "Musikschule"
                      (nnimap-address "secure.emailsrvr.com")
                      (nnimap-server-port 993)
                      (nnimap-stream ssl)))

(setq-default
 gnus-summary-mark-below -300
 gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references)

(setq gnus-face-5 'font-lock-comment-face)
(copy-face 'bold 'my-gnus-face-6)
(set-face-background 'my-gnus-face-6 "#333")
(set-face-foreground 'my-gnus-face-6 "#9ee")
(setq gnus-face-6 'my-gnus-face-6)

(copy-face 'default 'my-gnus-mouse-face-6)
(set-face-background 'my-gnus-mouse-face-6 "#993")
(setq gnus-mouse-face-6 'my-gnus-mouse-face-6)

(copy-face 'bold 'my-gnus-face-7)
(set-face-background 'my-gnus-face-7 "#333")
(set-face-foreground 'my-gnus-face-7 "#9ee")
(set-face-attribute 'my-gnus-face-7 nil :box '(:line-width -1 :color "#555"))
(setq gnus-face-7 'my-gnus-face-7)

(setq gnus-summary-thread-gathering-function 'gnus-gather-threads-by-subject
      gnus-thread-sort-functions '((not gnus-thread-sort-by-date))
      gnus-summary-line-format
      "%U%R%z %5{│%}%6{ %d %}%5{│%}%6( %-23,23f %)%5{│%}%* %5{%B%}%s\\n"
      gnus-sum-thread-tree-false-root " • "
      gnus-sum-thread-tree-indent " "
      gnus-sum-thread-tree-leaf-with-other "├─▶ "
      gnus-sum-thread-tree-root "• "
      gnus-sum-thread-tree-single-leaf "└─▶ "
      gnus-sum-thread-tree-vertical "│"
      gnus-group-line-format "%M%S%p%P%5y:%B%(%G%)\n"
      gnus-posting-styles '((message-news-p
                             (name "quxbam")
                             (address "no@news.invalid"))))

(setq gnus-use-adaptive-scoring '(word))

(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
(setq nnml-use-compressed-files t
      gnus-topic-display-empty-topics nil
      gnus-topic-line-format "%i%i%7{ %(%-12n%)%7A %}\n")
(add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)

(setq gnus-parameters
      '(("WIKI"
         (gnus-summary-line-format
          "%U%R %5{│%}%6{ %5,5i %}%5{│%}%* %-40,40f %5{│ %s%}\\n")
         (gnus-article-sort-functions '(gnus-article-sort-by-author gnus-article-sort-by-subject gnus-article-sort-by-score))
         (gnus-show-threads nil))))


(eval-after-load "mm-decode"
  '(progn
     (add-to-list 'mm-discouraged-alternatives "text/html")
     (add-to-list 'mm-discouraged-alternatives "text/richtext")))
