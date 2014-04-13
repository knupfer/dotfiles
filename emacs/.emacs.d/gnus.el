(setq user-full-name "Florian Knupfer"
      user-mail-address "fknupfer@gmail.com"
      message-generate-headers-first t)

(setq gnus-select-method
      '(nnimap "gmail"
               (nnimap-address "imap.gmail.com")
               (nnimap-server-port 993)
               (nnimap-stream ssl)))

(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials '(("smtp.gmail.com" 587
                                   "fknupfer@gmail.com" nil))
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      starttls-use-gnutls t)

(add-to-list 'gnus-secondary-select-methods
             '(nntp "eternal september"
                    (nntp-address "news.eternal-september.org")))

(add-to-list 'gnus-secondary-select-methods
             '(nntp "gmane"
                    (nntp-address "news.gmane.org")))

(add-to-list 'gnus-secondary-select-methods
             '(nnimap "Musikschule"
                      (nnimap-address "secure.emailsrvr.com")
                      (nnimap-server-port 993)
                      (nnimap-stream ssl)))

(setq-default
 gnus-summary-mark-below -100
 gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references)

(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
(setq gnus-face-5 'font-lock-comment-face)
(copy-face 'bold 'my-gnus-face-6)
(copy-face 'default 'my-gnus-mouse-face-6)
(set-face-foreground 'my-gnus-face-6 "#9ee")
(set-face-background 'my-gnus-face-6 "#333")
(set-face-background 'my-gnus-mouse-face-6 "#993")
(setq gnus-face-6 'my-gnus-face-6)
(setq gnus-mouse-face-6 'my-gnus-mouse-face-6)

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
      gnus-group-line-format "%M%S%p%P%5y:%B%(%G%)
")

(setq gnus-use-adaptive-scoring t)

(setq gnus-permanently-visible-groups ".*Gesendet")

(setq gnus-parameters
      '((".*Gesendet"
         (display . all))))
