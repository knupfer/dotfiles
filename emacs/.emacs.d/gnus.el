(setq gnus-select-method
      '(nnimap "gmail"
               (nnimap-address "imap.gmail.com")
               (nnimap-server-port 993)
               (nnimap-stream ssl)))

(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials '(("smtp.gmail.com" 587 "fknupfer@gmail.com" nil))
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      starttls-use-gnutls t)

(setq gnus-thread-sort-functions                                                           
      '((not gnus-thread-sort-by-date)
        (not gnus-thread-sort-by-number)))

(setq-default
  gnus-summary-line-format "%U%R%z %(%&user-date;  %-15,15f  %B%s%)\n"
  gnus-user-date-format-alist '((t . "%Y-%m-%d %H:%M"))
  gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references
  gnus-sum-thread-tree-false-root ""
  gnus-sum-thread-tree-indent ""
  gnus-sum-thread-tree-leaf-with-other "-> "
  gnus-sum-thread-tree-root ""
  gnus-sum-thread-tree-single-leaf "|_ "
  gnus-sum-thread-tree-vertical "|")


(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

(setq gnus-summary-thread-gathering-function
      'gnus-gather-threads-by-subject)

(setq user-full-name "Florian Knupfer"
      user-mail-address "fknupfer@gmail.com"
      message-generate-headers-first t
      )

;(setq gnus-posting-styles
;      '((".*"
;     (name "My Name"
;          (address "username@gmail.com"
;                   (organization "")
;                   (signature-file "~/.signature")
;                   ("X-Troll" "Emacs is better than Vi")
;                   )))))

;(setq mm-text-html-renderer 'w3m)

;sehr gutes Nachschlagewerk:  http://blog.binchen.org/?p=403
