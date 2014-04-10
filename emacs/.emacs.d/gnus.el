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
      smtpmail-auth-credentials '(("smtp.gmail.com" 587 "fknupfer@gmail.com" nil))
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      starttls-use-gnutls t
      gnus-thread-sort-functions '((not gnus-thread-sort-by-date)))

(setq-default
 gnus-summary-line-format "%(%&user-date;%* %-12,12a%R%O%B%-0,59s%)\n"
  gnus-user-date-format-alist '((t . "%d-%m"))
  gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references
  gnus-sum-thread-tree-false-root ""
  gnus-sum-thread-tree-indent " "
  gnus-sum-thread-tree-leaf-with-other "->  "
  gnus-sum-thread-tree-root ""
  gnus-sum-thread-tree-single-leaf "\\  "
  gnus-sum-thread-tree-vertical "|")

(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

(setq gnus-summary-thread-gathering-function
      'gnus-gather-threads-by-subject)

;sehr gutes Nachschlagewerk:  http://blog.binchen.org/?p=403

(setq gnus-permanently-visible-groups ".*Gesendet")

(setq gnus-parameters
      '(("INBOX"
         (display . 41))
        (".*Gesendet"
         (display . all))))


