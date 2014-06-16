;;; smtpmail-multi-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (smtpmail-multi-send-it smtpmail-multi-get-accounts
;;;;;;  smtpmail-multi-change) "smtpmail-multi" "smtpmail-multi.el"
;;;;;;  (21402 7723 765009 40000))
;;; Generated autoloads from smtpmail-multi.el

(autoload 'smtpmail-multi-change "smtpmail-multi" "\
Change the smtp settings to match the settings for ACCOUNT in `smtpmail-multi-accounts'.

\(fn ACCOUNT)" nil nil)

(autoload 'smtpmail-multi-get-accounts "smtpmail-multi" "\
Returns the SMTP accounts associated with the current buffer according to `smtpmail-multi-associations'.
The account details associated with each account name are stored in `smtpmail-multi-accounts'.
If there is no SMTP account associated with the current buffer, return `smtpmail-multi-default-account'
instead.

\(fn)" nil nil)

(autoload 'smtpmail-multi-send-it "smtpmail-multi" "\
Send mail using smtp server selected by the `smtpmail-multi-select' function.

\(fn)" nil nil)

;;;***

;;;### (autoloads nil nil ("smtpmail-multi-pkg.el") (21402 7723 772335
;;;;;;  622000))

;;;***

(provide 'smtpmail-multi-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; smtpmail-multi-autoloads.el ends here
