;;; pretty-sha-path-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "pretty-sha-path" "pretty-sha-path.el" (21859
;;;;;;  8350 900234 811000))
;;; Generated autoloads from pretty-sha-path.el

(autoload 'pretty-sha-path-mode "pretty-sha-path" "\
Toggle Pretty SHA Path mode.

With a prefix argument ARG, enable Pretty SHA Path mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil.

When Pretty SHA Path mode is enabled, SHA-parts of the Guix/Nix
store paths (see `pretty-sha-path-regexp') are prettified,
i.e. displayed as `pretty-sha-path-char' character.  This mode
can be enabled programmatically using hooks:

  (add-hook 'shell-mode-hook 'pretty-sha-path-mode)

It is possible to enable the mode in any buffer, however not any
buffer's highlighting may survive after adding new elements to
`font-lock-keywords' (see `pretty-sha-path-special-modes' for
details).

Also you can use `global-pretty-sha-path-mode' to enable Pretty
SHA Path mode for all modes that support font-locking.

\(fn &optional ARG)" t nil)

(defvar global-pretty-sha-path-mode nil "\
Non-nil if Global-Pretty-Sha-Path mode is enabled.
See the command `global-pretty-sha-path-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-pretty-sha-path-mode'.")

(custom-autoload 'global-pretty-sha-path-mode "pretty-sha-path" nil)

(autoload 'global-pretty-sha-path-mode "pretty-sha-path" "\
Toggle Pretty-Sha-Path mode in all buffers.
With prefix ARG, enable Global-Pretty-Sha-Path mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Pretty-Sha-Path mode is enabled in all buffers where
`pretty-sha-path-turn-on' would do it.
See `pretty-sha-path-mode' for more information on Pretty-Sha-Path mode.

\(fn &optional ARG)" t nil)

(defalias 'pretty-sha-path-global-mode 'global-pretty-sha-path-mode)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; pretty-sha-path-autoloads.el ends here
