;;; num3-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (global-num3-mode num3-mode) "num3-mode" "num3-mode.el"
;;;;;;  (21307 56740 320000 3000))
;;; Generated autoloads from num3-mode.el

(autoload 'num3-mode "num3-mode" "\
Toggle num3 minor mode in the current buffer.
Num3 minor mode makes long numbers more readable by highlighting
groups of digits when font-lock mode is on.

If a number is longer than `num3-threshold', the mode will split
it into a group of `num3-group-size' (if number is decimal) or
4 (if number is hexadecimal) digits.  Hexadecimal number is
detected as one starting with 0x, 0X or #.

With decimal numbers, fractions are recognised as well and
grouped from the beginning rathar then from end.  For instance,
with group size of 3, a number \"12345.12345\" will be split into
groups as follows: \"12|345.123|45\".  Fractions without integer
part are also recognised, eg. \".12345\".

The groups are highlighted alternately using `num3-face-odd' and
`num3-face-even' faces.  `num3-face-odd' face (which is empty by
default) is the one used for the group closest to the decimal point,
ie. groups are counted starting with one outwards from the (place
where) decimal point (would be) is.

\(fn &optional ARG)" t nil)

(defvar global-num3-mode nil "\
Non-nil if Global-Num3 mode is enabled.
See the command `global-num3-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-num3-mode'.")

(custom-autoload 'global-num3-mode "num3-mode" nil)

(autoload 'global-num3-mode "num3-mode" "\
Toggle Num3 mode in all buffers.
With prefix ARG, enable Global-Num3 mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Num3 mode is enabled in all buffers where
`num3-mode' would do it.
See `num3-mode' for more information on Num3 mode.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("num3-mode-pkg.el") (21307 56740 446217
;;;;;;  2000))

;;;***

(provide 'num3-mode-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; num3-mode-autoloads.el ends here
