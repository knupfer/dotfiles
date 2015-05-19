;;; oneonone-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "oneonone" "oneonone.el" (21851 9163 559024
;;;;;;  669000))
;;; Generated autoloads from oneonone.el

(let ((loads (get 'One-On-One 'custom-loads))) (if (member '"oneonone" loads) nil (put 'One-On-One 'custom-loads (cons '"oneonone" loads))))

(defvar 1on1-minibuffer-frame-flag t "\
*Non-nil means use a separate, specialized frame for the minibuffer.
Note that a non-nil value for this option also causes option
`pop-up-frames' to be set to `t'.  That is, it causes `display-buffer'
to generally use a separate frame.

If you change this variable, you will need to restart Emacs for it to
take effect.")

(custom-autoload '1on1-minibuffer-frame-flag "oneonone" t)

(defvar 1on1-remap-other-frame-command-flag (> emacs-major-version 23) "\
*Non-nil means rebind keys for `other-frame' to `1on1-other-frame'.
This has no effect unless `1on1-minibuffer-frame' is a frame, which
means that `1on1-minibuffer-frame-flag' is non-nil.
A non-nil value can be useful for Emacs starting with version 24,
because an inactive minibuffer has its own keymap.")

(custom-autoload '1on1-remap-other-frame-command-flag "oneonone" t)

(defvar 1on1-active-minibuffer-frame-background "PaleGoldenrod" "\
*The color of the `1on1-minibuffer-frame' when it is active.

Note: This is not used if `1on1-minibuffer-frame-flag' is nil.")

(custom-autoload '1on1-active-minibuffer-frame-background "oneonone" t)

(defvar 1on1-inactive-minibuffer-frame-background "LightBlue" "\
*The color of the `1on1-minibuffer-frame' when it is inactive.

Note: This is not used if `1on1-minibuffer-frame-flag' is nil.")

(custom-autoload '1on1-inactive-minibuffer-frame-background "oneonone" t)

(defvar 1on1-isearch-minibuffer-frame-background "bisque" "\
*Color of the `1on1-minibuffer-frame' when `isearch' is active.
See `1on1-color-isearch-minibuffer-frame'.

Note: This is not used if `1on1-minibuffer-frame-flag' is nil.")

(custom-autoload '1on1-isearch-minibuffer-frame-background "oneonone" t)

(defvar 1on1-color-mode-line-flag t "\
*Non-nil means use `1on1-(in)active-mode-line-background'.
If you change this variable, you will need to restart Emacs for it to
take effect.")

(custom-autoload '1on1-color-mode-line-flag "oneonone" t)

(defvar 1on1-color-minibuffer-frame-on-exit-increment 0.1 "\
*Increment to change minibuffer-frame hue when minibuffer is exited.
This should be opposite in sign to
`1on1-color-minibuffer-frame-on-setup-increment.'")

(custom-autoload '1on1-color-minibuffer-frame-on-exit-increment "oneonone" t)

(defvar 1on1-color-minibuffer-frame-on-setup-increment -0.1 "\
*Increment to change minibuffer-frame hue when minibuffer is entered.
This should be opposite in sign to
`1on1-color-minibuffer-frame-on-exit-increment.'")

(custom-autoload '1on1-color-minibuffer-frame-on-setup-increment "oneonone" t)

(defvar 1on1-active-mode-line-background "PaleGoldenrod" "\
*The color of the mode-line when it is active.
Note: This is not used if `1on1-color-mode-line-flag' is nil.")

(custom-autoload '1on1-active-mode-line-background "oneonone" t)

(defvar 1on1-inactive-mode-line-background "LightGray" "\
*The color of the mode-line when it is inactive.
Note: This is not used if `1on1-color-mode-line-flag' is nil.")

(custom-autoload '1on1-inactive-mode-line-background "oneonone" t)

(defvar 1on1-minibuffer-frame-left 0 "\
*Position of left edge of minibuffer frame, in pixels.
An integer.  If negative, then the position is that of the frame
bottom relative to the screen right (not left) edge.

See `default-frame-alist' for an explanation of frame parameters.

Note: This is not used if `1on1-minibuffer-frame-flag' is nil.

If you customize this variable, you will need to rerun `1on1-emacs'
for the new value to take effect.")

(custom-autoload '1on1-minibuffer-frame-left "oneonone" t)

(defvar 1on1-minibuffer-frame-top/bottom nil "\
*Position of top (or bottom) of minibuffer frame, in pixels.
If nil, function `1on1-set-minibuffer-frame-top/bottom' will position
minibuffer at bottom of display.

An integer.  If negative, then the position is that of the frame
bottom relative to the screen bottom.

See `default-frame-alist' for an explanation of frame parameters.

Note: This is not used if `1on1-minibuffer-frame-flag' is nil.

If you customize this variable, you will need to rerun `1on1-emacs'
for the new value to take effect.")

(custom-autoload '1on1-minibuffer-frame-top/bottom "oneonone" t)

(defvar 1on1-minibuffer-frame-width nil "\
*Width, in characters, for minibuffer frame.
If nil, then function `1on1-set-minibuffer-frame-width' is used instead.

Note: This is not used if `1on1-minibuffer-frame-flag' is nil.

If you customize this variable, you will need to rerun `1on1-emacs'
for the new value to take effect.")

(custom-autoload '1on1-minibuffer-frame-width "oneonone" t)

(defvar 1on1-minibuffer-frame-width-percent 100 "\
*Max percent of the total display width to give to minibuffer frame.
See function `1on1-set-minibuffer-frame-width'.

Note: This is not used if `1on1-minibuffer-frame-flag' is nil.

If you customize this variable, you will need to rerun `1on1-emacs'
for the new value to take effect.")

(custom-autoload '1on1-minibuffer-frame-width-percent "oneonone" t)

(defvar 1on1-fit-minibuffer-frame-flag t "\
*Non-nil means adjust `1on1-minibuffer-frame' height to fit content.
This is done after each command or user event (e.g. each key press)
when the minibuffer is active.
This option has no effect if `1on1-minibuffer-frame-flag' is nil.")

(custom-autoload '1on1-fit-minibuffer-frame-flag "oneonone" t)

(defvar 1on1-fit-minibuffer-frame-max-height nil "\
*Maximum height, in lines, that `fit-frame' gives to `1on1-minibuffer-frame'.
If nil, then function `fit-frame-max-height' is used instead,
respecting `1on1-fit-minibuffer-frame-max-height-percent'.
This has no effect if you do not use library `fit-frame.el'.")

(custom-autoload '1on1-fit-minibuffer-frame-max-height "oneonone" t)

(defvar 1on1-fit-minibuffer-frame-max-height-percent 10 "\
*Max percent that `fit-frame' gives to `1on1-minibuffer-frame'.
This is a percentage of the display height.
Not used unless `1on1-fit-minibuffer-frame-max-height' is nil.
This has no effect if you do not use library `fit-frame.el'.")

(custom-autoload '1on1-fit-minibuffer-frame-max-height-percent "oneonone" t)

(defvar 1on1-*Help*-frame-flag t "\
*Non-nil means use a special appearance for the *Help* frame.

If you customize this variable, you will need to rerun `1on1-emacs'
for the new value to take effect.")

(custom-autoload '1on1-*Help*-frame-flag "oneonone" t)

(defvar 1on1-help-frame-background "Thistle" "\
*Default background color for the *Help* buffer's frame.

Note: This is not used if `1on1-*Help*-frame-flag' is nil.

If you customize this variable, you will need to rerun `1on1-emacs'
for the new value to take effect.")

(custom-autoload '1on1-help-frame-background "oneonone" t)

(defvar 1on1-help-frame-mouse+cursor-color "Blue Violet" "\
*Default color for cursor & pointer of *Help* frame.

Note: This is not used if `1on1-*Help*-frame-flag' is nil.

If you customize this variable, you will need to rerun `1on1-emacs'
for the new value to take effect.")

(custom-autoload '1on1-help-frame-mouse+cursor-color "oneonone" t)

(defvar 1on1-*Completions*-frame-flag t "\
*Non-nil means use a special appearance for the *Completions* frame.

If you customize this variable, you will need to rerun `1on1-emacs'
for the new value to take effect.")

(custom-autoload '1on1-*Completions*-frame-flag "oneonone" t)

(defvar 1on1-*Completions*-frame-at-right-flag nil "\
*Non-nil means place *Completions* frame at right edge of display.
This can be useful to make *Completions* more visible.
This has no effect if `1on1-*Completions*-frame-flag' is nil.")

(custom-autoload '1on1-*Completions*-frame-at-right-flag "oneonone" t)

(defvar 1on1-completions-frame-background "LavenderBlush2" "\
*Default background color for the *Completions* buffer's frame.

Note: This is not used if `1on1-*Completions*-frame-flag' is nil.

If you customize this variable, you will need to rerun `1on1-emacs'
for the new value to take effect.")

(custom-autoload '1on1-completions-frame-background "oneonone" t)

(defvar 1on1-completions-frame-mouse+cursor-color "VioletRed" "\
*Default color for cursor & pointer of *Completions* frame.

Note: This is not used if `1on1-*Completions*-frame-flag' is nil.

If you customize this variable, you will need to rerun `1on1-emacs'
for the new value to take effect.")

(custom-autoload '1on1-completions-frame-mouse+cursor-color "oneonone" t)

(defvar 1on1-completions-frame-width 100 "\
*Width, in characters, for *Completions* frame.
If this is nil, then the pertinent default frame width is used.

Note: This is not used if `1on1-*Completions*-frame-flag' is nil.

If you customize this variable, you will need to rerun `1on1-emacs'
for the new value to take effect.")

(custom-autoload '1on1-completions-frame-width "oneonone" t)

(defvar 1on1-completions-frame-zoom-font-difference (or (and (require 'zoom-frm 'nil t) (* 2 frame-zoom-font-difference)) 2) "\
*Number of points to reduce the *Completions* frame font size.
This must be less than the current default font size, since the new
font size cannot be less than 1 point.
A value of zero or nil means the *Completions* frame is not zoomed.")

(custom-autoload '1on1-completions-frame-zoom-font-difference "oneonone" t)

(defvar 1on1-change-cursor-on-input-method-flag t "\
*Non-nil means to use a different cursor when using an input method.
If you customize this variable, you will need to rerun `1on1-emacs'
for the new value to take effect.")

(custom-autoload '1on1-change-cursor-on-input-method-flag "oneonone" t)

(defvar 1on1-default-frame-cursor-color "Red" "\
*Default text cursor color for non-special frames.
If you customize this variable, you will need to rerun `1on1-emacs'
for the new value to take effect.  Furthermore, if
`1on1-change-cursor-on-input-method-flag' is nil when you rerun
`1on1-emacs', you will need to toggle that variable to non-nil (and
back to nil, if that's the value you want).  Otherwise, the new value
will take effect only after you restart Emacs.")

(custom-autoload '1on1-default-frame-cursor-color "oneonone" t)

(defvar 1on1-default-frame-cursor-color-input-method "Orange" "\
*Default cursor color for non-special frames if using an input method.
This has no effect if `1on1-change-cursor-on-input-method-flag' is nil.
If you customize this variable, you will need to rerun `1on1-emacs'
for the new value to take effect.")

(custom-autoload '1on1-default-frame-cursor-color-input-method "oneonone" t)

(defvar 1on1-change-cursor-on-overwrite/read-only-flag t "\
*Non-nil means use a different cursor when overwrite mode or read-only.
If you customize this variable, you will need to rerun `1on1-emacs'
for the new value to take effect.")

(custom-autoload '1on1-change-cursor-on-overwrite/read-only-flag "oneonone" t)

(defvar 1on1-default-frame-cursor-type 'bar "\
*Default text cursor type for non-special frames.
If you customize this variable, you will need to rerun `1on1-emacs'
for the new value to take effect.  Furthermore, if
`1on1-change-cursor-on-overwrite/read-only-flag' is nil when you rerun
`1on1-emacs', you will need to toggle that variable to non-nil (and
back to nil, if that's the value you want).  Otherwise, the new value
will take effect only after you restart Emacs.")

(custom-autoload '1on1-default-frame-cursor-type "oneonone" t)

(defvar 1on1-default-frame-cursor-type-overwrite/read-only 'box "\
*Default text cursor type for overwrite mode or read-only buffer.
This applies only to non-special frames.  This has no effect if
`1on1-change-cursor-on-overwrite/read-only-flag' is nil.  If you
customize this variable, you will need to rerun `1on1-emacs' for the
new value to take effect.")

(custom-autoload '1on1-default-frame-cursor-type-overwrite/read-only "oneonone" t)

(autoload '1on1-emacs "oneonone" "\
One-on-One Emacs setup.
Use `1on1-default-frame-alist' and `1on1-special-display-frame-alist'.

If `1on1-minibuffer-frame-flag' is non-nil, then create
   minibuffer-only frame, `1on1-minibuffer-frame', using
   `1on1-minibuffer-frame-alist'.

If `1on1-separate-minibuffer-*Help*-flag' is non-nil, then use
   special frame for *Help* buffer.

If `1on1-separate-minibuffer-*Completions*-flag' is non-nil, then
   use special frame for *Completions* buffer.

\(fn)" t nil)

(autoload '1on1-set-cursor-type "oneonone" "\
Set the cursor type of the selected frame to CURSOR-TYPE.
When called interactively, prompt for the type to use.
To get the frame's current cursor type, use `frame-parameters'.

\(fn CURSOR-TYPE)" t nil)

(defalias 'toggle-box-cursor-when-idle '1on1-toggle-box-cursor-when-idle)

(autoload '1on1-toggle-box-cursor-when-idle "oneonone" "\
Turn on or off automatically changing to a box cursor when idle.
When on, the cursor is changed to a box whenever Emacs is idle.
With prefix argument, turn on if ARG > 0; else turn off.

\(fn &optional ARG)" t nil)

(autoload '1on1-set-box-cursor-when-idle-interval "oneonone" "\
Set wait until automatically change to a box cursor when Emacs is idle.
Whenever Emacs is idle for this many seconds it will change the cursor
to a box.

To turn on or off automatically changing to a box cursor when idle,
use `\\[toggle-box-cursor-when-idle].

\(fn SECS)" t nil)

(autoload '1on1-other-frame "oneonone" "\
Same as `other-frame', except include frame `1on1-minibuffer-frame'.
If `1on1-minibuffer-frame' is non-nil then it is a standalone
minibuffer frame.  In this case, include it as well as all other
visible or iconified frames as candidates.

Select the ARGth different visible frame on current display, and raise it.
Select the frame ARG steps away in the sequence of frames.
A negative ARG moves in the opposite direction.

To make this command work properly, you must tell Emacs
how the system (or the window manager) generally handles
focus-switching between windows.  If moving the mouse onto a window
selects it (gives it focus), set `focus-follows-mouse' to t.
Otherwise, that variable should be nil.

\(fn ARG)" t nil)

(autoload '1on1-fit-minibuffer-frame "oneonone" "\
Fit the standalone minibuffer frame height to its contents.
Repeat to increase the height by 1.
With a prefix arg, reset the frame to its default position and height.
Bind this in minibuffer keymaps to a key such as `C-o' that you can
use during minibuffer input.
This command requires library `fit-frame.el'.

\(fn &optional RESETP)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; oneonone-autoloads.el ends here
