haskell-emacs-base.el provides a lot of haskell functions from
Prelude.  It uses `haskell-emacs' to register these functions.

Run (require 'haskell-emacs-base) or put it into your .emacs.
Afterwards run M-x haskell-emacs-init.

(Base.product '(1 2 3))
  => 6.0

If you want to use these functions in your library, put there the
following:

(require 'haskell-emacs-base)
(eval-when-compile (haskell-emacs-init))

See documentation for `haskell-emacs-init' for more info.
