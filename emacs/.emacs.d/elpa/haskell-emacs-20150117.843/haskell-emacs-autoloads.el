;;; haskell-emacs-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "haskell-emacs" "haskell-emacs.el" (21690 42218
;;;;;;  265071 167000))
;;; Generated autoloads from haskell-emacs.el

(autoload 'haskell-emacs-init "haskell-emacs" "\
Initialize haskell FFI or reload it to reflect changed functions.

It will try to wrap all exported functions within
`haskell-emacs-dir' into an synchronous and an asynchronous elisp
function.

Dependencies:
 - GHC
 - attoparsec
 - atto-lisp

Consider that you've got the following toy program:

---- ~/.emacs.d/haskell-fun/Matrix.hs
module Matrix (transpose, dyadic) where

import qualified Data.List as L

transpose :: [[Int]] -> [[Int]]
transpose = L.transpose

dyadic :: [Int] -> [Int] -> [[Int]]
dyadic xs ys = map (\\x -> map (x*) ys) xs
----

Now call `haskell-emacs-init' to provide the elisp wrappers.

  (Matrix.transpose '((1 2) (3 4) (5 6)))
    => ((1 3 5) (2 4 6))

  (Matrix.dyadic '(1 2 3) '(4 5 6))
    => ((4 5 6) (8 10 12) (12 15 18))

If you provide bad input, a description of the type error will be
shown to you.

If you call the async pendant of your functions, you'll get a
future which will block on evaluation if the result is not already present.

  (Matrix.transpose-async '((1 2) (3 4) (5 6)))
    => (haskell-emacs--get 7)

  (eval (haskell-emacs--get 7))
    => ((1 3 5) (2 4 6))

Or perhaps more convenient:

  (let ((tr (Matrix.transpose-async '((1 2) (3 4) (5 6)))))

       ;; other elisp stuff, or more asyncs

       (eval tr))

Haskell-emacs can handle functions of arbitrary arity (including
0), but you should note, that only monomorphic functions are
supported, and only about ten different types.

Functions that take only one argument will be fused on Emacs
side, this allows executing a chain of functions asynchronously:

  (let ((result (Matrix.transpose-async (Matrix.transpose '((1 2) (3 4))))))

    ;; other stuff

    (eval result))
     => ((1 2) (3 4))

Furthermore, it nullifies the small performance overhead (0.07 ms
per function call) between fused functions which allows more
modularity and using haskell for even more basic tasks.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("haskell-emacs-pkg.el" "haskell-emacs-test.el")
;;;;;;  (21690 42218 285307 960000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; haskell-emacs-autoloads.el ends here
