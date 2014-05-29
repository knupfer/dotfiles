(setq knu-org-mode-map t)
(defun knu-org-latex-snip ()
  (interactive)
  (insert "\\begin{align*}\n\n")
  (insert "\\end{align*}")
  (forward-line -1))

(defun knu-org-auto-preview ()
  (setq knu-org-auto-preview-count nil)
  (when (equal mode-name "Org")
    (while-no-input (progn (when (not knu-org-auto-preview-count)
                             (setq knu-org-auto-preview-count t)
                             (knu-org-preview-latex-fragment)
                             (redisplay)
                             (knu-org-preview-latex-fragment))))))

(defun knu-org-preview-latex-fragment ()
  (unless buffer-file-name
    (user-error "Can't preview LaTeX fragment in a non-file buffer"))
  (when (display-graphic-p)
    (save-excursion
      (save-restriction
        (let (beg end at msg pos)
          (setq pos (point))
          (setq beg (window-start) end (window-end))
          (narrow-to-region beg end)
          (goto-char beg)
          (knu-org-format-latex
           (concat org-latex-preview-ltxpng-directory (file-name-sans-extension
                                                       (file-name-nondirectory
                                                        buffer-file-name)))
           default-directory 'overlays nil at 'forbuffer
           org-latex-create-formula-image-program pos))))))

(defun knu-org-format-latex (prefix &optional dir overlays msg at
                                    forbuffer processing-type pos)
  (if (and overlays (fboundp 'clear-image-cache)) (clear-image-cache))
  (let* ((prefixnodir (file-name-nondirectory prefix))
         (absprefix (expand-file-name prefix dir))
         (todir (file-name-directory absprefix))
         (opt org-format-latex-options)
         (optnew org-format-latex-options)
         (matchers (plist-get opt :matchers))
         (re-list org-latex-regexps)
         (cnt 0) txt hash link beg end re e checkdir
         string
         m n block-type block linkfile movefile ov)
    ;; Check the different regular expressions
    (while (setq e (pop re-list))
      (setq m (car e) re (nth 1 e) n (nth 2 e) block-type (nth 3 e)
            block (if block-type "\n\n" ""))
      (when (member m matchers)
        (goto-char (point-min))
        (while (re-search-forward re nil t)
          (when (and (or (not at) (equal (cdr at) (match-beginning n)))
                     (or (not overlays)
                         (not (eq (get-char-property (match-beginning n)
                                                     'org-overlay-type)
                                  'org-latex-overlay))))
            (setq txt (match-string n)
                  beg (match-beginning n) end (match-end n)
                  cnt (1+ cnt))
            (let ((face (face-at-point))
                  (fg (plist-get opt :foreground))
                  (bg (plist-get opt :background))
                  ;; Ensure full list is printed.
                  print-length print-level)
              (when forbuffer
                ;; Get the colors from the face at point.
                (goto-char beg)
                (when (eq fg 'auto)
                  (setq fg (face-attribute face :foreground nil 'default)))
                (when (eq bg 'auto)
                  (setq bg (face-attribute face :background nil 'default)))
                (setq optnew (copy-sequence opt))
                (plist-put optnew :foreground fg)
                (plist-put optnew :background bg))
              (setq hash (sha1 (prin1-to-string
                                (list org-format-latex-header
                                      org-latex-default-packages-alist
                                      org-latex-packages-alist
                                      org-format-latex-options
                                      forbuffer txt fg bg)))
                    linkfile (format "%s_%s.png" prefix hash)
                    movefile (format "%s_%s.png" absprefix hash)))
            (setq link (concat block "[[file:" linkfile "]]" block))
            (goto-char beg)
            (unless checkdir        ; Ensure the directory exists.
              (setq checkdir t)
              (or (file-directory-p todir) (make-directory todir t)))
            (unless (file-exists-p movefile)
              (org-create-formula-image
               txt movefile optnew forbuffer processing-type)
              (save-excursion
                (goto-char pos)
                (redisplay)))
            (if overlays
                (progn
                  (mapc (lambda (o)
                          (if (eq (overlay-get o 'org-overlay-type)
                                  'org-latex-overlay)
                              (delete-overlay o)))
                        (overlays-in beg end))
                  (setq ov (make-overlay beg end))
                  (overlay-put ov 'org-overlay-type 'org-latex-overlay)
                  (if (featurep 'xemacs)
                      (progn
                        (overlay-put ov 'invisible t)
                        (overlay-put
                         ov 'end-glyph
                         (make-glyph (vector 'png :file movefile))))
                    (overlay-put
                     ov 'display
                     (list 'image :type 'png :file movefile :ascent 'center)))
                  (push ov org-latex-fragment-image-overlays)
                  (goto-char end))
              (delete-region beg end)
              (insert (org-add-props link
                          (list 'org-latex-src
                                (replace-regexp-in-string
                                 "\"" "" txt)
                                'org-latex-src-embed-type
                                (if block-type 'paragraph 'character)))))))))))

(run-with-idle-timer 2 t 'knu-org-auto-preview)

(define-key org-mode-map (kbd "<f5>") 'knu-org-latex-snip)

(define-key org-mode-map "α" "\\alpha")
(define-key org-mode-map "β" "\\beta")
(define-key org-mode-map "γ" "\\gamma")
(define-key org-mode-map "δ" "\\delta")
(define-key org-mode-map "ε" "\\epsilon")
(define-key org-mode-map "ζ" "\\zeta")
(define-key org-mode-map "θ" "\\theta")
(define-key org-mode-map "ι" "\\iota")
(define-key org-mode-map "κ" "\\kappa")
(define-key org-mode-map "λ" "\\lambda")
(define-key org-mode-map "μ" "\\mu")
(define-key org-mode-map "ν" "\\nu")
(define-key org-mode-map "ξ" "\\xi")
(define-key org-mode-map "ο" "\\omicron")
(define-key org-mode-map "π" "\\pi")
(define-key org-mode-map "ρ" "\\rho")
(define-key org-mode-map "σ" "\\sigma")
(define-key org-mode-map "τ" "\\tau")
(define-key org-mode-map "φ" "\\phi")
(define-key org-mode-map "χ" "\\chi")
(define-key org-mode-map "ψ" "\\psi")
(define-key org-mode-map "ω" "\\omega")

