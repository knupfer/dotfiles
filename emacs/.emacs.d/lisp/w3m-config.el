
(require 'w3m)

;; Guessed
(defun w3m-filter-find-relationships (url next previous)
  "Add <LINK> tags if they don't yet exist."
  (let ((case-fold-search t))
    (goto-char (point-max))
    (when (re-search-backward next nil t)
      (when (re-search-backward "href=\"?\\([^\" \t\n]+\\)" nil t)
        (setq w3m-next-url (match-string 1))))
    (when (re-search-backward previous nil t)
      (when (re-search-backward "href=\"?\\([^\" \t\n]+\\)" nil t)
        (setq w3m-previous-url (match-string 1))))))

(defun w3m-download-with-wget ()
  (interactive)
  (let ((url (or (w3m-anchor) (w3m-image))))
    (if url
        (let ((proc (start-process "wget" (format "*wget %s*" url)
                                   "wget" "-x" "--passive-ftp" "-nv"
                                   "-P" "/home/sacha/notebook/mirrors" url)))
          (with-current-buffer (process-buffer proc)
            (erase-buffer))
          (set-process-sentinel proc (lambda (proc str)
                                       (message "wget download done"))))
      (message "Nothing to get"))))

(defvar sacha/w3m-mirror-directory nil "*Directory where my files are mirrored.")

(defun sacha/w3m-mirror-current-page (url &optional recursive)
  "Download specified URL to my mirrors directory.
If called interactively, mirrors current page.
If prefix argument RECURSIVE is non-nil, recurse into subdirectories."
  (interactive (list w3m-current-url current-prefix-arg))
  (with-temp-buffer
    (message "Getting %s" url)
    (cd sacha/w3m-mirror-directory)
    (apply 'start-process "wget"
           (format "*wget %s*" w3m-current-url)
           "wget"
           "-nv" ; non-verbose
           "-x"
           "--random-wait"
           "-k"
           "-p"
           url
           (if recursive
               (list "-r" "--no-parent" "-N" "-nw")))))

(defun sacha/w3m-mirror-link (recursive)
  "Mirror current link.
If prefix argument RECURSIVE is non-nil, recurse into subdirectories."
  (interactive "P")
  (sacha/w3m-mirror-current-page (w3m-anchor) recursive))


;(defadvice switch-to-buffer (after sacha activate)
;  "Update w3m tabs."
;  (when (and (interactive-p)
;           (eq major-mode 'w3m-mode)
;           header-line-format)
;    (w3m-force-window-update)))

(defun sacha/w3m-setup-keymap ()
  "Use my heavily customized map."
  (interactive)
 ; ;; Undefine this key and use the advice instead so that my ido doesn't get
 ; ;; overridden
 ; (define-key w3m-mode-map (kbd "C-x b") nil)
 ; (define-key w3m-mode-map "C" 'w3m-print-this-url)
 ; (define-key w3m-mode-map "a" 'sacha/delicious-url)
 ; (define-key w3m-mode-map "A" 'w3m-bookmark-add-current-url)
 ; (define-key w3m-mode-map "w" 'w3m-download-with-wget)
 ; (define-key w3m-mode-map "d" 'w3m-download-with-wget)
 ; (define-key w3m-mode-map "D" 'w3m-download-this-url)
 ; ;; Do not override my ever so handy ERC binding
 ; (define-key w3m-mode-map (kbd "C-c C-SPC") nil)
 ; (define-key w3m-mode-map "m" 'sacha/w3m-mirror-current-page)
 ; (define-key w3m-mode-map "M" 'sacha/w3m-mirror-link)
 ; ;; I use search much more often than the context history list, although
 ; ;; context is still cool. 
 ; (define-key w3m-mode-map "!" 'sacha/w3m-mirror-current-page)
 ; (define-key w3m-mode-map "s" 'w3m-search)
 ; (define-key w3m-mode-map "h" 'w3m-history)
 ; (define-key w3m-mode-map "t" 'w3m-scroll-down-or-previous-url)
 ; (define-key w3m-mode-map "n" 'w3m-scroll-up-or-next-url)
 ; ;; I don't often w3m to edit pages, so I'm borrowing o and e (right
 ; ;; below , / . for tab navigation) for page navigation instead.
 ; (define-key w3m-mode-map "o" 'w3m-view-previous-page)
 ; (define-key w3m-mode-map "e" 'w3m-view-next-page)
 ; ;; i is a more useful mnemonic for toggling images
 ; (define-key w3m-mode-map "i" 'w3m-toggle-inline-image)
 ; (define-key w3m-mode-map "I" 'w3m-toggle-inline-images)
 ; ;; and X for closing the buffer
 ; (define-key w3m-mode-map "X" 'w3m-delete-buffer)
 ; (define-key w3m-mode-map "x" 'w3m-delete-buffer)
 ; (define-key w3m-mode-map "z" 'w3m-delete-buffer)
 ; ;; and b for bookmarks
 ; (define-key w3m-mode-map "b" 'w3m-bookmark-view)
 ; ;; I don't use the Qwerty keymap, so hjkl is useless for me
 ; ;; I'll use HTNS, though
 ; (define-key w3m-mode-map "H" 'backward-char)
 ; (define-key w3m-mode-map "T" 'previous-line)
 ; (define-key w3m-mode-map "N" 'next-line)
 ; (define-key w3m-mode-map "S" 'forward-char)
 ; ;; Browse in new sessions by default
 ; (define-key w3m-mode-map (kbd "RET") 'w3m-view-this-url-new-session)
 ; (define-key w3m-mode-map [(shift return)] 'w3m-view-this-url)
 ; (define-key w3m-mode-map "g" 'w3m-goto-url)
 ; (define-key w3m-mode-map "G" 'w3m-goto-url-new-session)
 ; ;; f for forward? I want to be able to follow links without removing
 ; ;; most of my fingers from home row. My fingers are too short to hit
 ; ;; Enter.
 ; (define-key w3m-mode-map "f" 'w3m-view-this-url-new-session)
 ; (define-key w3m-mode-map "F" 'w3m-view-this-url)
 ; ;; Use cursor keys to scroll
 ; (define-key w3m-mode-map [(left)] 'backward-char)
 ; (define-key w3m-mode-map [(right)] 'forward-char)
 ; (define-key w3m-mode-map [(shift left)] 'w3m-shift-right)
 ; (define-key w3m-mode-map [(shift right)] 'w3m-shift-left)
 ; ;; Which means I can now use , and . to switch pages
 ; (define-key w3m-mode-map "." 'w3m-next-buffer)
 ; (define-key w3m-mode-map "," 'w3m-previous-buffer)
 ; ;; IBM stuff
 ; (define-key w3m-mode-map "i" nil)
 ; (define-key w3m-mode-map "ib" 'sacha/ibm-blog)
 ; (define-key w3m-mode-map "id" 'sacha/dogear-url)
 ; (define-key w3m-mode-map "f" 'sacha/w3m-open-in-firefox)


  (define-key w3m-mode-map (kbd "M-RET") 'w3m-view-this-url-new-session)

  (define-key w3m-mode-map [(left)] 'backward-char)
  (define-key w3m-mode-map [(right)] 'forward-char)
  (define-key w3m-mode-map [(up)] 'previous-line)
  (define-key w3m-mode-map [(down)] 'next-line)

  (define-key w3m-mode-map "u" 'w3m-scroll-down-or-previous-url)
  (define-key w3m-mode-map "." 'w3m-scroll-up-or-next-url)
  (define-key w3m-mode-map "\M-u" 'w3m-view-previous-page)
  (define-key w3m-mode-map "\M-." 'w3m-view-next-page)
  (define-key w3m-mode-map "\C-a" 'w3m-next-anchor)
  (define-key w3m-mode-map "\C-i" 'w3m-previous-anchor)
  (define-key w3m-mode-map "\M-a" 'w3m-next-buffer)
  (define-key w3m-mode-map "\M-i" 'w3m-previous-buffer)

  )

(defun sacha/w3m-open-in-firefox ()
  (interactive)
  (browse-url-firefox w3m-current-url))

;; Don't know if it's the best way , but it seemed to work. (Requires emacs >= 20)
(defun browse-apropos-url (text &optional new-window)
  (interactive (browse-url-interactive-arg "Location: "))
  (let ((text (replace-regexp-in-string 
               "^ *\\| *$" "" 
               (replace-regexp-in-string "[ \t\n]+" " " text)))
        ___braplast)
    (let ((url (or (assoc-if
                    (lambda (a) (string-match a text))
                    apropos-url-alist)
                   text)))
      (browse-url (replace-regexp-in-string (car url) (cdr url) text) new-window))))

(setq apropos-url-alist
      '(("^gw?:? +\\(.*\\)" . ;; Google Web 
         "http://www.google.com/search?q=\\1")

        ("^g!:? +\\(.*\\)" . ;; Google Lucky
         "http://www.google.com/search?btnI=I%27m+Feeling+Lucky&q=\\1")
        
        ("^gl:? +\\(.*\\)" .  ;; Google Linux 
         "http://www.google.com/linux?q=\\1")
        
        ("^gi:? +\\(.*\\)" . ;; Google Images
         "http://images.google.com/images?sa=N&tab=wi&q=\\1")

        ("^gg:? +\\(.*\\)" . ;; Google Groups
         "http://groups.google.com/groups?q=\\1")

        ("^gd:? +\\(.*\\)" . ;; Google Directory
         "http://www.google.com/search?&sa=N&cat=gwd/Top&tab=gd&q=\\1")

        ("^gn:? +\\(.*\\)" . ;; Google News
         "http://news.google.com/news?sa=N&tab=dn&q=\\1")

        ("^gt:? +\\(\\w+\\)|? *\\(\\w+\\) +\\(\\w+://.*\\)" . ;; Google Translate URL
         "http://translate.google.com/translate?langpair=\\1|\\2&u=\\3")
        
        ("^gt:? +\\(\\w+\\)|? *\\(\\w+\\) +\\(.*\\)" . ;; Google Translate Text
         "http://translate.google.com/translate_t?langpair=\\1|\\2&text=\\3")

        ("^/\\.$" . ;; Slashdot 
         "http://www.slashdot.org")

        ("^/\\.:? +\\(.*\\)" . ;; Slashdot search
         "http://www.osdn.com/osdnsearch.pl?site=Slashdot&query=\\1")        
        
        ("^fm$" . ;; Freshmeat
         "http://www.freshmeat.net")

        ("^ewiki:? *?\\(.*\\)" . ;; Emacs Wiki Search
         "http://www.emacswiki.org/cgi-bin/wiki?search=\\1")
 
        ("^ewiki$" . ;; Emacs Wiki 
         "http://www.emacswiki.org")

        ("^arda$" . ;; The Encyclopedia of Arda 
         "http://www.glyphweb.com/arda/")
         
         ))

(provide 'w3m-config)
