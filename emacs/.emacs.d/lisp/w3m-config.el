(require 'w3m)

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
    (cd "~/")
    (if url
        (let ((proc (start-process "wget" "*wget*" ;;(format "*wget %s*" url)
                                   "wget" "-nv"
                                   "-P" "Downloads" url)))
          (message "Download started")
          (with-current-buffer (process-buffer proc) (insert "\n"))
          (set-process-sentinel proc (lambda (proc str)
                                       (message "wget download done"))))
      (message "Nothing to get"))))

(defun sacha/w3m-setup-keymap ()
  "Use my heavily customized map."
  (interactive)
  ;;;; FIREFOX
  (define-key w3m-mode-map (kbd "M-<left>") 'w3m-view-previous-page)
  (define-key w3m-mode-map (kbd "M-<right>") 'w3m-view-next-page)
  (define-key w3m-mode-map (kbd "RET") 'w3m-view-this-url-new-session)
  (define-key w3m-mode-map (kbd "C-w") 'w3m-delete-buffer)
  (define-key w3m-mode-map [(shift return)] 'w3m-view-this-url)
  (define-key w3m-mode-map (kbd "<tab>") 'w3m-next-buffer)
  (define-key w3m-mode-map (kbd "<S-iso-lefttab>") 'w3m-previous-buffer)
  (define-key w3m-mode-map "d" 'w3m-download-with-wget)

  (define-key w3m-mode-map "g" 'w3m-goto-url)
  (define-key w3m-mode-map "G" 'w3m-goto-url-new-session)
  (define-key w3m-mode-map (kbd "C-f") 'sacha/w3m-open-in-firefox)
  (define-key w3m-mode-map (kbd "M-RET") 'w3m-view-this-url-new-session)

  (define-key w3m-mode-map [(left)] 'backward-char)
  (define-key w3m-mode-map [(right)] 'forward-char)
  (define-key w3m-mode-map [(up)] 'previous-line)
  (define-key w3m-mode-map [(down)] 'next-line)

  (define-key w3m-mode-map (kbd "M-<down>") 'w3m-next-anchor)
  (define-key w3m-mode-map (kbd "M-<up>") 'w3m-previous-anchor))

(sacha/w3m-setup-keymap)

(define-key w3m-mode-map (kbd "S-SPC") 'ace-jump-mode)

(defun sacha/w3m-open-in-firefox ()
  (interactive)
  (browse-url-firefox w3m-current-url))

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
