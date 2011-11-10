;;; -*- Mode: Emacs-Lisp -*-
;;; Copyright © 2002 Jamie Zawinski <jwz@jwz.org>.
;;;
;;; Permission to use, copy, modify, distribute, and sell this software and its
;;; documentation for any purpose is hereby granted without fee, provided that
;;; the above copyright notice appear in all copies and that both that
;;; copyright notice and this permission notice appear in supporting
;;; documentation.  No representations are made about the suitability of this
;;; software for any purpose.  It is provided "as is" without express or 
;;; implied warranty.
;;;
;;; Created: 27-May-2002.
;;;
;;; Thist posts to LiveJournal, via the HTML interface.
;;; Really all it does is let you edit some text, then constructs
;;; a proper URL and does a HTTP POST to it.
;;; It's clever about figuring out what your username and password
;;; are by digging around in your Netscape/Mozilla cookies file.
;;;
;;; Interesting commands:
;;;
;;;    M-x livejournal  Fill in the fields, edit the body.
;;;    M-x ljpreview    Save the HTML to a temp file and send it to a
;;;                     web browser to see what it will look like.
;;;                     Converts the <LJ> tags to something readable.
;;;    C-c C-c          Submit it.
;;;
;;; Useful non-interactive function:
;;;
;;;    jwz-lj-post      Useful for letting other programs submit entries.
;;;

(defvar jwz-lj-fcc-file nil
  "*Where to save a copy of your LJ posts.")


(defvar jwz-lj-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-name map 'jwz-lj-mode-map)
    (define-key map "\C-c\C-c" 'jwz-lj-submit)
    map))


(defconst jwz-lj-entity-table
  '(
    ;("quot"   . ?\") ("amp"    . ?\&) ("lt"     . ?\<) ("gt"     . ?\>)
    ;("nbsp"   . ?\ )
    ("iexcl"  . ?\¡) ("cent"   . ?\¢) ("pound"  . ?\£)
    ("curren" . ?\¤) ("yen"    . ?\¥) ("brvbar" . ?\¦) ("sect"   . ?\§)
    ("uml"    . ?\¨) ("copy"   . ?\©) ("ordf"   . ?\ª) ("laquo"  . ?\«)
    ("not"    . ?\¬) ("shy"    . ?\­) ("reg"    . ?\®) ("macr"   . ?\¯)
    ("deg"    . ?\°) ("plusmn" . ?\±) ("sup2"   . ?\²) ("sup3"   . ?\³)
    ("acute"  . ?\´) ("micro"  . ?\µ) ("para"   . ?\¶) ("middot" . ?\·)
    ("cedil"  . ?\¸) ("sup1"   . ?\¹) ("ordm"   . ?\º) ("raquo"  . ?\»)
    ("frac14" . ?\¼) ("frac12" . ?\½) ("frac34" . ?\¾) ("iquest" . ?\¿)
    ("Agrave" . ?\À) ("Aacute" . ?\Á) ("Acirc"  . ?\Â) ("Atilde" . ?\Ã)
    ("Auml"   . ?\Ä) ("Aring"  . ?\Å) ("AElig"  . ?\Æ) ("Ccedil" . ?\Ç)
    ("Egrave" . ?\È) ("Eacute" . ?\É) ("Ecirc"  . ?\Ê) ("Euml"   . ?\Ë)
    ("Igrave" . ?\Ì) ("Iacute" . ?\Í) ("Icirc"  . ?\Î) ("Iuml"   . ?\Ï)
    ("ETH"    . ?\Ð) ("Ntilde" . ?\Ñ) ("Ograve" . ?\Ò) ("Oacute" . ?\Ó)
    ("Ocirc"  . ?\Ô) ("Otilde" . ?\Õ) ("Ouml"   . ?\Ö) ("times"  . ?\×)
    ("Oslash" . ?\Ø) ("Ugrave" . ?\Ù) ("Uacute" . ?\Ú) ("Ucirc"  . ?\Û)
    ("Uuml"   . ?\Ü) ("Yacute" . ?\Ý) ("THORN"  . ?\Þ) ("szlig"  . ?\ß)
    ("agrave" . ?\à) ("aacute" . ?\á) ("acirc"  . ?\â) ("atilde" . ?\ã)
    ("auml"   . ?\ä) ("aring"  . ?\å) ("aelig"  . ?\æ) ("ccedil" . ?\ç)
    ("egrave" . ?\è) ("eacute" . ?\é) ("ecirc"  . ?\ê) ("euml"   . ?\ë)
    ("igrave" . ?\ì) ("iacute" . ?\í) ("icirc"  . ?\î) ("iuml"   . ?\ï)
    ("eth"    . ?\ð) ("ntilde" . ?\ñ) ("ograve" . ?\ò) ("oacute" . ?\ó)
    ("ocirc"  . ?\ô) ("otilde" . ?\õ) ("ouml"   . ?\ö) ("divide" . ?\÷)
    ("oslash" . ?\ø) ("ugrave" . ?\ù) ("uacute" . ?\ú) ("ucirc"  . ?\û)
    ("uuml"   . ?\ü) ("yacute" . ?\ý) ("thorn"  . ?\þ) ("yuml"   . ?\ÿ))
  "HTML entities to Latin1 characters.")


(defun jwz-lj-entify (&optional start end)
  "Convert any non-ASCII characters in the region to HTML entities.
If there is no region, use the whole buffer."
  (interactive)
  (let ((re (concat "["
                    (mapconcat #'(lambda (x) (make-string 1 (cdr x)))
                               jwz-lj-entity-table nil)
                    "]")))
    (cond ((or start end)
           (or start (setq start (point-min)))
           (or end   (setq end   (point-max))))
          (t
           (setq start (point-min))
           (setq end (point-max)))
          (if (region-active-p)
              (setq start (if (< (point) (mark)) (point) (mark))
                    end   (if (< (point) (mark)) (mark) (point)))))
    (save-excursion
      (goto-char start)
      (setq end (copy-marker end))
      (while (search-forward-regexp re end t)
        (let* ((ch (preceding-char))
               (entity (or (car (rassq ch jwz-lj-entity-table))
                           (error "no entity %c" ch))))
          (delete-char -1)
          (insert "&" entity ";"))))))


(defun jwz-lj-http-encode (string &optional convert-latin1-p)
  "Encodes the string as per the URL quoting conventions (%XX)."
  (let (b)
    (save-excursion
      (unwind-protect
          (progn
            (setq b (get-buffer-create " *jwz-lj-encode*"))
            (set-buffer b)
            (erase-buffer)
            (insert string)

            (if convert-latin1-p
                (jwz-lj-entify (point-min) (point-max)))

            (goto-char (point-min))
            (while (re-search-forward "[^-a-zA-Z0-9_]" nil t)
              (let ((c (preceding-char)))
                (delete-char -1)
                (insert "%" (format "%02X" c))))

            (buffer-string))
        (if b (kill-buffer b))))))


(defun jwz-lj-make-url (subject body user
                        &optional security-level
                                  auto-format-p disallow-comments-p
                                  current-mood current-music)
  "Creates a URL for making a post to LiveJournal."

  (setq subject   (jwz-lj-http-encode subject t))
  (setq body      (jwz-lj-http-encode body t))

  (or security-level (setq security-level "public"))
  (setq security-level (jwz-lj-http-encode security-level t))

  (if (stringp current-mood)
      (setq current-mood  (jwz-lj-http-encode current-mood t)))

  (if current-music
      (setq current-music (jwz-lj-http-encode current-music t)))

  (let* ((timestr (current-time-string))
         (year (substring timestr 20 24))
         (mon  (format "%02d"
                       (position (intern (substring timestr  4  7))
                                 '(nil Jan Feb Mar Apr May Jun
                                       Jul Aug Sep Oct Nov Dec))))
         (day  (format "%02d" (string-to-int (substring timestr  8 10))))
         (hour (format "%02d" (string-to-int (substring timestr 11 13))))
         (min  (format "%02d" (string-to-int (substring timestr 14 16))))
         (url-base "http://www.livejournal.com/update.bml")
         (url (concat
               url-base
               "?mode=update"
               "&oldmode=full"
               "&remoteuser=" user
               "&user=" user
               "&hpassword=_%28remote%29"
               "&year=" year
               "&mon=" mon
               "&day=" day
               "&hour=" hour
               "&min=" min
               "&subject=" subject
               "&event=" body
               "&do_spellcheck=0"
               "&webversion=full"
               "&security=" security-level
               "&prop_opt_preformatted=" (if auto-format-p "0" "1")
               "&prop_opt_nocomments=" (if disallow-comments-p "1" "0")
               "&prop_opt_noemail=0"
               "&prop_opt_backdated=0"
               (cond ((integerp current-mood)
                      (concat "&prop_current_moodid=" current-mood))
                     (current-mood
                      (concat "&prop_current_mood=" current-mood))
                     (t ""))
               (if current-music
                   (concat "&prop_current_music=" current-music)
                 "")
               "&submit=Update%20Journal")))
    url))


(defun jwz-lj-post-1 (url cookies)
  "Does an HTTP POST to the given URL.
If fast-server-p is true, sends the fast-server cookie as well."

  (or (string-match "\\`http://\\([^/]+\\)\\([^?&]+\\)\\?\\(.*\\)\\'" url)
      (error "unparsable url: %s" url))

  ;; convert alist entries to an http header.
  (setq cookies (concat "Cookie: "
                        (mapconcat #'(lambda (c) (concat (car c) "=" (cdr c)))
                                   cookies
                                   "; ")
                        "\r\n"))

  (let* ((timeout 180)  ; seconds to wait
         (host (match-string 1 url))
         (port 80)  ; sue me
         (path (match-string 2 url))
         (args (match-string 3 url))
         
         (post-cmd
          (concat "POST " path " HTTP/1.0\r\n"
                  "Content-Type: application/x-www-form-urlencoded\r\n"
                  "Content-Length: " (int-to-string (length args)) "\r\n"
                  "Host: " host "\r\n"
                  cookies
                  "\r\n"
                  args))
         proc buf)

    (unwind-protect
        (progn
          (setq proc (open-network-stream "LiveJournal"
                                          "*LiveJournal-Server-Response*"
                                          host
                                          port)
                buf (process-buffer proc))

          (process-send-string proc post-cmd)
          (message "Posted to %s; waiting for response..." host)

          (while (equal (process-status proc) 'open)
            (unless (accept-process-output proc timeout)
              (delete-process proc)
              (error "Server error: timed out while waiting!")))

          (message "Response received; processing...")

          (with-current-buffer buf
            (goto-char (point-min))
            (cond ((not (looking-at "^HTTP/1.* 200 .*"))
                   (error "Post failed: HTTP error: %s" (buffer-string)))
                  ((not (re-search-forward "\\bUpdate successful\\b" nil t))
                   (let ((err "Post failed: Update not successful?"))
                     (if (re-search-forward
                          "<span class=\"heading\">\\(.*\\)</span>" nil t)
                         (setq err (concat "LJ Error: " (match-string 1))))
                     (error err)))
                  (t  ; ok.
                   nil)))

          (message "Posted!")
          )
      ;; unwind-protect
      (if buf (kill-buffer buf))))
  nil)


(defun jwz-lj-get-cookies (&optional hostname)
  "Looks in the Netscape and/or Mozilla cookie files to find LiveJournal data.
Returns an alist of matching cookies."

  (or hostname (setq hostname "www.livejournal.com"))

  (let ((files (list "~/.netscape/cookies"))
        (cookies '()))

    ;; FTSO Mozilla!
    (if (file-directory-p "~/.mozilla")
        (let ((dir
               (let (d)
                 (cond ((file-directory-p (setq d "~/.mozilla/default"))
                        d)
                       ((file-directory-p
                         (setq d (concat "~/.mozilla/" (user-login-name))))
                        d)
                       (t (error "can't figure out your .mozilla profile"))))))
          (setq dir (car (directory-files dir "\\.slt$" nil 'dirs)))
          (or dir (error "couldn't figure mozilla salt directory"))
          (setq files (cons (concat dir "/cookies.txt") files))))

    ;; Galeon
    (if (file-directory-p "~/.galeon/mozilla/galeon")
        (setq files (cons "~/.galeon/mozilla/galeon/cookies.txt"
                          files)))

    (while files
      (save-excursion
        (let (b)
          (unwind-protect
              (progn
                (setq b (get-buffer-create " *lj-cookie-tmp*"))
                (set-buffer b)
                (insert-file-contents (car files) nil nil nil t)
                (goto-char (point-min))
                (while (not (eobp))
                  (if (looking-at (concat "^\\([^\t]+\\)\t" ; 1 host
                                          "\\([^\t]+\\)\t" ; 2 bool
                                          "\\([^\t]+\\)\t" ; 3 path
                                          "\\([^\t]+\\)\t" ; 4 bool
                                          "\\([^\t]+\\)\t" ; 5 time_t
                                          "\\([^\t]+\\)\t" ; 6 key
                                          "\\([^\t]+\\)$")) ; 7 val
                      (let ((host (match-string 1))
                            (key (match-string 6))
                            (val (match-string 7)))
                        (if (string-match "^\\." host)
                            (setq host (concat "^.*"
                                               (regexp-quote host) "$"))
                          (setq host (concat "^" (regexp-quote host) "$")))
                        (if (and (string-match host hostname)
                                 (not (assoc key cookies)))
                            (setq cookies (cons (cons key val) cookies)))
                        ))
                  (forward-line 1))
                )
            ;; unwind-protected
            (if b (kill-buffer b)))))
      (if cookies
          (setq files nil)
        (setq files (cdr files))))
    (nreverse cookies)))


(defun jwz-lj-get-cookie-user (cookies)
  ;; extracts the user id out of the alist of lj cookies
  (let ((c (cdr (assoc "ljsession" cookies))))
    (and (string-match "^\\([^:]+\\):\\([^:]+\\):" c)
         (match-string 2 c))))


(defun jwz-lj-post (subject body
                    &optional security-level
                              auto-format-p disallow-comments-p
                              current-mood current-music)
  "Post to LiveJournal.
Determines the user and hpassword from the Netscape/Mozilla cookies,
if they are not provided."

  (let* ((cookies (or (jwz-lj-get-cookies) (error "no LJ cookies found")))
         (user (jwz-lj-get-cookie-user cookies))
         (url (jwz-lj-make-url subject body user
                               security-level auto-format-p disallow-comments-p
                               current-mood current-music)))
    (jwz-lj-post-1 url cookies)))


(defun jwz-lj-mode ()
  (interactive)
  (html-mode)
  (use-local-map jwz-lj-mode-map)
  (setq mode-name "jwz-LJ")
  (setq major-mode 'jwz-lj-mode)
  (run-hooks 'jwz-lj-mode-hook))

(defun jwz-lj ()
  "*Compose a post to LiveJournal."
  (interactive)
  (switch-to-buffer (generate-new-buffer "*livejournal*"))
  (erase-buffer)
  (goto-char (point-min))
  (insert "Subject: \n"
          "Music: \n"
;          "Mood: \n"
          "Security: public\n"
          "FCC: " (or jwz-lj-fcc-file "") "\n"
          "\n")
  (goto-char (point-min))
  (end-of-line)
  (jwz-lj-mode))


(defun jwz-lj-html-clean (string &optional unfold-lines-p show-lj-tags-p)
  ;; basically just replaces newlines with spaces.
  ;; if show-lj-tags is true, then makes any <lj*> tags be visible.
  (let (b)
    (save-excursion
      (unwind-protect
          (progn
            (setq b (get-buffer-create " *jwz-lj-clean*"))
            (set-buffer b)
            (erase-buffer)
            (insert string)

            (cond (unfold-lines-p
                   (goto-char (point-min))
                   (while (search-forward "\n" nil t)
                     (delete-char -1)
                     (unless (or (= (preceding-char) ?\ )
                                 (= (following-char) ?\ ))
                       (insert " ")))))
            (cond (show-lj-tags-p
                   (goto-char (point-min))
                   (let ((case-fold-search t))
                     (while (re-search-forward "<\\(/?lj[^>]*\\)+>" nil t)
                       (let ((tt (match-string 1))
                             (u nil)
                             (ins nil)
                             (e (match-end 0)))
                         (goto-char (match-beginning 0))

                         (cond ((looking-at "</?lj[ \t]*user=\"\\([^\"]+\\)\"")
                                (setq u (match-string 1)))
                               ((looking-at "<lj-pq\\b")
                                (setq ins "<P>")
                                (save-excursion
                                  (goto-char e)
                                  (search-forward "<" nil t)
                                  (forward-char -1)
                                  (insert "<UL>")))
                               ((looking-at "</lj-pq\\b") (setq ins "</UL>"))
                               ((looking-at "<lj-pi\\b")  (setq ins "<LI>"))
                               )

                         (delete-region (point) e)
                         (if ins (insert ins))
                         (if u
                             (insert
                              "<A HREF=\"http://www.livejournal.com/"
                              "userinfo.bml?user=" u "\">"
                              "<IMG SRC=\"http://www.livejournal.com/"
                              "img/userinfo.gif\" WIDTH=17 HEIGHT=17"
                              " BORDER=0 ALIGN=absmiddle VSPACE=0 HSPACE=0>"
                              "</A>"
                              "<A HREF=\"http://www.livejournal.com"
                              "/users/" u "\">" u "</A>")
                           (insert "<B><U>&lt;" tt "&gt;</U></B>"))
                         )))))

            ;; put some para breaks in
            (goto-char (point-min)) (insert "<P>")
            (goto-char (point-max)) (insert "<P>")
            (buffer-string))
        (if b (kill-buffer b))))))


(defun jwz-lj-submit ()
  "*Compose a post to LiveJournal."
  (interactive)
  (let ((case-fold-search t)
        (auto-p nil)
        (no-comments-p nil)
        subj music mood sec fcc body)
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (search-forward "\n\n")
        (narrow-to-region (point-min) (point))

        (goto-char (point-min))
        (if (re-search-forward "^Subject:[ \t]*\\(.*\\)$" nil t)
            (setq subj (match-string 1)))

        (goto-char (point-min))
        (if (re-search-forward "^Music:[ \t]*\\(.*\\)$" nil t)
            (setq music (match-string 1)))

        (goto-char (point-min))
        (if (re-search-forward "^Mood:[ \t]*\\(.*\\)$" nil t)
            (setq mood (match-string 1)))

        (goto-char (point-min))
        (if (re-search-forward "^FCC:[ \t]*\\(.*\\)$" nil t)
            (setq fcc (match-string 1)))

        (goto-char (point-min))
        (if (re-search-forward "^Security:[ \t]*\\(.*\\)$" nil t)
            (setq sec (match-string 1)))

        (goto-char (point-max))
        (widen)
        (setq body (buffer-substring (point) (point-max)))))

    (if (and fcc (not (equal fcc "")))
        (jwz-lj-do-fcc fcc (jwz-lj-html-clean body nil t) subj))

    (setq body (jwz-lj-html-clean body t nil))

    (jwz-lj-post subj body sec auto-p no-comments-p mood music))
  (kill-buffer (current-buffer)))


(defun jwz-lj-do-fcc (file html subj)
  (let (b)
    (save-excursion
      (unwind-protect
          (progn
            (setq b (get-buffer-create " *jwz-lj-fcc*"))
            (set-buffer b)
            (erase-buffer)
            (insert "From " (user-login-name) " -\n"
                    "From: " (user-full-name) " <" (user-mail-address) ">\n"
                    "Date: " (current-time-string) "\n"
                    "Subject: " (or subj "") "\n"
                    "MIME-Version: 1.0\n"
                    "Content-Type: text/html\n"
                    "X-Mozilla-Status: 0000\n"
                    "\n"
                    html
                    "\n\n")
            (write-region (point-min) (point-max) file t nil))
        (if b (kill-buffer b)))))
  nil)


(defun jwz-lj-preview ()
  "Sends the body of the current post to Mozilla to view it."
  (interactive)
  (let ((file "/tmp/ljtmp.html")
        body)
    (save-excursion
      (save-restriction
        (widen)
        (let (subject music)
          (goto-char (point-min))
          (if (re-search-forward "^Subject:[ \t]*\\(.*\\)$" nil t)
              (setq subject (match-string 1)))
          (goto-char (point-min))
          (if (re-search-forward "^Music:[ \t]*\\(.*\\)$" nil t)
              (setq music (match-string 1)))
          (goto-char (point-min))
          (search-forward "\n\n")
          (setq body
              (concat "<TITLE>LiveJournal Preview</TITLE>\n"
                      "<P><B>Subject: " (or subject "")
                      "<BR>Music: " (or music "") "</B><P>"
                      (jwz-lj-html-clean (buffer-substring (point) (point-max))
                                         t t))))))

    (let (b)
      (save-excursion
        (unwind-protect
            (progn
              (setq b (get-buffer-create " *jwz-lj-preview*"))
              (set-buffer b)
              (erase-buffer)
              (insert body)
              (write-region (point-min) (point-max) file))
          (if b (kill-buffer b)))))

    (browse-url file)))

(fset 'lj 'jwz-lj)
(fset 'livejournal 'jwz-lj)
(fset 'ljpreview 'jwz-lj-preview)
(fset 'entify 'jwz-lj-entify)

(provide 'jwz-lj)
