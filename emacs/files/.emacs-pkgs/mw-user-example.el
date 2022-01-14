(defconst my-user-name "Person")
(defconst my-active-modeline-image nil)   ;Path to a small image file
(defconst my-active-modeline-image-width 10)
(defconst my-frame-height 60)
(defconst my-frame-width 100)
(defconst my-frame-title (concat my-user-name "’s GNU Emacs"))
(defconst my-theme nil)                   ;Name of theme as a symbol
(defconst my-font nil)
(defconst my-font-size 130)             ;Pixel size * 10
;; Change the location of the .emacs.d dir (full path).
(defconst my-emacsd-location nil)

;; A list of org file that are opened by command "org-open".
(defconst my-org-files '())
;; Path to the org capture file.
(defconst my-org-capture-file nil)
;; Inline CSS to embed
(defvar-local my-org-inline-css-file nil)
;; Dir for org-raom
(defconst my-roam-dir nil)

;; Path to the splash image for the splash buffer
(defconst my-splash-image nil)
(defconst my-splash-image-scale 1.0)
;; Set this to t to automatically go full screen on Mac.
(defconst my-full-screen nil)
(defconst my-smart-mode-line-theme 'light)
(defconst my-shorten-mode-string t)

;; Mail settings

(defconst my-fcc-dirs
  '(
    ;; (".*@mycompany\\.com>?" . "company-mails/sent_items")
    (".*" . nil)
    ))
(defconst my-mail-host "localhost")
(defconst my-sendmail nil)

;; How to choose signature when composing an email. Each element in
;; `my-sig-condition' is a associative list containing three keys:
;; header, pattern and sig-type. When composing a mail, the field in
;; the mail denoted by the "header" key is matched against the regexp
;; denoted by the "pattern" key. If it matches, a signature from a
;; source is chosen randomly. The signature source is denoted by the
;; "sig-type" key, which can be one of the symbols "file", "dir",
;; "fortune", or "cookie". If it's "file", a signature is chosen from
;; the file denoted by variable `my-signature-file'; if it's "dir",
;; from files in `my-signature-dir'.
(defconst my-signature-file nil)
(defconst my-signature-dir nil)
(defconst my-sig-condition
  '(((header . "from")
     (pattern . ".*@mycompany\\.com>?")
     (sig-type . file))
    ((header . "from")
     (pattern . ".*@personal-mail.com>?")
     (sig-type . dir))
    ((header . "from")
     (pattern . ".*")
     (sig-type . cookie))))

;; When replying an email, set the from field (to the cdr) by matching
;; the to field against the car.
(defconst my-from-by-to
  '((".*@mycompany\\.com>?" . "My Real Name <my.r.name@mycompany.com>")
    (".*" . "Secret Identity <secret.identity@personal-mail.com>")))

;; This will become `notmuch-saved-searches'.
(defconst my-notmuch-searchs nil)

;; IRC

;; Master nick for freenode: Darksair, and for OFTC: Corsair.
(defconst irc-nick "Nick")
(defconst irc-full-name "Full Name")
(defconst irc-password "secret")
(defconst irc-channels
  '(((host . "irc.libera.chat") (tls . t) (channels "#emacs"))
    ((host . "oftc.net") (tls . t) (channels "#channel1" "#channel2"))))

(defconst irc-pals '("joe" "john"))

;; (provide 'mw-user)
