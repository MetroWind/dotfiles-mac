(defgroup my nil "Customization group for my configuration")

(defcustom my-user-name "My Name"
  "Name of user. Used in Email and frame title."
  :type 'string)

(defcustom my-active-modeline-image nil
  "A path of a small image file.

If non-nil, it will be displayed in the modeline."
  :type '(radio (file :tag "Path of the image")
                (const :tag "No image (nil)" nil)))

(defcustom my-active-modeline-image-width 0
  "Pixel width of the modeline image."
  :type 'integer)

(defcustom my-frame-height 60
  "Height of the first frame, in characters."
  :type 'integer)

(defcustom my-frame-width 100
  "Width of the first frame, in characters."
  :type 'integer)

(defcustom my-frame-title (lambda () (concat my-user-name "’s GNU Emacs"))
  "Title of the frame."
  :type '(radio (string :tag "A static title")
                (function
                 :tag "Call this function to generate a static title")))

(defcustom my-theme-hooks '()
  "Theme hooks, see `mw-apply-theme'."
  :type '(repeat (cons (string :tag "Regexp matching the theme name")
                       (cons (choice (function :tag "Pre-hook")
                                     (const :tag "No pre-hook"))
                             (choice (function :tag "Post-hook")
                                     (const :tag "No post-hook"))))))

(defcustom my-random-theme-excludes '()
  "Exclude these themes when `apply-random-theme'."
  :type '(repeat symbol))

(defcustom my-auto-sml-theme-excludes '()
  "Don't automatically set a smart-mode-line theme when using
 these themes."
  :type '(repeat symbol))

(defcustom my-theme nil
  "The theme to use.

Can be one of these:

- The name of a theme, as a symbol.
- A list of symbols. They are applied in order. This is used when
  themes do not cover the same set of faces, and you want to
  cover the union of those sets.
- 'random, to use a random theme, respecting `my-random-theme-excludes'.
- A function to call that applies a theme.
- Nil, which means using the default theme."
  :type '(radio (const :tag "Use the default theme" nil)
                (const :tag "Use a random theme" random)
                (symbol :tag "Name of a theme")
                (repeat :tag "Apply themes in order"
                        (symbol :tag "Name of a theme"))
                function))

(defcustom my-font nil
  "The name of the font family. Nil means using the default font."
  :type '(radio (string :tag "Family name")
                (const :tag "Use default font" nil)))

(defcustom my-font-size 120
  "Pixel size of the font, multiplied by 10."
  :type 'integer)

(defcustom my-use-nerd-font nil
  "Whether to use the icons in Nerd Font."
  :type 'boolean)

(define-widget 'window-configuration 'lazy
  "A binary tree made of cons-cells and strings."
  :offset 2
  :tag "Configuration"
  :type '(choice (const :tag "No further split" nil)
                 (choice :tag "Split"
                         (repeat :tag "Columns"
                                 (cons (const column) window-configuration))
                         (repeat :tag "Rows"
                                 (cons (const row) window-configuration)))))

(defcustom my-initial-frame-configuration nil
  "See `apply-window-configuration'."
  :type 'window-configuration)

(defcustom my-emacsd-location nil
  "Change the location of what's usually ~/.emacs.d"
  :type '(radio (const :tag "No change" nil)
                (directory)))

(defcustom my-inline-completion 'company
  "Choose an inline completion package to use.

Right now the choices are 'company or nil.
"
  :type '(radio (const company)
                (const :tag "No inline completion" nil)))

(defcustom my-org-files '()
  "A list of org file that are opened by command `org-open'."
  :type '(repeat file))

(defcustom my-org-capture-file nil "The org capture file"
  :type '(radio (const :tag "No org capture file" nil) file))

(defcustom my-org-inline-css-file nil
  "Inline CSS to embed when exporting an org into HTML"
  :type '(radio (const :tag "Use default CSS" nil) file))

(defcustom my-roam-dir nil
  "Org-roam root dir"
  :type '(radio (const :tag "Don't use org-roam.") directory))

(defcustom my-splash-image nil
  "The splash image to use in the Simple Splash buffer"
  :type '(radio (const :tag "No splash image" nil) file))

(defcustom my-splash-image-scale 1.0 "The scale of splash image" :type 'float)

(defcustom my-full-screen nil "Go full screen in macOS" :type 'boolean)

(defcustom my-smart-mode-line-theme 'light
  "The theme for smart-mode-line"
  :type '(radio (const light) (const dark)))

(defcustom my-shorten-mode-string t
  "Replace mode names by symbols in the mode-line."
  :type 'boolean)

(defcustom my-fcc-dirs '((".*" . nil))
  "FCC out-going mails into folders by matching the recipient.

Example:

  '((\".*@mycompany\\.com>?\" . \"company-mails/sent_items\"))
"
  :type '(repeat (cons (string :tag "Regex to match recipient")
                       (choice (string :tag "Folder")
                               (const :tag "Don't FCC" nil)))))

(defcustom my-mail-host "localhost"
  "Hostname of the mail server" :type 'string)

(defcustom my-sendmail "/usr/bin/sendmail"
  "The sendmail program to use" :type 'string)


(defcustom my-signature-file nil
  "The file that has the email signature. See `my-sig-condition'."
  :type '(radio (const :tag "No signature" nil)
                file))

(defcustom my-signature-dir nil
  "The directory that has the email signature files. See `my-sig-condition'."
  :type '(radio (const :tag "No signature" nil)
                directory))

(defcustom my-sig-condition
  '(((header . "from")
     (pattern . ".*@mycompany\\.com>?")
     (sig-type . file))
    ((header . "from")
     (pattern . ".*@personal-mail.com>?")
     (sig-type . dir))
    ((header . "from")
     (pattern . ".*")
     (sig-type . cookie)))
  "How to choose signature when composing an email.

Each element in `my-sig-condition' is a associative list
containing three keys: header, pattern and sig-type. When
composing a mail, the field in the mail denoted by the `header'
key is matched against the regexp denoted by the `pattern' key.
If it matches, a signature from a source is chosen randomly. The
signature source is denoted by the `sig-type' key, which can be
one of the 'file, 'dir, 'fortune, or 'cookie. If it's 'file, a
signature is chosen from the file denoted by variable
`my-signature-file'; if it's 'dir, from files in
`my-signature-dir'."
  :type '(repeat
          (list
           :tag "Condition"
           (cons :format "%v"
                 (const :format "" header)
                 (string :tag "Header to match"))
           (cons :format "%v"
                 (const :format "" pattern)
                 (string :tag "Against this regexp"))
           (cons :format "%v"
                 (const :format "" sig-type)
                 (radio :tag "Source of signature"
                        (const file)
                        (const dir)
                        (const cookie)
                        (const fortune))))))

(defcustom my-from-by-to
  '((".*@mycompany\\.com>?" . "My Real Name <my.r.name@mycompany.com>")
    (".*" . "Secret Identity <secret.identity@personal-mail.com>"))
  "Set the From field by matching the value of To.

When replying an email, set the from field (to the cdr) by
matching the to field against the car."
  :type '(repeat (cons (string :tag "Match From against this regexp")
                       (string :tag "Set To to"))))

(defcustom irc-nick "Nick" "Default nickname in IRC" :type 'string)
(defcustom irc-full-name "Full Name" "Default full name in IRC" :type 'string)
(defcustom irc-password "secret" "Default password in IRC" :type 'string)

(defcustom irc-channels
  '(((host . "irc.libera.chat") (tls . t) (channels "#emacs"))
    ((host . "oftc.net") (tls . t) (channels "#channel1" "#channel2")))
  "Automatically connect to these servers and channels with `erc-start'."
  :type '(repeat :tag "Servers"
          (list
           :tag "Server and channels specification"
           (cons :format "%v"
                 (const :format "" host)
                 (string :tag "IRC server hostname"))
           (cons :format "%v"
                 (const :format "" tls)
                 (boolean :tag "Use TLS?"))
           (cons :format "%v\n"
                 (const :format "" channels)
                 (repeat :format "%v" (string :tag "Channel name"))))))

(defcustom irc-pals '("pal1" "pal2")
  "A list of pals in IRC"
  :type '(repeat (string :tag "Nickname")))

(defcustom my-notmuch-searchs nil
  "This will become `notmuch-saved-searches'."
  :type 'sexp)

(provide 'mw-customization)
