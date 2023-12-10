(defgroup my nil "Customization group for my configuration")

(defcustom my-user-name "My Name"
  "Name of user. Used in Email and frame title."
  :type '(string))
(defcustom my-active-modeline-image nil
  "A path of a small image file. If non-nil, it will be displayed in
the modeline."
  :type '(radio (file :tag "Path of the image")
                (const :tag "No image (nil)" nil)))
(defcustom my-active-modeline-image-width 0
  "Pixel width of the modeline image."
  :type '(integer))
(defcustom my-frame-height 60
  "Height of the first frame, in characters."
  :type '(integer))
(defcustom my-frame-width 100
  "Width of the first frame, in characters."
  :type '(integer))
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
  "The theme to use. Can be one of these:

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
                (repeat (symbol :tag "Name of a theme"))
                function))
(defcustom my-font nil
  "The name of the font family. Nil means using the default font."
  :type '(radio (string :tag "Family name")
                (const :tag "Use default font" nil)))
(defcustom my-font-size 120
  "Pixel size of the font, multiplied by 10."
  :type '(integer))

(define-widget 'window-configuration 'lazy
  "A binary tree made of cons-cells and strings."
  :offset 2
  :tag "Configuration"
  :type '(choice (const :tag "No further split" nil)
                 (choice :tag "Split"
                  (repeat :tag "Columns" (cons (const column) window-configuration))
                  (repeat :tag "Rows" (cons (const row) window-configuration)))))

(defcustom my-initial-frame-configuration nil
  "Pixel size of the font, multiplied by 10."
  :type 'window-configuration)

(provide 'mw-customization)
