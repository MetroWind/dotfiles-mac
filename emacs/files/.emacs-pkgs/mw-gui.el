(require 'mw-theme-utils)
(require 'mw-lib-generic)

(if use-straight-p
    (straight-use-package
     '(monte-carlo-theme :type git :host github :repo "MetroWind/monte-carlo-theme")))

(use-package ligature
  :if (not macp)
  :ensure t
  :hook (after-init . global-ligature-mode))

(use-package emacs
  :if window-system
  :bind (("<f5>" . toggle-old-man))
  :config
  ;; Size
  (add-to-list 'default-frame-alist (cons 'height my-frame-height))
  (add-to-list 'default-frame-alist (cons 'width my-frame-width))
  ;; Remove frame border
  (add-to-list 'default-frame-alist (cons 'internal-border-width 0))
  (add-to-list 'default-frame-alist (cons 'border-width 0))
  (if (>= emacs-major-version 29)
      (progn
        (set-frame-parameter nil 'alpha-background 85)
        (add-to-list 'default-frame-alist (cons 'alpha-background 85))))

  ;; Font
  (if my-font
      (set-face-attribute
       'default nil
       :family my-font
       :height my-font-size
       :width my-font-width
       :weight my-font-weight)
    (set-face-attribute
     'default nil
     :height my-font-size
     :weight 'normal))

  ;; Font fallback
  ;; (set-fontset-font
  ;;  (frame-parameter nil 'font)
  ;;  (cons my-font-fallback-boundary #xe007f)
  ;;  (font-spec :family my-unicode-font))

  ;; (add-to-list 'face-font-rescale-alist '("Noto Sans Mono CJK SC" . 1.1))

  ;; preserve the size of the frames when changeing fonts and stuff.
  (if (> emacs-major-version 24)
      (setq frame-inhibit-implied-resize t))

  (if (and macp (emacs-version>= 24 4))
      (progn
        (setq ns-use-native-fullscreen t)
        (if my-full-screen
            (set-frame-parameter nil 'fullscreen 'fullboth))))

  (if (and macp (>= emacs-major-version 26))
      (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t)))

  (defun enable-old-man ()
    (interactive)
    (set-face-attribute 'default nil
      :height (truncate (* (with-default 'my-font-size 100) 3))))

  (defun disable-old-man ()
    (interactive)
    (set-face-attribute 'default nil :height (with-default 'my-font-size 100)))

  (defun toggle-old-man ()
    (interactive)
    (if (with-default 'is-old-man nil)
        (progn
          (disable-old-man)
          (setq is-old-man nil))
      (enable-old-man)
      (setq is-old-man t)))
)

;; Config for Emacs in terminal
(use-package emacs
  :if (not window-system)
  :hook (tty-setup
         .
         (lambda ()
           (message "Setting TTY...")
           ;; By default in terminal the vertical divider between
           ;; windows is a “|” char. Change it to a space.
           (add-hook 'window-configuration-change-hook
                     (lambda ()
                       (let ((display-table (or buffer-display-table
                                                standard-display-table)))
                         (set-display-table-slot display-table 5 ? ))))))
  :config
  ;; (xterm-mouse-mode)
  )

(use-package emacs
  :hook (after-init
         .
         (lambda ()
           (apply-window-configuration my-initial-frame-configuration))))

;; Ligature
(use-package ligature
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("->" "=>" "//" "://"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures
   'prog-mode
   '("==" "===" "<=" ">=" "!=" "!==" "##" "###" "####" "${" "&&" "%%"
      "||" "!!" "??" "<!--" "-->" ">>" "<<" ">>>" "<<<" "/*" "*/" "?."
      "?:" "::" "```" "+=" "-=" "*=" "/=" "--" "---" "**" "***"))
  ;; Enables ligature checks globally in all buffers. You can also do
  ;; it per mode with `ligature-mode'.
  (global-ligature-mode t))

;; Recommended themes: flucui, lab, notink, monte-carlo, ef, ample,
;; anti-zenburn, apropospriate, badwolf, challenger-deep, chocolate,
;; dracula, kaolin, modus, moe, solarized, srcery, subatomic.
(use-package emacs
  :config
  (defun apply-random-theme ()
    "Apply a random theme."
    (interactive)
    (let ((random-excludes (with-default 'my-random-theme-excludes nil))
          (theme-hooks (with-default 'my-theme-hooks nil))
          (sml-excludes (with-default 'my-auto-sml-theme-excludes nil)))
      (mw-apply-random-theme random-excludes theme-hooks sml-excludes)))

  (defun apply-theme (theme)
    "Apply a theme"
    (interactive
     (list
      (intern (completing-read
               "Load custom theme: "
               (mapcar #'symbol-name (custom-available-themes))))))
    (unless (custom-theme-name-valid-p theme)
      (error "Invalid theme name `%s'" theme))

    (let ((theme-hooks (with-default 'my-theme-hooks nil))
          (sml-excludes (with-default 'my-auto-sml-theme-excludes nil)))
      (mw-apply-theme theme theme-hooks sml-excludes))))

;; Load theme
(if (bound-and-true 'my-theme)
    (progn
      (if (not (tty-color-24bit (list 0 0 0)))
          (progn
            (defun on-after-init ()
              (unless (display-graphic-p (selected-frame))
                (set-face-background 'default "unspecified-bg"
                                     (selected-frame))))
            (add-hook 'window-setup-hook 'on-after-init)))

      (let ((theme-hooks (with-default 'my-theme-hooks nil))
            (sml-excludes (with-default 'my-auto-sml-theme-excludes nil)))
        (cl-typecase my-theme
          (symbol
           (if (equal my-theme 'random)
               (progn
                 (message "Loading a random theme...")
                 (mw-apply-random-theme
                  my-random-theme-excludes
                  my-theme-hooks
                  my-auto-sml-theme-excludes))
             (message (format "Loading theme %s..." my-theme))
             (mw-apply-theme my-theme theme-hooks sml-excludes)))
          (list
           (message (format "Loading themes %s..." my-theme))
           (dolist (theme my-theme)
                  (mw-apply-theme theme theme-hooks sml-excludes)))
          (function (funcall my-theme))
          (t
           (message "Invalid type of `my-theme'."))))))

(provide 'mw-gui)
