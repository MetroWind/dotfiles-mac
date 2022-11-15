(require 'mw-theme-utils)

(use-package emacs
  :if window-system
  :config
  ;; Size
  (add-to-list 'default-frame-alist (cons 'height my-frame-height))
  (add-to-list 'default-frame-alist (cons 'width my-frame-width))
  ;; Remove frame border
  (add-to-list 'default-frame-alist (cons 'internal-border-width 0))
  (add-to-list 'default-frame-alist (cons 'border-width 0))

  ;; Font
  (if my-font
      (set-face-attribute
       'default nil
       :family my-font
       :height my-font-size
       :weight 'normal)
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

  (if (and macp (>= emacs-major-version 26))
      (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t)))
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

;; Go full screen and split
(use-package emacs
  :if (and window-system my-full-screen)
  :hook (after-init
         .
         (lambda ()
           (if truemacp
               (setq initial-frame-alist '((fullscreen . fullscreen)))
             (toggle-frame-fullscreen))
           (split-window-horizontally)
           (split-window-horizontally)
           (select-window (previous-window))
           (split-window-vertically)
           (balance-windows))))

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
(if (not (null-or-unboundp 'my-theme))
    (progn
      (if (not (tty-color-24bit (list 0 0 0)))
          (progn
            (defun on-after-init ()
              (unless (display-graphic-p (selected-frame))
                (set-face-background 'default "unspecified-bg"
                                     (selected-frame))))
            (add-hook 'window-setup-hook 'on-after-init)))

      (message (format "Loading theme(s) %s..." my-theme))
      (let ((theme-hooks (with-default 'my-theme-hooks nil))
            (sml-excludes (with-default 'my-auto-sml-theme-excludes nil)))
        (cl-typecase my-theme
          (function (funcall my-theme))
          (list (dolist (theme my-theme)
                  (apply-theme theme theme-hooks sml-excludes)))
          (t (apply-theme my-theme theme-hooks sml-excludes))))))

(provide 'mw-gui)
