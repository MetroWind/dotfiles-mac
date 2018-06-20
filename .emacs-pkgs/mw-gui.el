(use-package emacs
  :if window-system
  :config
  ;; Size
  (add-to-list 'default-frame-alist (cons 'height my-frame-height))
  (add-to-list 'default-frame-alist (cons 'width my-frame-width))

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

  ;; Load theme
  (if my-theme
      (load-theme my-theme t))
)

;; Go full screen and split
(use-package emacs
  :if (and window-system my-full-screen)
  :hook (after-init
         .
         (lambda ()
           (toggle-frame-fullscreen)
           (split-window-horizontally)
           (split-window-horizontally)
           (select-window (previous-window))
           (split-window-vertically)
           (balance-windows))))

(provide 'mw-gui)
