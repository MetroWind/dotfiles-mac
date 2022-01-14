(require 'cl)

;; Shorter modeline
(defvar mode-line-cleaner-alist
  '((auto-complete-mode . "α")
    (git-gutter-mode . "γ")
    (company-mode . "κ")
    (abbrev-mode . "")
    (helm-mode . "^")
    (whitespace-mode . "_")
    (hs-minor-mode . "±")
    (yas-minor-mode . "Y")
    (hi-lock-mode . "")
    (flyspell-mode . "✓")
    (auto-fill-function . "↵")
    (projectile-mode . "p")
    (which-key-mode . "")
    (auto-revert-mode . "∀")
    (eldoc-mode . "?")
    ;; Major modes
    (lisp-interaction-mode . "λ")
    (python-mode . "π")
    (emacs-lisp-mode . "Λ")
    (c++-mode . "C++")
    (c-mode . "C")
    (sh-mode . "!")
    (help-mode . "?")
    (dired-mode . "ls")
    (emacs-lock-mode . "")
    (mpdel-mode . "▶")
    (lsp-mode . "")
    (editorconfig-mode . "")
    )
  "Alist for `clean-mode-line'.
When you add a new element to the alist, keep in mind that you
must pass the correct minor/major mode symbol and a string you
want to use in the modeline *in lieu of* the original.")

(defun clean-mode-line ()
  (interactive)
  (loop for cleaner in mode-line-cleaner-alist
        do (let* ((mode (car cleaner))
                 (mode-str (cdr cleaner))
                 (old-mode-str (cdr (assq mode minor-mode-alist))))
             (when old-mode-str
                 (setcar old-mode-str mode-str))
               ;; major mode
             (when (eq mode major-mode)
               (setq mode-name mode-str)))))

(if (boundp 'my-shorten-mode-string)
    (if my-shorten-mode-string
        (add-hook 'after-change-major-mode-hook 'clean-mode-line)))

;; Smart modeline
(use-package smart-mode-line
  :if (ensure-single-package-installed 'smart-mode-line)
  :demand t
  :config
  (if (boundp 'my-smart-mode-line-theme)
      (if my-smart-mode-line-theme
          (setq sml/theme my-smart-mode-line-theme)))
  (setq sml/show-file-name nil)
  (setq sml/name-width 35)
  (setq sml/mode-width 'right)
  (setq sml/no-confirm-load-theme t)
  ;; Hide dir name.
  (setq sml/replacer-regexp-list '((".*" "")))
  (sml/setup))

;; Add a small image to the mode line for current window~~
(if (and my-active-modeline-image (> my-active-modeline-image-width 0))
    (progn
      (defconst active-modeline-image
        (create-image my-active-modeline-image 'png nil
                      :ascent 'center))
      (defconst mode-line-img
        (propertize (make-string my-active-modeline-image-width #x20)
                    'display active-modeline-image))

      (defvar my-current-win nil)
      (defun set-current-window (windows)
        (when (not (minibuffer-window-active-p (frame-selected-window)))
          (setq my-current-win (selected-window))))
      (add-function :before pre-redisplay-function #'set-current-window)

      (add-hook 'after-init-hook
                (lambda ()
                  (setq-default
                   mode-line-format
                   (append mode-line-format
                           `((:eval
                              (if (eq my-current-win (get-buffer-window))
                                  ,mode-line-img)))))))))

(provide 'mw-modeline)
