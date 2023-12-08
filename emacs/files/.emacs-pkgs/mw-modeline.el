(require 'cl)

;; Shorter modeline
(defvar mode-line-cleaner-alist
  (list '(auto-complete-mode . "α")
        '(git-gutter-mode . "γ")
        '(company-mode . "κ")
        '(abbrev-mode . "")
        (cons 'helm-mode (if my-use-nerd-font "\U000f0d93 " "^"))
        (cons 'whitespace-mode (if my-use-nerd-font "\U000f1050 " "_"))
        '(hs-minor-mode . "")
        (cons 'yas-minor-mode (if my-use-nerd-font "\U0000eb66 " ""))
        '(hi-lock-mode . "")
        '(flyspell-mode . "✓")
        '(auto-fill-function . "↵")
        (cons 'projectile-mode (if my-use-nerd-font "\U0000f502 " "p"))
        '(which-key-mode . "")
        '(auto-revert-mode . "⤾ ")
        '(eldoc-mode . "?")
        (cons 'visual-line-mode (if my-use-nerd-font "\U000f05b6 " "↩"))
        ;; Major modes
        '(text-mode . "¶")
        '(lisp-interaction-mode . "λ")
        (cons 'python-mode (if my-use-nerd-font "\U0000e73c " "π"))
        '(emacs-lisp-mode . "Λ")
        (cons 'c++-mode (if my-use-nerd-font "\U0000e61d " "C++"))
        '(c-mode . "C")
        '(sh-mode . "#")
        '(help-mode . "?")
        (cons 'dired-mode (if my-use-nerd-font "\U0000f4d3 " "ls"))
        '(emacs-lock-mode . "")
        '(mpdel-mode . "▶")
        '(lsp-mode . "")
        '(editorconfig-mode . "")
        (cons 'tree-sitter-mode (if my-use-nerd-font "\U0000f1bb " "T"))
        (cons 'magit-status-mode (if my-use-nerd-font "\U000f02a2 " "Mg"))
        (cons 'magit-rev-mode (if my-use-nerd-font "\U000f0718 " ""))
        (cons 'magit-diff-mode (if my-use-nerd-font "\U0000f47f " ""))
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

(if (bound-and-true 'my-shorten-mode-string)
    (add-hook 'after-change-major-mode-hook 'clean-mode-line))

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
  (setq sml/mode-width 'full)
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
