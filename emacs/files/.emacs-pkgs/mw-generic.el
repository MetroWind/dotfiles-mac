;; Stuff that doesn't fit anywhere else.

(use-package mw-lib-generic
  :bind (("M-RET" . toggle-line-wrap)
         ("C-;" . comment-sectional)
         ("%" . match-paren)
         ("M-w" . copy-line-or-region)
         ("C-<return>" . cua-or-multicursor)
         ("C-@" . cua-or-multicursor)
         ("C-S-y" . yank-with-indent))

  :config
  ;; Add timestamp to messages
  ;; (advice-add 'message :before 'ad-timestamp-message)
  )

(use-package mw-lib-window-focus
  :bind ("C-`" . maximize-or-restore-window))

(use-package multiple-cursors-core
  :commands mc/edit-lines
  :ensure multiple-cursors
  :config
  ;; http://emacs.stackexchange.com/a/9916/514
  (add-to-list 'mc--default-cmds-to-run-once 'cua-or-multicursor))

(use-package ibuffer
  :unless (or (<= emacs-major-version 21)
              (not (require 'cl-lib nil 'noerror)))
  ;; (load "ibuffer-21")

  :bind ("C-x C-b" . ibuffer)

  :config
  (setq ibuffer-saved-filter-groups
        (quote (("default"
                 ("Documents" (or (name . ".*\\.tex$")
                                  (mode . adoc-mode)
                                  (mode . markdown-mode)
                                  (derived-mode . org-mode)))
                 ("Dired" (mode . dired-mode))
                 ("Programming" (or
                                 (derived-mode . prog-mode)
                                 ))
                 ("Version control" (derived-mode . magit-mode))
                 ("Process" (derived-mode . comint-mode))
                 ("Email" (or
                           (derived-mode . message-mode)
                           (derived-mode . mail-mode)
                           (mode . notmuch-show-mode)
                           (mode . notmuch-hello-mode)
                           (mode . gnus-group-mode)
                           (mode . gnus-summary-mode)
                           (mode . gnus-article-mode)))
                 ("ERC" (mode . erc-mode))
                 ("Emacs" (or
                           (name . "^\\*scratch\\*$")
                           (name . "^\\*Messages\\*$")
                           (name . "^\\*GNU Emacs\\*$")
                           (name . "^\\*Completions\\*$")
                           (mode . apropos-mode)
                           (mode . help-mode)
                           (mode . helpful-mode)
                           ))
                 ("Temp" (or
                          (mode . helm-major-mode)))
                 ))))
  (setq ibuffer-show-empty-filter-groups nil)
  :hook (ibuffer-mode
         . (lambda ()
             (ibuffer-switch-to-saved-filter-groups "default"))))

;; add the last component of the path to the filename to distinguish
;; different files with the same name
(use-package uniquify
  :demand t
  :config
  (setq uniquify-buffer-name-style 'forward))

(use-package linum
  :commands global-linum-mode
  :unless (>= emacs-major-version 26)
  :bind ("M-n" . toggle-linum)
  :config
  ;; Acutally linum is slow, so don't use it globally.
  ;; (global-linum-mode t)
  (defun toggle-linum ()           ; Toggle line numbering
    (interactive)
    (if linum-mode (linum-mode -1)
      (linum-mode t))))

(use-package display-line-numbers
  :if (>= emacs-major-version 26)
  :bind ("M-n" . display-line-numbers-mode))

;; Protect buffers
(use-package emacs
  :hook (after-init
         .
         (lambda ()
           (dolist (buff protected-buffers)
             (with-current-buffer buff (emacs-lock-mode 'kill)))))
  :config
  (defconst protected-buffers '("*scratch*" "*Messages*")))

;; Rainbow delimiters
(use-package rainbow-delimiters
  :if (ensure-single-package-installed 'rainbow-delimiters)
  :hook
  ((cc-mode scheme-mode lisp-mode emacs-lisp-mode LaTeX-mode ConTeXt-mode)
   . 'rainbow-delimiters-mode))

;; Yet another snippet extension for Emacs.
(use-package yasnippet
  :commands (yas-activate-extra-mode yas-global-mode)
  :config
  (add-to-list 'yas-snippet-dirs (concat ModeDir "/snippets"))
  ;; Don't need bundled snippets.
  (delq 'yas-installed-snippets-dir yas-snippet-dirs)
  (yas-global-mode 1))

(use-package tsmanip
  :commands (tsmanip-timestamp-increase tsmanip-timestamp-decrease))

(if use-straight-p
    (straight-use-package
     '(beancount :type git
           :host github
           :repo "beancount/beancount-mode"
           :files ("*.el" ("term" "term/*.el") "*.texi"
                   "*.ti" ("terminfo/e" "terminfo/e/*")
                   ("terminfo/65" "terminfo/65/*")
                   ("integration" "integration/*")
                   (:exclude ".dir-locals.el" "*-tests.el")))))

(use-package beancount
  :mode ("\\.beancount\\'" . beancount-mode)
  :hook (beancount-mode . (lambda () (yas-activate-extra-mode 'beancount-mode)))
  :bind (:map beancount-mode-map
         ("S-<up>" . tsmanip-timestamp-increase)
         ("S-<down>" . tsmanip-timestamp-decrease)))

(use-package projectile
  :demand t
  :after helm-mode
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq projectile-completion-system 'helm)
  (projectile-mode 1))

(use-package twittering-mode
  :commands twit
  :config
  (setq twittering-status-format
        "%i%RT{♺}%QT{❝} @%s %FACE[font-lock-type-face]{%S} %FACE[font-lock-comment-face]{%@}\n%FOLD[    ]{%T\n%QT{+----\n%FOLD[|]{%i @%s %FACE[font-lock-type-face]{%S} %FACE[font-lock-comment-face]{%@}\n%FOLD[  ]{%T}}\n+----\n}%RT{(Retweeted from @%s)\n}")
  (setq twittering-use-native-retweet t)
  ;; This is required to store auth token.
  (setq twittering-use-master-password t))

;; Not through Elpa
(load "lilypond-init.el" t)

(use-package which-key
  :demand t
  :config
  (which-key-mode))

;; Atomic/GhostText
(use-package atomic-chrome
  :disabled
  :hook (after-init . atomic-chrome-start-server))

(use-package windmove
  :bind
  ("<M-up>" . windmove-up)
  ("M-[ 1 ; 3 a" . windmove-up)
  ("<M-down>" . windmove-down)
  ("M-[ 1 ; 3 b" . windmove-down)
  ("<M-left>" . windmove-left)
  ("M-[ 1 ; 3 d" . windmove-left)
  ("<M-right>" . windmove-right)
  ("M-[ 1 ; 3 c" . windmove-right)
  ("s-k" . windmove-up)
  ("s-j" . windmove-down)
  ("s-h" . windmove-left)
  ("s-l" . windmove-right))

(use-package buffer-move
  :ensure t
  :bind
  ("s-K" . buf-move-up)
  ("s-J" . buf-move-down)
  ("s-H" . buf-move-left)
  ("s-L" . buf-move-right)
  ("<M-S-up>" . buf-move-up)
  ("M-[ 1 ; 4 a" . buf-move-up)
  ("<M-S-down>" . buf-move-down)
  ("M-[ 1 ; 4 b" . buf-move-down)
  ("<M-S-left>" . buf-move-left)
  ("M-[ 1 ; 4 d" . buf-move-left)
  ("<M-S-right>" . buf-move-right)
  ("M-[ 1 ; 4 c" . buf-move-right))

;; This doesn’t work correctly sometimes but I’ll just put it here...
(use-package winner
  :bind
  ("C-M-h" . winner-undo)
  ("C-M-l" . winner-redo))

;; Restore previous session
(use-package session
  :ensure t
  :hook (after-init . session-initialize))

(use-package simple-splash
  :demand t
  :config
  (setq simple-splash-image my-splash-image)
  (setq simple-splash-image-scale my-splash-image-scale)
  (setq simple-splash-content
        "* Key Bindings

<s-up>, <s-down>, <s-left>, <s-right> - Change focused window to
the window in the corresponding direction.
s-k, s-j, s-h, s-l - Same thing, using Vim directional keys.
s-K, s-J, s-H, s-L - Move the current buffer to the window in the
corresponding direction, using Vim directinal keys.
s-=, s--, s-0 - Increace/decreace/reset font size.
C-` - Maximize/restore current window.

M-SPC    Start selection.
C-x C-k  Kill current buffer
M-RET    Toggle visual line mode.
C-;      At empty line: start a sectional comment; at non-empty
         line: finish a sectional comment.
%        Jump to matching parenthesis. Same as in Vim.
M-w      Copy current line if no text is selected; otherwise copy
         selected.
C-RET    Start selecting a rectangle region if no text is
         selected; otherwise start multi-cursor.
C-@      Same thing.
M-n      Toggle line number.
s-,      Open Emacs configuration directory.
")

  (simple-splash-setup-startup-hook))

(use-package smart-text
  :hook ((prog-mode text-mode) . smart-text-mode))

(use-package adoc-mode :mode "\\.adoc\\'")

(use-package flycheck
  :demand t
  :config
  (setq-default flycheck-disabled-checkers
                '(emacs-lisp emacs-lisp-checkdoc))
  (global-flycheck-mode)

  ;; Let proselint support more modes
  (flycheck-define-checker proselint
    "Flycheck checker using Proselint.

See URL `http://proselint.com/'."
    :command ("proselint" "--json" "-")
    :standard-input t
    :error-parser flycheck-proselint-parse-errors
    :modes (text-mode markdown-mode gfm-mode message-mode
            notmuch-message-mode org-mode))
  )

(use-package lilypond-mode
  :init
  (add-to-list 'load-path "/Applications/LilyPond.app/Contents/Resources/share/emacs/site-lisp")
  :mode ("\\.ly\\'" . LilyPond-mode))

(use-package telega
  :commands telega
  :config
  (setq telega-chat-use-markdown-version 2)
  (setq telega-chat-input-markups (list "markdown2" "markdown1" nil))
  (setq telega-msg-rainbow-title nil)
  (setq telega-sticker-size (cons 7 24))
  (if macp
      (setq telega-server-libs-prefix "/opt/homebrew")))

;; Https://github.com/beancount/beancount-mode
(use-package beancount
  :mode ("\\.beancount\\'" . beancount-mode)
  :bind (:map beancount-mode-map
              ("M-n" . outline-next-visible-heading)
              ("M-p" . outline-previous-visible-heading)
              ("<backtab>" . beancount-toggle-hide))
  :init
  (add-hook 'beancount-mode-hook #'outline-minor-mode)
  ;; Don’t auto-align amounts.
  (add-hook 'beancount-mode-hook
            (lambda () (setq-local electric-indent-chars nil)))
  :config
  ;; automatically determine the minimum column that will allow to
  ;; align all amounts
  (setq beancount-number-alignment-column 0)
  (setq beancount-hide nil)
  (defun beancount-toggle-hide ()
    (interactive)
    (if beancount-hide
        (progn
          (outline-show-all)
          (setq beancount-hide nil))
      (outline-hide-body)
      (setq beancount-hide t))))

(use-package helpful
  :ensure t
  :bind
  ("C-h f" . helpful-callable)
  ("C-h v" . helpful-variable)
  ("C-h k" . helpful-key))
;; NOTE: helpful and helm-eval automatically load edebug, which
;; interferes with C-x C-a in elisp-mode.

(use-package pyim
  :config
  (setq default-input-method "pyim")
  (pyim-default-scheme 'microsoft-shuangpin)
  (setq pyim-page-length 5)
  (require 'pyim-basedict)
  (pyim-basedict-enable))

(use-package emacs
  :config
  (setq word-wrap-by-category t))

(use-package visual-fill-column
  :hook (visual-line-mode
         .
         (lambda ()
           (if visual-line-mode
               (visual-fill-column-mode 1)
             (visual-fill-column-mode -1))))
  :config
  (setq-default visual-fill-column-width 80))

(if use-straight-p
    (straight-use-package
     '(eat :type git
           :host codeberg
           :repo "akib/emacs-eat"
           :files ("*.el" ("term" "term/*.el") "*.texi"
                   "*.ti" ("terminfo/e" "terminfo/e/*")
                   ("terminfo/65" "terminfo/65/*")
                   ("integration" "integration/*")
                   (:exclude ".dir-locals.el" "*-tests.el")))))

(use-package atomic-chrome
  :ensure t
  :config
  (atomic-chrome-start-server))

(use-package goggles
  :ensure t
  :hook ((prog-mode text-mode) . goggles-mode)
  :config
  (setq-default goggles-pulse t)
  (set-face-attribute
   'goggles-added nil
   :background 'unspecified
   :inherit 'region)
  (set-face-attribute
   'goggles-changed nil
   :background 'unspecified
   :inherit 'region)
  (set-face-attribute
   'goggles-removed nil
   :background 'unspecified
   :inherit 'region))

(provide 'mw-generic)
