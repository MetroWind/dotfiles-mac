;; Stuff that doesn't fit anywhere else.

(use-package mw-lib-generic
  :commands enable-smart-quote-in-map
  :bind (("M-RET" . toggle-line-wrap)
         ("C-;" . comment-sectional)
         ("%" . match-paren)
         ("M-w" . copy-line-or-region)
         ("C-<return>" . cua-or-multicursor)
         ("C-@" . cua-or-multicursor)))

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
                 ("TeX" (name . ".*\\.tex$"))
                 ("Dired" (mode . dired-mode))
                 ("Programming" (or
                                 (mode . python-mode)
                                 (mode . pov-mode)
                                 (mode . asy-mode)
                                 (mode . c-mode)
                                 (mode . c++-mode)
                                 (mode . emacs-lisp-mode)
                                 (mode . scheme-mode)
                                 (mode . sh-mode)
                                 (mode . makefile-mode)
                                 ))
                 ("Process" (mode . comint-mode))
                 ("Gnus" (or
                          (mode . message-mode)
                          (mode . bbdb-mode)
                          (mode . mail-mode)
                          (mode . gnus-group-mode)
                          (mode . gnus-summary-mode)
                          (mode . gnus-article-mode)
                          (name . "^\\.bbdb$")
                          (name . "^\\.newsrc-dribble")))
                 ("ERC" (mode . erc-mode))
                 ("Planner" (or
                             (name . "^\\*Calendar\\*$")
                             (name . "^diary$")
                             (mode . muse-mode)))
                 ("Emacs" (or
                           (name . "^\\*scratch\\*$")
                           (name . "^\\*Messages\\*$")
                           (name . "^\\*GNU Emacs\\*$")
                           (name . "^\\*Completions\\*$")
                           (mode . apropos-mode)
                           (mode . help-mode)
                           ))
                 ))))
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
  :bind ("M-n" . toggle-linum)
  :config
  ;; Acutally linum is slow, so don't use it globally.
  ;; (global-linum-mode t)
  (defun toggle-linum ()           ; Toggle line numbering
    (interactive)
    (if linum-mode (linum-mode -1)
      (linum-mode t))))

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

(use-package beancount
  :mode ("\\.beancount\\'" . beancount-mode)
  :hook (beancount-mode . (lambda () (yas-activate-extra-mode 'beancount-mode))))

(use-package projectile
  :demand t
  :after helm-mode
  :config
  (projectile-global-mode)
  (setq projectile-completion-system 'helm)
  (helm-projectile-on))

(use-package twittering-mode
  :commands twit
  :config
  (setq twittering-status-format
        "%i%RT{♺}%QT{❝} @%s %FACE[font-lock-type-face]{%S} %FACE[font-lock-comment-face]{%@}\n%FOLD[    ]{%T\n%QT{+----\n%FOLD[|]{%i @%s %FACE[font-lock-type-face]{%S} %FACE[font-lock-comment-face]{%@}\n%FOLD[  ]{%T}}\n+----\n}%RT{(Retweeted from @%s)\n}"))

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
  :demand t
  :bind
  ("<s-up>" . windmove-up)
  ("<s-down>" . windmove-down)
  ("<s-left>" . windmove-left)
  ("<s-right>" . windmove-right)
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
  ("s-L" . buf-move-right))

;; Restore previous session
(use-package session
  :ensure t
  :hook (after-init . session-initialize))

(use-package simple-splash
  :demand t
  :config
  (setq simple-splash-image my-splash-image)
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

(provide 'mw-generic)
