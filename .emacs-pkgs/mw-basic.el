(use-package emacs
  :demand t
  :hook
  ;; Remove useless whitespaces when save
  (before-save . delete-trailing-whitespace)

  :config
  (message "Doing basic configs...")
  (setq custom-file "~/.emacs-custom.el") ; Go away, custom!!!  Go away!!
  (setenv "COLUMNS" "80")                 ; Kill the dumb COLUMN warning from `ls'
  (ignore-errors (setq user-full-name my-user-name))
  (ignore-errors (setq user-mail-address my-email))
  (setq default-major-mode 'text-mode)
  (setq-default tab-width 4)
  (setq-default indent-tabs-mode nil)
  (setq column-number-mode t)
  (setq kill-ring-max 255)
  (setq next-screen-context-lines 5)
  ;; (setq sentence-end "\\([。！？；]\\|……\\|[.?!;][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")
  (setq sentence-end-double-space nil)
  (ignore-errors (setq frame-title-format my-frame-title))
  ;; Don't bring partially a visible line to fully visible before
  ;; scrolling.
  (setq auto-window-vscroll nil)
  ;; let the delete key delete foreward
  (if linux-x-p
      (normal-erase-is-backspace-mode 1))
  ;; X primary selection has priority.  For emacs 24.1
  (if linux-x-p
      (setq x-select-enable-primary t))

  ;; Add a newline in the end if none.
  (setq require-final-newline t)

  ;; 所有的 yes or no 全都用 y or n 代替 :-)
  (fset 'yes-or-no-p 'y-or-n-p)
  ;; Grammar hightlighting
  (global-font-lock-mode 1)
  ;; Turn on auto save function
  (setq auto-save-default t)
  ;; Replace selected text when typing
  (delete-selection-mode t)
  ;; Default input method for `C-\'
  (setq default-input-method "TeX")
  ;; Remove *bar...
  (progn
    (if window-system
        (progn
          (tool-bar-mode 0)
          (scroll-bar-mode -1)))
    (if (not macp)
        (menu-bar-mode 0)))
  ;; No sound alarm!  Set `visible-bell' to t would disable bell sound.
  ;; However, on OS X 10.11 there will be a white box showing seemingly
  ;; enlarged text and obscure the Emacs window, when the bell is sound.
  ;; It takes a redraw to get rid of the white box.  To test this, go to
  ;; the end of some buffer, and go down one line; make sure there are
  ;; some text around the center of the Emacs window.  A work around
  ;; would be
  ;; (http://stuff-things.net/2015/10/05/emacs-visible-bell-work-around-on-os-x-el-capitan/):
  ;; (setq visible-bell nil) ;; The default
  ;; (setq ring-bell-function 'ignore)
  (setq visible-bell nil)

  (setq case-fold-search t)
  ;; debug when error
  (setq debug-on-error nil)
  ;; In Transient Mark mode, when the mark is active, the region is
  ;; highlighted.
  (setq transient-mark-mode t)
  ;; Backup Control
  (setq make-backup-files t)
  (setq backup-directory-alist '(("." . "~/.backup/")))
  (setq backup-by-copying t)

  ;; Delete duplicates in minibuffer history
  (setq history-delete-duplicates t)

  ;; Change the initial message in `*scratch'
  (setq initial-scratch-message
        ";; What is thy bidding, my master?\n\n")
  ;; Do not add a new string to `kill-ring' when it is the same as the
  ;; last one.
  (setq kill-do-not-save-duplicates t)
  ;; Don't open new frame when open file by dragging and emacsclient in
  ;; Mac.
  (if macp (setq ns-pop-up-frames nil))
  ;; PATH
  (defun stripStr(str)
    "Strip leading and tailing whitespace from STR."
    (replace-regexp-in-string (rx (or (: bos (* (any " \t\n")))
                                      (: (* (any " \t\n")) eos)))
                              ""
                              str))
  (if macp
      (let ((Path (stripStr (shell-command-to-string "zsh -l -c 'echo $PATH'"))))
        (setenv "PATH" Path)
        (setq exec-path (split-string Path ":" t))))


  ;; (electric-quote-mode)

  ;; Save 256 recent files
  (require 'recentf)
  (setq recentf-max-saved-items 256)

  ;; Completely disable the yes/no graphical dialogs.
  ;; https://superuser.com/a/125571
  (defadvice yes-or-no-p (around prevent-dialog activate)
    "Prevent yes-or-no-p from activating a dialog"
    (let ((use-dialog-box nil))
      ad-do-it))
  (defadvice y-or-n-p (around prevent-dialog-yorn activate)
    "Prevent y-or-n-p from activating a dialog"
    (let ((use-dialog-box nil))
      ad-do-it))

  ;; Automatically kill old buffers
  (require 'midnight)

  ;; Info dir
  (add-to-list 'Info-directory-list (concat ModeDir "/info"))

  ;; Security
  (setq tls-checktrust t)
  (let ((cas "/etc/ssl/certs/cacert.pem"))
    (setq tls-program
          (list
           (format "gnutls-cli%s --x509cafile %s -p %%p %%h"
                   (if (eq window-system 'w32) ".exe" "") cas)))
    (setq gnutls-verify-error t)
    (setq gnutls-trustfiles (list cas)))

  :bind
  (("s-p" . previous-buffer)
   ("s-n" . next-buffer)
   ("M-SPC" . set-mark-command)
   ("C-x C-k" . kill-this-buffer)
   ("s-v" . clipboard-yank)
   ("C-x M-f" . find-file-at-point)
   ("C-x M-r" . revert-buffer)
   ("C-x =" . balance-windows)
   ("C-x +" . what-cursor-position)
   ("s-," . (lambda () (interactive) (find-file ModeDir)))

   ;; Change font size
   ("s-=" . text-scale-adjust)
   ("s--" . text-scale-adjust)
   ("s-0" . text-scale-adjust)

   )
  )

(use-package bookmark
  :if linux-x-p
  :bind ("<C-XF86KbdBrightnessUp>" . list-bookmarks))
(use-package bookmark
  :unless linux-x-p
  :bind ("C-<f10>" . 'list-bookmarks))

(use-package auto-fill-mode
  :hook text-mode
  :config
  (setq adaptive-fill-regexp "[ \t]+\\|[ \t]*\\([0-9]+\\.\\|\\*+\\)[ \t]*"))

(use-package tramp
  :config
  ;; Let tramp recognize passcode prompt on Kraken
  ;; (setq tramp-password-prompt-regexp
  ;;       "^.*\\([pP]assword\\|[pP]assphrase\\|PASSCODE\\).*:\0? *")
  ;; Non-nil if searches and matches should ignore case.
  (setq tramp-use-ssh-controlmaster-options nil))

(use-package fringe
  :if window-system
  :config
  (setq fringe-mode 'left-only)
  (setq-default right-fringe-width 0)
  (setq default-indicate-buffer-boundaries
        '((top . left) (t . left))))

(use-package flyspell
  ;; Prevent flyspell to overide this key.
  :bind (:map flyspell-mode-map ("C-;" . nil))
  :hook (text-mode . flyspell-mode)
  :config
  (setq-default ispell-program-name "aspell"))

(use-package comint
  :config
  ;; Shell Mode settings
  (setq comint-scroll-to-bottom-on-input t)
  (setq comint-prompt-read-only t)
  (setq comint-input-ignoredups t)

  ;; interpret ansi color sequences
  :hook (shell-mode . ansi-color-for-comint-mode-on))

(use-package paren
  :demand t
  :config
  ;;Auto Show Matching Brace
  (show-paren-mode 1))

(use-package woman
  :commands woman
  :config
  ;; Let woman not open new frame
  (setq woman-use-own-frame nil)
  (setq woman-fill-column 75))

(use-package printing
  :disabled
  :config
  ;; Don't know why...
  (pr-update-menus))

(use-package cua-rect
  :commands (cua-rectangle-mark-mode cua--extract-rectangle)
  ;; :demand t
  :init
  ;; Have to set this before enabling cua mode, otherwise cua will force
  ;; set C-<return> to its rectangle function.
  (setq cua-rectangle-mark-key (kbd "C-S-<return>"))
  (setq cua-enable-cua-keys nil)          ; Disable win keys

  :config
  (cua-mode t))

(use-package time-stamp
  :hook before-save)

(use-package cookie1
  :commands cookie
  :config
  (setq cookie-file (concat ModeDir "/fortune.txt")))

(use-package message
  :mode ("/mutt-.*-[0-9\\-]+" . message-mode))

(use-package dired
  :bind (:map dired-mode-map
           ("C-o" . open-file-with-default-thing)
           ("M-o" . open-tmux-in-current-dir))
  :config
  (setq dired-recursive-deletes 'always)
  (setq dired-recursive-copies 'always)
  (setq dired-dwim-target t)
  (setq insert-directory-program "gls")
  (setq dired-listing-switches "-lXGh --group-directories-first")
  (setq delete-by-moving-to-trash t)
  (defun system-move-file-to-trash (file)
    "Use \"trash\" to move FILE to the system trash.
When using Homebrew, install it using \"brew install trash\"."
    (call-process (executable-find "trash")
                  nil 0 nil
                  file))

  (defun open-file-with-default-thing ()
    "Open the current file or dired marked files in external app."
    (interactive)
    (let ( doIt
           (myFileList
            (cond
             ((string-equal major-mode "dired-mode") (dired-get-marked-files))
             (t (list (buffer-file-name))) ) ) )

      (setq doIt (if (<= (length myFileList) 5)
                     t
                   (y-or-n-p "Open more than 5 files?") ) )

      (when doIt
        (cond
         ((string-equal system-type "windows-nt")
          (mapc (lambda (fPath) (w32-shell-execute "open" (replace-regexp-in-string "/" "\\" fPath t t)) ) myFileList)
          )
         ((string-equal system-type "darwin")
          (mapc (lambda (fPath) (shell-command (format "open \"%s\"" fPath)) )  myFileList) )
         ((string-equal system-type "gnu/linux")
          (mapc (lambda (fPath) (let ((process-connection-type nil)) (start-process "" nil "xdg-open" fPath)) ) myFileList) ) ) ) ) )
  (defun open-tmux-in-current-dir ()
    "Open a new Tmux window in current directory."
    (interactive)
    (shell-command "tmux new-window")))

(provide 'mw-basic)
