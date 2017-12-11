;; -*- coding: utf-8; -*-
;; What are we using?? (from http://www.xsteve.at/prg/emacs/.emacs.txt)
(require 'cl)
(defconst win32p
    (eq system-type 'windows-nt)
  "Are we running on a WinTel system?")
(defconst aquap (featurep 'aquamacs) "Are we using AquaEmacs?")
(defconst macp (eq system-type 'darwin) "Are we running in Mac OS?")
(defconst cygwinp
    (eq system-type 'cygwin)
  "Are we running on a WinTel cygwin system?")
(defconst linuxp
    (or (eq system-type 'gnu/linux)
        (eq system-type 'linux))
  "Are we running on a GNU/Linux system?")
(defconst unixp
  (or linuxp
      (eq system-type 'usg-unix-v)
      (eq system-type 'berkeley-unix))
  "Are we running unix")
(defconst linux-x-p
    (and window-system linuxp)
  "Are we running under X on a GNU/Linux system?")
;; Aquamacs settings
(if aquap
    ((lambda ()
       (setq mac-command-modifier 'super)
       (setq mac-option-modifier 'meta)
       (setq mac-input-method-mode t)
       (setq default-input-method "MacOSX")
       (one-buffer-one-frame-mode -1)
       )))
(defconst use-pkg-p (>= emacs-major-version 24))

;; ========== Add load path =========================================>
(defconst ModeDir (expand-file-name "~/.emacs-pkgs"))
(add-to-list 'load-path ModeDir)
(if (not use-pkg-p)
    (let* ((Moodes
            '("helm" "company-mode" "magit"))
           (Modes (if (>= emacs-major-version 24)
                      Moodes
                    (cons "emacs24-subst" Moodes))))
      (dolist (Mode Modes)
        (add-to-list 'load-path (expand-file-name
                                 (concat ModeDir "/" Mode))))))

(if (>= emacs-major-version 24)
    (add-to-list 'custom-theme-load-path 
                 (concat ModeDir "/themes/")))
(add-to-list 'load-path (concat ModeDir "/notmuch"))
(add-to-list 'load-path (concat ModeDir "/lilypond"))

(load "emacs-user" t)
;; ========== Packages ==============================================>
(if use-pkg-p
    (progn
      (require 'package)
      (setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                               ("melpa" . "http://melpa.org/packages/")))
      (package-initialize)
      ;; http://stackoverflow.com/a/10095853/782130
      (defun ensure-package-installed (&rest packages)
        "Assure every package is installed, ask for installation if it’s not.
Return a list of installed packages or nil for every skipped package."
        (mapcar
         (lambda (package)
           ;; (package-installed-p 'evil)
           (if (package-installed-p package)
               nil
             (if (y-or-n-p (format "Package %s is missing. Install it? " package))
                 (package-install package)
               package)))
         packages))

      ;; make sure to have downloaded archive description.
      ;; Or use package-archive-contents as suggested by Nicolas Dudebout
      (or (file-exists-p package-user-dir)
          (package-refresh-contents))

      (ensure-package-installed 'session 'linum 'whitespace
       'company 'company-jedi 'company-quickhelp 'web-mode 'magit
       'helm 'helm-gtags 'multiple-cursors 'yasnippet 'auctex
       'smart-mode-line 'ggtags 'rainbow-delimiters 'flycheck
       'cmake-mode 'indent-guide 'scss-mode 'markdown-mode 'which-key
       'buffer-move)))

;; ========== Coding and Language ===================================>
(if (<= emacs-major-version 22)
    (progn
      (set-selection-coding-system 'utf-8)
      (set-clipboard-coding-system 'utf-8)
      (set-keyboard-coding-system 'utf-8) ; if utf8, unable to use input method.
      (set-language-environment 'utf-8)
      (setq locale-coding-system 'utf-8)
      (setq current-language-environment "utf-8")))
;;=========== Some basic settings ===================================>
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
(setq adaptive-fill-regexp "[ \t]+\\|[ \t]*\\([0-9]+\\.\\|\\*+\\)[ \t]*")
;; let the delete key delete foreward
(if linux-x-p
    (normal-erase-is-backspace-mode 1))
;; X primary selection has priority.  For emacs 24.1
(if linux-x-p
    (setq x-select-enable-primary t))
(add-hook 'text-mode-hook
		  (lambda () (auto-fill-mode t)))

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

;; Let tramp recognize passcode prompt on Kraken
(setq tramp-password-prompt-regexp
      "^.*\\([pP]assword\\|[pP]assphrase\\|PASSCODE\\).*:\0? *")
;; Non-nil if searches and matches should ignore case.
(setq case-fold-search t)
;; debug when error
(setq debug-on-error nil)
;; Fringe
(if window-system
    (progn
      (setq fringe-mode 'left-only)
      (setq-default right-fringe-width 0)
      (setq default-indicate-buffer-boundaries
            '((top . left) (t . left)))))
;; In Transient Mark mode, when the mark is active, the region is
;; highlighted.
(setq transient-mark-mode t)
;; Backup Control
(setq make-backup-files t)
(setq backup-directory-alist '(("." . "~/.backup/")))
(setq backup-by-copying t)

(setq-default ispell-program-name "aspell")
(add-hook 'text-mode-hook 'flyspell-mode)
;; Delete duplicates in minibuffer history
(setq history-delete-duplicates t)

;; Version Control Settings
(if (>= emacs-major-version 22)
  (progn
    (require 'vc-git)
    (when (featurep 'vc-git) (add-to-list 'vc-handled-backends 'git))
    ;; (require 'magit)
    (autoload 'magit-status "magit" "Load Magit" t)
))
;; Shell Mode settings
(setq comint-scroll-to-bottom-on-input t)
(setq comint-prompt-read-only t)
(setq comint-input-ignoredups t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on) ; interpret ansi color sequences
;;Auto Show Matching Brace
(require 'paren)
(show-paren-mode 1)
;; Don't know why...
;; (require 'printing)
;; (pr-update-menus)
;; Let woman not open new frame
(autoload 'woman "woman" nil t)
(setq woman-use-own-frame nil)
(setq woman-fill-column 75)
;; CUA mode
;; 
;; Have to set this before enabling cua mode, otherwise cua will force
;; set C-<return> to its rectangle function.
(setq cua-rectangle-mark-key (kbd "C-S-<return>"))
(setq cua-enable-cua-keys nil)          ; Disable win keys
(if (< emacs-major-version 22)
    (progn
      (load "cua-21")
      (CUA-mode 'emacs))
  (cua-mode t))
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

;; Write time stamp when saving
(add-hook 'before-save-hook 'time-stamp)

(setq cookie-file (concat ModeDir "/fortune.txt"))

;; Open mutt messages with message-mode
(add-to-list 'auto-mode-alist '("/mutt-.*-[0-9\\-]+" . message-mode))

;; Dired
(eval-after-load "dired" (lambda ()
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
  (shell-command "tmux new-window"))
(define-key dired-mode-map (kbd "C-o") 'open-file-with-default-thing)
(define-key dired-mode-map (kbd "M-o") 'open-tmux-in-current-dir)
))

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

;; ========== CC Mode ===============================================>
;; CC Indention
;; offset customizations not in my-c-style
(setq c-offsets-alist '((member-init-intro . ++)))
;; Customizations for all modes in CC Mode.
(defun my-c-mode-common-hook ()
  (setq-default c-basic-offset 4)
  ;; add my personal style and set it for the current buffer
  (setq c-default-style "bsd"
        c-basic-offset 4)
  ;; we like auto-newline and hungry-delete
  (c-toggle-auto-hungry-state 1)
  ;; key bindings for all supported languages.  We can put these in
  ;; c-mode-base-map because c-mode-map, c++-mode-map, objc-mode-map,
  ;; java-mode-map, idl-mode-map, and pike-mode-map inherit from it.
  (define-key c-mode-base-map "\C-m" 'c-context-line-break)
  )
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
;; Whether the indentation should be controlled by the syntactic context.
(setq c-syntactic-indentation t)
;; Make make command print directory information
(setq compile-command "make -w")

;; ========== Other programming settings ============================>
;; Subword
(add-hook 'c-mode-common-hook
          (lambda () (subword-mode 1)))
(add-hook 'python-mode-hook
          (lambda () (subword-mode 1)))

;; Whitespace mode
(if (not use-pkg-p)
         (progn
           (autoload 'whitespace-mode "whitespace" nil t)
           (autoload 'whitespace-cleanup "whitespace" nil t)))
(defvar my-whitespace-style '(face tab-mark trailing lines-tail space-before-tab))
(setq-default whitespace-style my-whitespace-style)
(setq-default whitespace-active-style my-whitespace-style)
(setq-default whitespace-line-column 80)
(add-hook 'emacs-lisp-mode-hook 'whitespace-mode)
(add-hook 'cc-mode-hook 'whitespace-mode)
(add-hook 'c-mode-hook 'whitespace-mode)
(add-hook 'c++-mode-hook 'whitespace-mode)
(add-hook 'python-mode-hook 'whitespace-mode)
(add-hook 'yaml-mode-hook 'whitespace-mode)

;; Indent guide
(autoload 'indent-guide-mode "indent-guide" nil t)
(eval-after-load "indent-guide"
  (progn
    (setq indent-guide-char "⎸")))
(add-hook 'yaml-mode-hook 'indent-guide-mode)

(add-hook 'python-mode-hook (lambda () (setq python-shell-interpreter "python3")))
;; There’s a bug in python-mode that fills the 1st line of a docstring
;; incorrectly.  The fill width is counted from the """, where for the
;; rest of the lines, the fill is counted from the beginning of the
;; line.  See bugs https://debbugs.gnu.org/cgi/bugreport.cgi?bug=20860
;; and https://debbugs.gnu.org/cgi/bugreport.cgi?bug=21254, and my showcase 
(add-hook 'python-mode-hook (lambda () (setq-local fill-column 80)))
(setq python-fill-docstring-style 'pep-257-nn)

;; Hide show mode (from emacswiki)
(defun toggle-selective-display (column)
  (interactive "P")
  (set-selective-display
   (or column
       (unless selective-display
             (1+ (current-column))))))
(defun toggle-hiding (column)
  (interactive "P")
  (if hs-minor-mode
      (if (condition-case nil
              (hs-toggle-hiding)
            (error t))
          (hs-show-all))
    (toggle-selective-display column)))

(add-hook 'c-mode-common-hook   'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
(add-hook 'java-mode-hook       'hs-minor-mode)
(add-hook 'lisp-mode-hook       'hs-minor-mode)
(add-hook 'perl-mode-hook       'hs-minor-mode)
(add-hook 'sh-mode-hook         'hs-minor-mode)
(add-hook 'python-mode-hook     'hs-minor-mode)

;; Prohibit emacs-lisp-mode-map to use C-x C-a as a prefix.
(setq edebug-inhibit-emacs-lisp-mode-bindings t)

;; Flycheck
(add-hook 'python-mode-hook 'flycheck-mode)
(setq flycheck-mode-line
      '(:eval (format "x%d" (length flycheck-current-errors))))

;; Add XSLT 2.0 and 3.0 schemas
(add-hook
 'nxml-mode-hook
 (lambda ()
   (setq rng-schema-locating-files
         (append (list (car rng-schema-locating-files)) ; Schemas.xml in current dir.
                 (list (expand-file-name
                        "~/.emacs-pkgs/schemas/xslt-relax-ng/xslt.rnc"))
                 (cdr rng-schema-locating-files))))) ; Emacs' schemas.xml

;; =============== Home-made Functions ===============>
(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
	((looking-at "\\s\)") (forward-char 1) (backward-list 1))
	(t (self-insert-command (or arg 1)))))

;; Unfill buffer
(defun unfill-buffer ()
  "Unfill current buffer."
  (interactive "")
  (setq m (point-marker))
  (beginning-of-buffer)
  (while (re-search-forward "\\([^ ]+\\) *
 *\\([^ ]\\)" nil t)
    (replace-match "\\1 \\2"))
  (set-marker m 0 (current-buffer)))

;; Word count
(defun word-count nil "Count words in buffer" (interactive)
  (shell-command-on-region (point-min) (point-max) "wc -w"))

;; Turn off my monitor
;; (defun turn-off-monitor ()
;;   (interactive)
;;   (shell-command "sleep 1; xset dpms force off"))

;; Find chars that not belong to the charset, copied from
;; http://ann77.stu.cdut.edu.cn/EmacsChineseCalendar.html
(defun find-invalid-char ()
  (interactive)
  (let (c m)
    (save-excursion
      (widen)
      (condition-case nil
          (progn
            (setq c (following-char))
            (while c
              (if (and (>= c 128)
                       (<= c 256))
                  (error ""))
              (if ( >= (point) (point-max))
                  (error ""))
              (goto-char (1+ (point)))
              (setq c (following-char))))
        (error (setq m (point)))))
    (goto-char m)))

;; From http://www.zeuux.org/science/learning-emacs.cn.html
(defun copy-line (&optional arg)
  "Save current line into Kill-Ring without mark the line"
  (interactive "P")
  (let ((beg (line-beginning-position))
        (end (line-end-position arg)))
    (copy-region-as-kill beg end)))

(defun copy-line-or-region (&optional n)
  "Save current line or region into Kill-Ring.  If the mark is
deactivated in current buffer, Save current line; otherwise save
the region."
  (interactive "p")
  (if mark-active
      (kill-ring-save
       (region-beginning) (region-end))
    (copy-line n)))

;; Insert current date
(defun insert-date ()
  "Insert the current date according to the variable
  \"insert-date-format\"."
  (interactive "*")
  (insert (format-time-string "%Y-%m-%d")))

(defun toggle-line-wrap ()
  "Toggle `visual-line-mode'."
  (interactive)
  (if auto-fill-function
      ((lambda ()
         (turn-off-auto-fill)
         (visual-line-mode 1)))
    ((lambda ()
       (turn-on-auto-fill)
       (visual-line-mode -1)))))

;; Insert a proper pair of quotes
(defun insert-pair-and-retreat (str)
  "Inserts `str' and go back one character."
  (insert str)
  (backward-char))

(defun insert-single-quotes ()
  "Inserts a proper pair of single quotes."
  (interactive)
  ;; If the last char is "\", inserts a literal char.
  (if (search-backward "\\" (- (point) 1) t)
      (progn (forward-char) (insert "'"))
    ;; We need to detect if the quote is for, for example, "I'm",
    ;; "It's", "Bob's", etc, or for quotation.
    (if (re-search-backward "[A-Za-z]" (- (point) 1) t)
        (progn (forward-char) (insert "’"))
      (insert-pair-and-retreat "‘’"))))

(defun insert-double-quotes ()
  "Inserts a proper pair of double quotes."
  (interactive)
  ;; If the last char is "\", inserts a literal char.
  (if (search-backward "\\" (- (point) 1) t)
      (progn (forward-char) (insert "\""))
    (insert-pair-and-retreat "“”")))

(defun auto-insert-and-convert-dash ()
  "Converts two dashes into an en-dash, or converts a en-dash
followed by a dash to an em-dash."
  (interactive)
  ;; If the last char is "\", inserts a literal char.
  (if (search-backward "\\" (- (point) 1) t)
      (progn (forward-char) (insert "-"))
    (progn
      (insert "-")
      (if (search-backward "--" (- (point) 2) t)
          (replace-match "–"))
      (if (search-backward "–-" (- (point) 2) t)
          (replace-match "—")))))

(defun enable-smart-quote-in-map (mode-map)
  (define-key mode-map "-" 'auto-insert-and-convert-dash)
  (define-key mode-map "'" 'insert-single-quotes)
  (define-key mode-map (kbd "C-'") (lambda () (interactive) (insert ?\")))
  (define-key mode-map "\"" 'insert-double-quotes))

(enable-smart-quote-in-map text-mode-map)
(eval-after-load "scribble-mode"
  (lambda () (enable-smart-quote-in-map scribble-mode-map)))

(eval-after-load "markdown-mode"
  (lambda ()
    (define-key markdown-mode-map "'" 'insert-single-quotes)
    (define-key markdown-mode-map "\"" 'insert-double-quotes)))

(eval-after-load "nxml-mode"
  (lambda ()
    (define-key nxml-mode-map "\"" nil)
    (define-key nxml-mode-map "-" nil)))

(eval-after-load "twittering-mode"
  (lambda ()
    (enable-smart-quote-in-map twittering-edit-mode-map)))

;; Replace stuff like `lambda', `->' with actual unicode chars.
(defun unicode-symbol (name)
  "Translate a symbolic name for a Unicode character -- e.g., LEFT-ARROW
  or GREATER-THAN into an actual Unicode character code. "
  (decode-char 'ucs (case name
                      ;; arrows
                      ('left-arrow 8592)
                      ('up-arrow 8593)
                      ('right-arrow 8594)
                      ('down-arrow 8595)
                      ('Right-arrow #x21d2)
                      ;; Math symbols
                      ('integral #x222b)
                      ('oint #x222e)
                      ('sum #x2211)
                      ('product #x220f)
                      ('infinity #x221e)
                      ('equal #X003d)
                      ('not-equal #X2260)
                      ('approximately #x2248)
                      ('identical #X2261)
                      ('not-identical #X2262)
                      ('less-than #X003c)
                      ('greater-than #X003e)
                      ('less-than-or-equal-to #X2264)
                      ('greater-than-or-equal-to #X2265)
                      ('much-less-than #x226a)
                      ('much-greater-than #x226b)
                      ('logical-and #X2227)
                      ('logical-or #X2228)
                      ('logical-neg #X00AC)
                      ('nil #X2205)
                      ('for-all #X2200)
                      ('there-exists #X2203)
                      ('element-of #X2208)
                      ('cdot #x22c5)
                      ;; boxes
                      ('double-vertical-bar #X2551)
                      ;; relational operators
                      ;; logical operators
                      ;; misc
                      ('horizontal-ellipsis #X2026)
                      ('double-exclamation #X203C)
                      ('prime #X2032)
                      ('double-prime #X2033)
                      ('dagger #x2020)
                      ;; mathematical operators
                      ('square-root #X221A)
                      ('squared #X00B2)
                      ('cubed #X00B3)
                      ;; letters
                      ('lambda #X03BB)
                      ('alpha #X03B1)
                      ('beta #X03B2)
                      ('gamma #X03B3)
                      ('delta #X03B4)
                      ('epsilon #X03B5)
                      ('zeta #X03B6)
                      ('eta #X03B7)
                      ('theta #X03B8)
                      ('iota #X03B9)
                      ('kappa #X03BA)
                      ('mu #X03BC)
                      ('nu #X03BD)
                      ('xi #X03BE)
                      ('pi #X03C0)
                      ('rho #X03C1)
                      ('sigma #X03C3)
                      ('tau #X03C4)
                      ('phi #X03C6)
                      ('chi #X03C7)
                      ('psi #X03C8)
                      ('omega #X03C9))))

(defun substitute-pattern-with-unicode (pattern symbol)
  "Add a font lock hook to replace the matched part of PATTERN with the 
  Unicode symbol SYMBOL looked up with UNICODE-SYMBOL."
  (interactive)
  (font-lock-add-keywords
   nil `((,pattern (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                             ,(unicode-symbol symbol))
                             nil))))))

(defun substitute-patterns-with-unicode (patterns)
  "Call SUBSTITUTE-PATTERN-WITH-UNICODE repeatedly."
  (mapcar #'(lambda (x)
              (substitute-pattern-with-unicode (car x)
                                               (cdr x)))
          patterns))

(defun elisp-unicode ()
  (interactive)
  (substitute-patterns-with-unicode
   (list (cons "(\\(lambda\\>\\)" 'lambda)
         (cons " +\\(nil\\)[ )]" 'nil))))

(add-hook 'emacs-lisp-mode-hook 'elisp-unicode)

(defun cua-or-multicursor ()
  (interactive)
  (if (use-region-p)
      (mc/edit-lines)
    (cua-rectangle-mark-mode)))
;; http://emacs.stackexchange.com/a/9916/514
(eval-after-load "multiple-cursors-core"
  (lambda ()
     (add-to-list 'mc--default-cmds-to-run-once 'cua-or-multicursor)))

(defun sum-cua-rectangle ()
  ;; Treat the content of current cua rectangle as numbers, and
  ;; calculate sum.
  (interactive)
  (message
   (number-to-string
    (reduce (lambda (x y) (+ x y))
            (mapcar (lambda (n) (string-to-number n))
                    (cua--extract-rectangle))))))

(defun comment-sectional ()
  "Start a sectional comment if at empty line, otherwise finish
the sectional comment."
  (interactive)
  (defun chomp (str)
    "Chomp leading and tailing whitespace from STR."
    (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'" str)
      (setq str (replace-match "" t t str))) str)
  (let* ((line (buffer-substring-no-properties
                (line-beginning-position) (line-end-position)))
         (len-line (length line))
         (len-line-bare (length (chomp line))))
    (if (= len-line-bare 0)
        (progn
          (comment-dwim nil)
          (insert "========== "))
      (progn
        (insert " ")
        (insert (make-string (- fill-column len-line 2) ?=))
        (insert ">")))))

;; Web mode
(add-hook 'web-mode-hook
          (lambda ()
            (setq web-mode-markup-indent-offset 2)))
(add-to-list 'auto-mode-alist '("\\.html?$" . web-mode))

;; Maximize and restore window.
(defvar window-confs (make-hash-table))
;; window-confs looks like this:
;;
;; { frame1: {"zoomed": false, "conf": <some conf>},
;;   frame2: {"zoomed": true, "conf": <some other conf>} }

(defun save-window-conf ()
  "Save current window configuration of current frame."
  (let ((this-frame (selected-frame))
        (this-conf (current-window-configuration)))
    (if (gethash this-frame window-confs)
        (puthash 'conf this-conf
                 (gethash this-frame window-confs))
      (let ((new-state (make-hash-table)))
        (puthash 'zoomed nil new-state)
        (puthash 'conf this-conf new-state)
        (puthash this-frame new-state window-confs)))))

(defun load-window-conf ()
  "Load saved window configuration of current frame."
  (let* ((this-frame (selected-frame))
         (saved-state (gethash this-frame window-confs)))
    (if saved-state
        (set-window-configuration (gethash 'conf saved-state)))))

(defun maximize-or-restore-window ()
  "If current window is not maximized, or no window in current
frame has been maximized before, store window configuration, and
maximize current window.  If current window is maximized, and
there's a saved window configuration for current frame, restore
that window configuartion."
  (interactive)
  (let* ((this-frame (selected-frame))
         (saved-state (gethash this-frame window-confs)))
    (if saved-state
        (if (gethash 'zoomed saved-state)
            ;; Already maximized.  Restore.
            (progn
              (load-window-conf)
              (puthash 'zoomed nil saved-state))
          ;; Not maximized.  Save window conf and maximize.
          (progn
            (save-window-conf)
            (delete-other-windows)
            (puthash 'zoomed t saved-state)))

      ;; Never maximized before.  Save window conf and maximize.
      (progn
        (puthash this-frame (make-hash-table) window-confs)
        (maximize-or-restore-window)))))

;; =============== External non-programming modes ===============>
;; ibuffer
(if (or (<= emacs-major-version 21)
        (not (require 'cl-lib nil 'noerror)))
    (load "ibuffer-21")
  (require 'ibuffer))
(global-set-key (kbd "C-x C-b") 'ibuffer)
;; Grouping
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

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))

;; Restore previous session
(require 'session)
(add-hook 'after-init-hook 'session-initialize)

;; add the last component of the path to the filename to distinguish
;; different files with the same name
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Display line number
(defun load-linum ()
  (if (>= emacs-major-version 22)
      (if (not use-pkg-p)
          (load "linum"))
    (progn
      (load "linum-21")
      (easy-mmode-define-global-mode
       global-linum-mode linum-mode linum-mode))))
;; Acutally linum is slow, so don't use it globally.
;; (global-linum-mode t)
(defun toggle-linum ()           ; Toggle line numbering
  (interactive)
  (if linum-mode (linum-mode nil)
    (linum-mode t)))

;; Helm
(if (not use-pkg-p)
    (require 'helm-config))
(helm-mode 1)
(setq helm-buffer-max-length 40)

;; Protect buffers
(defconst protected-buffers
  '("*scratch*" "*Messages*"))
(add-hook
 'after-init-hook
 (lambda ()
   (dolist (buff protected-buffers)
     (with-current-buffer buff (emacs-lock-mode 'kill)))))

;; Ido
(if (>= emacs-major-version 22)
    (require 'ido)
  (load "ido-21"))
(ido-mode t)
(setq ido-enable-flex-matching t) ;; enable fuzzy matching

;; Company
(if use-pkg-p
    (progn
      ;; (autoload 'company-mode "company" nil t)
      (defun complete-with-all-backends ()
        (interactive)
        (if (not (company-complete))
            (company-other-backend)))
      (eval-after-load "company"
        '(progn
           (setq company-dabbrev-ignore-case nil)
           (setq company-dabbrev-downcase nil)
           (setq company-async-timeout 5)
           (define-key company-active-map (kbd "C-n") 'company-select-next-or-abort)
           (define-key company-active-map (kbd "C-p")
             'company-select-previous-or-abort)
           (define-key company-active-map (kbd "M-/") 'company-select-next-or-abort)
           (define-key company-active-map (kbd "RET") 'company-complete-selection)
           (define-key company-active-map (kbd "TAB") 'company-complete-common)
           (setq company-backends
                 '(company-elisp
                   company-jedi
                   (company-clang company-xcode company-gtags
                    company-etags company-semanticcompany-eclim)
                    company-nxml company-css
                   (company-files company-keywords)
                   (company-dabbrev-code company-dabbrev)))
           (company-quickhelp-mode 1)))
      ;; (require 'company)
      (add-hook 'after-init-hook 'global-company-mode)
      ))

(setq jedi:server-args
      (list "--sys-path" (expand-file-name "~/.Python")
            "--virtual-env" (expand-file-name "~/Python3")
            "--virtual-env" (expand-file-name "~/Python")))

;; Rainbow delimiters
(add-hook 'cc-mode-hook 'rainbow-delimiters-mode)
(add-hook 'scheme-mode-hook 'rainbow-delimiters-mode)
(add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'LaTeX-mode-hook 'rainbow-delimiters-mode)
(add-hook 'ConTeXt-mode-hook 'rainbow-delimiters-mode)

;; Yet another snippet extension for Emacs.
(add-hook
 'after-init-hook
 (lambda ()
   (require 'yasnippet) ;; not yasnippet-bundle
   (add-to-list 'yas-snippet-dirs (concat ModeDir "/snippets"))
   ;; Don't need bundled snippets.
   (delq 'yas-installed-snippets-dir yas-snippet-dirs)
   (yas-global-mode 1)
   ))

(add-hook 'git-commit-mode-hook
          #'(lambda ()
              (yas-activate-extra-mode 'git-commit-mode)))

;; Beancount
(add-to-list 'auto-mode-alist '("\\.beancount\\'" . beancount-mode))
(autoload 'beancount-mode "beancount.el" "Load Beancount mode" t)
(add-hook 'beancount-mode-hook
          #'(lambda () (yas-activate-extra-mode 'beancount-mode)))

;; Notmuch and mail
(setq message-directory "~/mail")
(setq mail-host-address my-mail-host)
(autoload 'notmuch "notmuch.el" "Load notmuch mail searcher" t)
(add-hook 'notmuch-tree-mode-hook
          (lambda () (interactive)
            (define-key notmuch-tree-mode-map "g" 'notmuch-show-reply)
            (define-key notmuch-tree-mode-map "r" 'notmuch-show-reply-sender)))
(add-hook 'notmuch-show-mode-hook
          (lambda () (interactive)
            (define-key notmuch-show-mode-map "g" 'notmuch-show-reply)
            (define-key notmuch-show-mode-map "r" 'notmuch-show-reply-sender)))

(setq mail-specify-envelope-from t)
(setq message-sendmail-envelope-from 'header)
(setq mail-envelope-from 'header)
(setq send-mail-function 'sendmail-send-it)
(setq sendmail-program "msmtp")
(setq mail-default-headers
      (concat "X-Operating-System: "
              ;; This ends with a ‘\n’.
              (shell-command-to-string "uname -s -r -m")
              "X-Useless-Header: Use the Force, Luke!"))

;; If send from work, manually FCC sent_items, because of weird reasons.
(setq notmuch-fcc-dirs my-fcc-dirs)
(defun assoc-regexp (key dict)
  (if (null dict)
      nil
    (if (string-match (car (car dict)) key)
        (car dict)
      (assoc-regexp key (cdr dict)))))

(defun my-fcc-header-setup ()
  (let ((subdir (cdr (assoc-regexp (message-fetch-field "from") notmuch-fcc-dirs))))
    (if subdir
        (message-add-header (concat "Fcc: " subdir)))))
(add-hook 'message-send-hook 'my-fcc-header-setup)
(setq message-fcc-handler-function
      '(lambda (destdir)
         (notmuch-maildir-fcc-write-buffer-to-maildir destdir t)))

;; Signature
(defun get-random-element (list)
  "Returns a random element of LIST."
  (if (and list (listp list))
      (nth (random (1- (1+ (length list)))) list)
    (error "Argument to get-random-element not a list or the list is empty")))

(defun get-string-from-file (filePath)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(defun choose-sig-by-type (type)
  (cond ((eq type 'file)
         (get-string-from-file my-signature-file))
        ((eq type 'dir)
         (get-string-from-file
          (get-random-element
           (directory-files-recursively my-signature-dir ".*"))))
        ((eq type 'fortune) (fortune))
        ((eq type 'cookie) (cookie nil))))

(defun choose-sig-by-condition (condition)
  (if (null condition)
      nil
    (let ((this-cond (car condition)))
      (if (string-match
           (cdr (assoc 'pattern this-cond))
           (message-fetch-field (cdr (assoc 'header this-cond))))
          (choose-sig-by-type (cdr (assoc 'sig-type this-cond)))
        (choose-sig-by-condition (cdr condition))))))

(defun choose-sig ()
  (interactive)
  (choose-sig-by-condition my-sig-condition))

(defun choose-from-by-to ()
  (interactive)
  (let ((to-field (message-fetch-field "to")))
    (if to-field
        (let ((correct-from
               (cdr (assoc-regexp to-field my-from-by-to))))
          (if correct-from
              (message-replace-header "From" correct-from))))))

(add-hook 'message-signature-setup-hook
          (lambda () (interactive)
            (choose-from-by-to)
            (setq message-signature 'choose-sig)
            (message-insert-signature)
            (setq message-signature nil)))

;; (add-hook 'message-setup-hook 'choose-from-by-to)

;; Projectile
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)

;; For log viewing conveniences.
(define-generic-mode 'python-log-mode
  '()                                   ; comments
  '("INFO" "WARN" "CRIT" "ERRO")          ; Keywords
  '(("[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\},[0-9]+" . 'font-lock-constant-face)
    ("ERROR?" . 'error)
    ("FATAL" . 'error)
    ("[nN]ot expected" . 'error)
    ("[uU]nexpected" . 'error))
  '()
  nil
  "A mode for python log")

;; Twittering mode
(eval-after-load "twittering-mode"
  (lambda ()
    (setq twittering-status-format
          "%i%RT{♺}%QT{❝} @%s %FACE[font-lock-type-face]{%S} %FACE[font-lock-comment-face]{%@}\n%FOLD[    ]{%T\n%QT{+----\n%FOLD[|]{%i @%s %FACE[font-lock-type-face]{%S} %FACE[font-lock-comment-face]{%@}\n%FOLD[  ]{%T}}\n+----\n}%RT{(Retweeted from @%s)\n}")))

;; Lilypond
(load "lilypond-init.el" t)

;; Which-key
(add-hook 'after-init-hook 'which-key-mode)

;;=============== global bindings ====================>
(if linux-x-p
    (global-set-key (kbd "<C-XF86KbdBrightnessUp>") 'list-bookmarks)
  (global-set-key (kbd "C-<f10>") 'list-bookmarks))
(global-set-key (kbd "s-p") 'previous-buffer)
(global-set-key (kbd "s-n") 'next-buffer)
(global-set-key (kbd "<s-up>") 'windmove-up)
(global-set-key (kbd "<s-down>") 'windmove-down)
(global-set-key (kbd "<s-left>") 'windmove-left)
(global-set-key (kbd "<s-right>") 'windmove-right)
(global-set-key (kbd "s-k") 'windmove-up)
(global-set-key (kbd "s-j") 'windmove-down)
(global-set-key (kbd "s-h") 'windmove-left)
(global-set-key (kbd "s-l") 'windmove-right)
(global-set-key (kbd "s-K") 'buf-move-up)
(global-set-key (kbd "s-J") 'buf-move-down)
(global-set-key (kbd "s-H") 'buf-move-left)
(global-set-key (kbd "s-L") 'buf-move-right)
(global-set-key (kbd "M-SPC") 'set-mark-command)
(global-set-key (kbd "C-x C-k") 'kill-this-buffer)
(global-set-key (kbd "M-w") 'copy-line-or-region)
(global-set-key (kbd "s-v") 'clipboard-yank)
(global-set-key (kbd "C-x M-f") 'find-file-at-point)
(global-set-key (kbd "C-x M-r") 'revert-buffer)
(global-set-key (kbd "C-x =") 'balance-windows)
(global-set-key (kbd "C-x +") 'what-cursor-position)
(if linuxp (global-set-key (kbd "M-n") 'toggle-linum))
(global-set-key (kbd "C-<f12>") 'org-remember)
(global-set-key (kbd "C-=") 'toggle-hiding)
(global-set-key (kbd "C-+") 'toggle-selective-display)
(global-set-key (kbd "M-RET") 'toggle-line-wrap)
(if use-pkg-p
    (progn
      (global-set-key (kbd "C-<return>") 'cua-or-multicursor)
      (global-set-key (kbd "C-@") 'cua-or-multicursor)))

(global-set-key (kbd "C-;") 'comment-sectional)
;; Prevent flyspell to overide this key.
(add-hook 'flyspell-mode-hook
          (lambda ()
            (define-key flyspell-mode-map (kbd "C-;") nil)))

(global-set-key "%" 'match-paren)
(global-set-key (kbd "M-C") 'compile)
(global-set-key (kbd "s-,") (lambda () (interactive) (find-file "~/.emacs.el")))
(if (>= emacs-major-version 24)
    (global-set-key (kbd "M-/") 'complete-with-all-backends))
;; (if (>= emacs-major-version 24)
;;     (global-set-key (kbd "M-/") 'company-complete))

;; Helm
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-.") 'helm-gtags-dwim) ; Default: find-tag
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x C-a") 'helm-mini)
(global-set-key (kbd "M-s o") 'helm-occur)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "C-x r b") 'helm-filtered-bookmarks)

;; Change font size
(global-set-key (kbd "s-=") 'text-scale-adjust)
(global-set-key (kbd "s--") 'text-scale-adjust)
(global-set-key (kbd "s-0") 'text-scale-adjust)

;; Maximize and restore windows
(global-set-key (kbd "C-`") 'maximize-or-restore-window)

;; ========== Look & feel ===========================================>
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
    (achievements-mode . "A")
    (projectile-mode . "p")
    (which-key-mode . "")
    ;; Major modes
    (lisp-interaction-mode . "λ")
    (python-mode . "π")
    (emacs-lisp-mode . "Λ")
    (c++-mode . "C++")
    (c-mode . "C")
    (help-mode . "?")
    (dired-mode . "ls")
    (emacs-lock-mode . ""))
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

(add-hook 'after-change-major-mode-hook 'clean-mode-line)

;; Smart modeline
(setq sml/theme 'light)
(setq sml/show-file-name nil)
(setq sml/no-confirm-load-theme t)
;; Hide dir name.
(setq sml/replacer-regexp-list '((".*" "")))
(sml/setup)

;; Add a small image to the mode line for current window~~
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
                            ,mode-line-img)))))))

;; Size
(if window-system
    (progn
      (add-to-list 'default-frame-alist (cons 'height my-frame-height))
      (add-to-list 'default-frame-alist (cons 'width my-frame-width))))

;; Font
(if window-system
    (progn
      (set-face-attribute
       'default nil
       :family my-font
       :height my-font-size
       :weight 'normal)
      ;; Font fallback
      ;; (set-fontset-font
      ;;  (frame-parameter nil 'font)
      ;;  (cons my-font-fallback-boundary #xe007f)
      ;;  (font-spec :family my-unicode-font))
      ))

;; (add-to-list 'face-font-rescale-alist '("Noto Sans Mono CJK SC" . 1.1))

;; Startup screen
(if (boundp 'my-splash-image)
    (if my-splash-image
        (setq fancy-splash-image my-splash-image)))

;; preserve the size of the frames when changeing fonts and stuff.
(if (and window-system (> emacs-major-version 24))
    (setq frame-inhibit-implied-resize t))

;; ========== Other config files ====================================>
(autoload 'erc-start "emacs-erc.el" "Load my ERC configuration" t)
(load "emacs-tex.el" 'noerror)
(load "emacs-org.el" 'noerror)

;; ========== Finish up =============================================>
;; Go full screen and split
(if my-full-screen
    (progn
      (toggle-frame-fullscreen)
      (split-window-horizontally)
      (split-window-horizontally)
      (select-window (previous-window))
      (split-window-vertically)
      (balance-windows)))

(load custom-file 'noerror)
(load-theme 'FlatUI t)
(server-start)
