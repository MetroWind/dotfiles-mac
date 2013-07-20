;; -*- coding: utf-8; -*-
;; What are we using?? (from http://www.xsteve.at/prg/emacs/.emacs.txt)
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

;; =============== add load path ====================>
(let ((ModeDir "~/.emacs.d"))
  (add-to-list 'load-path (expand-file-name ModeDir))
  (let ((Modes
         '("imaxima" "slime-2.0" "slime48" "emacs-jabber"
           "gnuplot-mode.0.6.0" "haskell-mode" "mldonkey"
           "yasnippet" "pov-mode" "emacsim" "emeteo" "auctex"
           "anything" "remember" "python-mode"
           "twittering-mode" "auto-complete" "magit"
           "company-mode" "multiple-cursors" "powerline")))
    (dolist (Mode Modes)
      (add-to-list 'load-path (expand-file-name
                               (concat ModeDir "/" Mode))))
    )
)

(add-to-list 'custom-theme-load-path
             (expand-file-name "~/.emacs.d/themes/"))

(if (or linuxp macp)
    (add-to-list 'load-path (expand-file-name "~/.emacs.d/w3m")))

(load-file (expand-file-name "~/.emacs-passwd.el"))

;; Info path
(if macp
    (setq Info-default-directory-list
          (append '("~/.emacs.d/info")
                  Info-default-directory-list)))

;; =============== Coding and Language ===============>
;; (set-selection-coding-system 'utf-8)
;; (set-clipboard-coding-system 'utf-8)
;; (set-keyboard-coding-system 'utf-8) ; if utf8, unable to use input method.
;; (set-language-environment 'utf-8)
;; (setq locale-coding-system 'utf-8)
;; (setq current-language-environment "utf-8")
;; (setq locale-language-names
;;       (cons '("zh_CN.UTF-8" "UTF-8" utf-8) locale-language-names))
;; Fix the width of Chinese marks.
(if linuxp
    (let ((l '(chinese-gb2312
               gb18030-2-byte
               gb18030-4-byte-bmp
               gb18030-4-byte-ext-1
               gb18030-4-byte-ext-2
               gb18030-4-byte-smp)))
      (dolist (elt l)
        (map-charset-chars #'modify-category-entry elt ?|)
        (map-charset-chars
         (lambda (range ignore)
           (set-char-table-range char-width-table range 2))
         elt))))

;;=========== Some basic settings ==============================>
(setq custom-file "~/.emacs-custom.el") ; Go away, custom!!!  Go away!!
(setenv "COLUMNS" "80")                 ; Kill the dumb COLUMN warning from `ls'
(ignore-errors (setq user-full-name my-user-name))
(ignore-errors (setq user-mail-address my-email))
(setq mail-user-agent 'message-user-agent)
(setq default-major-mode 'text-mode)
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq column-number-mode t)
(setq kill-ring-max 255)
(setq next-screen-context-lines 5)
(setq display-time-day-and-date t)
(setq sentence-end "\\([。！？；]\\|……\\|[.?!;][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")
(setq sentence-end-double-space t)
(ignore-errors (setq frame-title-format my-title))
;; Don't bring partially a visible line to fully visible before scrolling.
(setq auto-window-vscroll nil)
;; (setq scroll-margin 4
;;       scroll-conservatively 10000) ;;continuous scrolling
(setq adaptive-fill-regexp "[ \t]+\\|[ \t]*\\([0-9]+\\.\\|\\*+\\)[ \t]*")
(setq tab-stop-list (number-sequence 4 200 4))
;; let the delete key delete foreward
(if linux-x-p
    (normal-erase-is-backspace-mode 1))
;; X primary selection has priority.  For emacs 24.1
(if linux-x-p
    (setq x-select-enable-primary t))
;; Display time information in modeline.
;; (setq display-time-format "%Y-%m-%d %A %I:%M")
(setq display-time-day-and-date nil)
;; (display-time)
;; Use an icon to represent new mail.
(setq display-time-use-mail-icon t)
(setq display-time-mail-file "~/Mail/mbox")

(add-hook 'text-mode-hook
          (lambda () (auto-fill-mode t)))

;; Add a newline in the end if none.
(setq require-final-newline t)

;; time stamp
(add-hook 'write-file-hooks 'time-stamp)
(setq time-stamp-active t)
(setq time-stamp-format "%:u %04y-%02m-%02d %02H:%02M:%02S")
(setq time-stamp-end: "\n")
;; 所有的 yes or no 全都用 y or n 代替 :-)
(fset 'yes-or-no-p 'y-or-n-p)
;; Grammar hightlighting
(global-font-lock-mode 1)
;; Turn on auto save function
(setq auto-save-default t)
;; Enable Narrow
(put 'narrow-to-region 'disabled nil)
;; 鼠标要挡住正在打的字时自动移开
(mouse-avoidance-mode 'animate)
;; Automatically identify image files.
(auto-image-file-mode)
;; Replace selected text when typing
(delete-selection-mode t)
;; Default input method for `C-\'
(setq default-input-method "TeX")
;; Remove *bar...
(tool-bar-mode 0)
(if (not macp)
    (menu-bar-mode 0))
(scroll-bar-mode -1)
;; No sound alarm!
(setq visible-bell t)
;; printer settings
(setq ps-printer-name "pdf_creator")
(setq ps-font-size '(7 . 8))
(setq ps-multibyte-buffer 'bdf-font-except-latin)
(setq ps-paper-type 'a4)
(setq ps-number-of-columns 1)
(setq ps-print-use-faces t)  ; always print using faces
(setq ps-print-color-p nil)  ; don't use colors for printing

;; Let tramp recognize passcode prompt on Kraken
(setq tramp-password-prompt-regexp
      "^.*\\([pP]assword\\|[pP]assphrase\\|PASSCODE\\).*:\0? *")
;; Non-nil if searches and matches should ignore case.
(setq case-fold-search t)
;; debug when error
(setq debug-on-error t)
;; Fringe
(setq fringe-mode 'left-only)
(setq-default right-fringe-width 0)
(setq default-indicate-buffer-boundaries '((top . left) (t . left)))
;; Dired
(setq dired-recursive-copies 'top)
(setq dired-recursive-deletes 'top)
(setq dired-dwim-target t)
;; Use external `ls' instead of `ls-lisp' if (well) supported.
(if linuxp
    (setq ls-lisp-use-insert-directory-program t)
  (progn (setq ls-lisp-use-insert-directory-program nil)
         (require 'ls-lisp)))
;; In Transient Mark mode, when the mark is active, the region is
;; highlighted.
(setq transient-mark-mode t)
;; Backup Control
(setq kept-old-versions 2)
(setq kept-new-versions 3)
(setq delete-old-versions nil)
(setq backup-directory-alist '(("." . "~/.backup/")))
(setq backup-by-copying t)
;; Spell Checking
(setq-default ispell-program-name
              (cond (linuxp "aspell")
                    (macp "/usr/local/bin/aspell")))
(add-hook 'text-mode-hook 'flyspell-mode)
;; Delete duplicates in minibuffer history
(setq history-delete-duplicates t)

;; Version Control Settings
(require 'vc-git)
(when (featurep 'vc-git) (add-to-list 'vc-handled-backends 'git))
(require 'magit)
;; Shell Mode settings
(setq comint-scroll-to-bottom-on-input t)
(setq comint-prompt-read-only t)
(setq comint-input-ignoredups t)
;; interpret ansi color sequences
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
;; E-Shell
(setq eshell-prompt-function
      (lambda nil
        (concat
         "---> " (eshell/pwd) "\n"
         (eshell/whoami)
         (if (= (user-uid) 0)
             "# " "$ "))))
(setq eshell-prompt-regexp "^[^#$\n]*[#$] ")
;; GDB
(setq gdb-show-main t)
(setq gdb-many-windows t)
;; Load gnus functions
(require 'gnus)
;;Auto Show Matching Brace
(require 'paren)
(show-paren-mode 1)
;; Don't know why...
(require 'printing)
(pr-update-menus)
;; Let woman not open new frame
(require 'woman)
(setq woman-use-own-frame nil)
(setq woman-fill-column 75)
;; CUA mode
(cua-mode -1)
(setq cua-enable-cua-keys nil)          ; Disable win keys
;; Yow file
(setq yow-file (expand-file-name "~/.emacs.d/yow.lines"))
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
;; (setq exec-path (cons "/usr/local/bin" exec-path))
(defun set-exec-path-from-shell-PATH ()
  "Set up `exec-path' and PATH environment variable to match that
used by the user's shell.  This is particularly useful in Mac,
where GUI apps are not started from a shell."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$"
                          ""
                          (shell-command-to-string
                           "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))
(if macp (set-exec-path-from-shell-PATH))

;; =============== C Mode ===============>
(add-hook 'c-mode-common-hook
		  (lambda () (c-toggle-auto-hungry-state 1)))
;; CC Indention
(defconst my-c-style
  '((c-tab-always-indent        . t)
    (c-comment-only-line-offset . 0)
    (c-hanging-braces-alist     . ((substatement-open after)
                                   (brace-list-open)))
    (c-hanging-colons-alist     . ((member-init-intro before)
                                   (inher-intro)
                                   (case-label after)
                                   (label after)
                                   (access-label after)))
    (c-cleanup-list             . (scope-operator
                                   empty-defun-braces
                                   defun-close-semi))
    (c-offsets-alist            . ((arglist-close . c-lineup-arglist)
                                   (substatement-open . 0)
                                   (case-label        . 4)
                                   (block-open        . 0)
                                   (knr-argdecl-intro . -)))
    (c-echo-syntactic-information-p . t)
    )
  "My C Programming Style")
;; offset customizations not in my-c-style
(setq c-offsets-alist '((member-init-intro . ++)))
;; Customizations for all modes in CC Mode.
(defun my-c-mode-common-hook ()
  ;; add my personal style and set it for the current buffer
  (c-add-style "PERSONAL" my-c-style t)
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

;; =============== Other programming settings =========>
;; Subword
(add-hook 'c-mode-common-hook
          (lambda () (subword-mode 1)))
(add-hook 'python-mode-hook
          (lambda () (subword-mode 1)))

;; PSF Python mode
(setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
(setq interpreter-mode-alist (cons '("python" . python-mode)
                                   interpreter-mode-alist))
(autoload 'python-mode "python-mode" "Python editing mode." t)

;; Python + Flymake
(when (load "flymake" t)
  (defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "pyflakes" (list local-file))))

  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pyflakes-init)))

;; (add-hook 'python-mode-hook 'flymake-mode)

;; Python + ElDoc
(add-hook 'python-mode-hook
          (lambda () (eldoc-mode 1)) t)
(defun my-python-documentation (w)
  "Launch PyDOC on the Word at Point"
  (interactive
   (list (let* ((word (thing-at-point 'word))
                (input (read-string
                        (format "pydoc entry%s: "
                                (if (not word) ""
                                  (format " (default %s)" word))))))
           (if (string= input "")
               (if (not word) (error "No pydoc args given")
                 word) ;sinon word
             input)))) ;sinon input
  (shell-command (concat py-python-command
                         " -c \"from pydoc import help;help(\'" w "\')\"")
                 "*PYDOCS*")
  (view-buffer-other-window "*PYDOCS*" t 'kill-buffer-and-window))

;; iPython
(if linuxp
    ((lambda ()
      (add-to-list 'interpreter-mode-alist '("python" . python-mode))
      (require 'ipython)
      (setq py-python-command-args '( "-colors" "Linux"))
      (setq ipython-command "/usr/bin/ipython"))))

;; Whitespace mode
(autoload 'whitespace-mode "whitespace" "Load whitespace" t)
(eval-after-load "whitespace"
  '(progn
    (setq-default whitespace-style
                  '(face tabs trailing lines space-before-tab))
    (setq-default whitespace-active-style
                  '(face tabs trailing lines space-before-tab))))
(add-hook 'emacs-lisp-mode-hook 'whitespace-mode)
(add-hook 'cc-mode-hook 'whitespace-mode)
(add-hook 'c-mode-hook 'whitespace-mode)
(add-hook 'c++-mode-hook 'whitespace-mode)
(add-hook 'python-mode-hook 'whitespace-mode)

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

;; CSS Mode
(autoload 'css-mode "css-mode")
(setq auto-mode-alist
     (cons '("\\.css\\'" . css-mode) auto-mode-alist))

;; Gnuplot Mode
(autoload 'gnuplot-mode "gnuplot" "gnuplot major mode" t)
(autoload 'gnuplot-make-buffer "gnuplot" "open a buffer in gnuplot mode" t)
(setq auto-mode-alist (append '(("\\.gnu$" . gnuplot-mode)) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.gp$" . gnuplot-mode)) auto-mode-alist))
;; (global-set-key [(f9)] 'gnuplot-make-buffer)

;; Haskell Mode
(load (expand-file-name "~/.emacs.d/haskell-mode/haskell-site-file"))
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
(setq auto-mode-alist (append '(("\\.hs$" . haskell-mode)) auto-mode-alist))

;; Graphviz DOT mode
(load-file (expand-file-name "~/.emacs.d/graphviz-dot-mode.el"))
(add-hook 'graphviz-dot-mode-hook
          (lambda ()
             (define-key graphviz-dot-mode-map "\C-c\C-c" 'compile)))

;; Wikipedia mode
(add-hook 'outline-mode-hook
          (lambda ()
            (require 'outline-magic)))
(add-hook 'outline-minor-mode-hook
          (lambda ()
            (require 'outline-magic)
            (define-key outline-minor-mode-map [(f10)] 'outline-magic)))
(autoload 'longlines-mode "longlines.el"
  "Minor mode for editing long lines." t)
(autoload 'wikipedia-mode
  "wikipedia-mode.el"
  "Major mode for editing documents in Wikipedia markup." t)
(add-hook 'wikipedia-mode-hook
          (lambda ()
             (setq auto-fill-function nil)))

;; Moinmoin mode
(autoload 'moinmoin-mode "moinmoin-mode.el"
  "Major mode for editing document in Moinmoin markup" t)

;; PKGBUILD Mode
(if linuxp
    ((lambda ()
      (autoload 'pkgbuild-mode "pkgbuild-mode.el" "PKGBUILD mode." t)
      (setq auto-mode-alist (append '(("/PKGBUILD$" . pkgbuild-mode))
                                    auto-mode-alist))
)))

;; Second Life Mode
(autoload 'lsl-mode "lsl-mode" "Load LSL mode." t)
(add-to-list 'auto-mode-alist '("\\.lsl$" . lsl-mode))

;; Javascript mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . javascript-mode))
(add-to-list 'auto-mode-alist '("\\.pac\\'" . javascript-mode))
(autoload 'javascript-mode "javascript" nil t)

;; POV-Ray mode
;; (require 'pov-mode)
(autoload 'pov-mode "pov-mode.el" "PoVray scene file mode" t)
(add-to-list 'auto-mode-alist '("\\.pov$" . pov-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . pov-mode))
(setq pov-command-alist
      '(("Render" "povray" "+A0.2 +i%s")
        ("Test quality render" "povray" "res120 -Q3 +i%s" nil)
        ("Low quality render" "povray" "res320 +i%s" nil)
        ("Medium quality render" "povray" "res1k +i%s" nil)
        ("High quality render" "povray" "res800 +i%s" nil)
        ("External view" "feh" "%s" nil)
        ("Internal view"
         ("Internal view")
         nil)))
(setq pov-default-view-internal nil)

;; Mode for asciidoc
(add-to-list 'auto-mode-alist '("\\.doc$" . doc-mode))
(autoload 'doc-mode "doc-mode")

;; Markdown
(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md" . markdown-mode))

;; Lua
(setq auto-mode-alist (cons '("\\.lua$" . lua-mode) auto-mode-alist))
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)

;; Rainbow mode
(require 'rainbow-mode)
(add-hook 'css-mode-hook 'rainbow-mode)

;; Muttrc mode
(autoload 'muttrc-mode "muttrc-mode.el"
  "Major mode to edit muttrc files" t)
(setq auto-mode-alist
      (append '(("muttrc\\'" . muttrc-mode))
              auto-mode-alist))

;; PHP mode
(setq auto-mode-alist (cons '("\\.php$" . php-mode) auto-mode-alist))
(autoload 'php-mode "php-mode" "PHP editing mode." t)

;; Scheme program
(autoload 'gambit-inferior-mode "gambit" "Hook Gambit mode into cmuscheme.")
(autoload 'gambit-mode "gambit" "Hook Gambit mode into scheme.")
(add-hook 'inferior-scheme-mode-hook (function gambit-inferior-mode))
(add-hook 'scheme-mode-hook (function gambit-mode))
(setq scheme-program-name "gsi -:d-")

;; Rainbow delimiters
(require 'rainbow-delimiters)
(global-rainbow-delimiters-mode)

;; YAML mode
(autoload 'yaml-mode "yaml-mode" "YAML major mode.")
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))
(add-hook 'yaml-mode-hook (lambda () (setq tab-width 2)))

;; Octave mode
(autoload 'octave-mode "octave-mod" nil t)
(add-hook 'octave-mode-hook
          (lambda ()
            (auto-fill-mode 1)
            (if (eq window-system 'x)
                (font-lock-mode 1))))

;; Mathematica mode
;; From <http://chasen.org/~daiti-m/dist/math++.el>
(autoload 'math-mode "math++" nil t)
(add-to-list 'auto-mode-alist '("\\.math$" . math-mode))

;; Web mode
(autoload 'web-mode "web-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.html?$" . web-mode))

;; =============== Home-made Functions ===============>
(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))
;; 中英文之间增加空格
(defun insert-space-between-eng-cn ()
  "Insert a space between English words and Chinese charactors"
  (interactive "")
  (beginning-of-buffer)
  (while (re-search-forward "\\(\\cc\\)\\([a-zA-Z0-9]\\)" nil t)
    (replace-match "\\1 \\2" nil nil))
  (beginning-of-buffer)
  (while (re-search-forward "\\([a-zA-Z0-9]\\)\\(\\cc\\)" nil t)
    (replace-match "\\1 \\2" nil nil))
;; 去掉全角标点与英文之间的空格
  (beginning-of-buffer)
  (while (re-search-forward "\\([。，！？；：“”（）、]\\) \\([a-zA-Z0-9]\\)" nil t)
    (replace-match "\\1\\2" nil nil))
  (beginning-of-buffer)
  (while (re-search-forward "\\([a-zA-Z0-9]\\) \\([。，！？；：“”（）、]\\)" nil t)
    (replace-match "\\1\\2" nil nil)))
(defun insert-wave-between-eng-cn () ; used when editing TeX files
  "Insert a wave mark between English words and Chinese charactors"
  (interactive "")
  (beginning-of-buffer)
  (while (re-search-forward "\\(\\cc\\)\\([a-z]\\)" nil t)
    (replace-match "\\1~\\2" nil nil))
  (beginning-of-buffer)
  (while (re-search-forward "\\([a-z]\\)\\(\\cc\\)" nil t)
    (replace-match "\\1~\\2" nil nil)))

(global-set-key (kbd "C-c C-r") 'insert-space-between-eng-cn)

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

(define-key text-mode-map "-" 'auto-insert-and-convert-dash)
(define-key text-mode-map "'" 'insert-single-quotes)
(define-key text-mode-map (kbd "C-'") (lambda () (interactive) (insert ?\")))
(define-key text-mode-map "\"" 'insert-double-quotes)

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

(require 'git-gutter-fringe)
(global-git-gutter-mode t)

;; =============== External non-programming modes ===============>
;; htmlize
(require 'htmlize)
(setq htmlize-output-type 'css)
;; (setq htmlize-before-hook
;;       'color-theme-emacs-21)
;; (setq htmlize-after-hook
;;       (lambda ()
;;         (color-theme-deep-blue)
;;         (set-face-attribute
;;          'mode-line nil
;;          :background "#dedede" :foreground "#000000"
;;          :height 80
;;          :box nil)))

;; ibuffer
(require 'ibuffer)
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
;; Save faces as ansi sequences
(require 'ansit)

;; NB tooltips
(require 'pos-tip)

;; hippie-expand (M-/)
;; (setq hippie-expand-try-functions-list
;;       '(try-expand-dabbrev
;;  try-expand-dabbrev-visible
;;  try-expand-dabbrev-all-buffers
;;  try-expand-dabbrev-from-kill
;;  try-complete-file-name-partially
;;  try-complete-file-name
;;  try-expand-all-abbrevs
;;  try-expand-list
;;  try-expand-line
;;  try-complete-lisp-symbol-partially
;;  try-complete-lisp-symbol))

;; Company-mode completion
(setq dabbrev-case-replace t)
(require 'company)
;; (defun append-to-each (xs e)
;;   ;; `e' should be a symbol
;;   (if xs
;;       (let ((first-e (car xs)))
;;         (cons
;;          (if (symbolp first-e) (list first-e e) (append first-e (list e)))
;;          (append-to-each (cdr xs) e)))
;;     '()))

(defun complete-with-all-backends ()
  (interactive)
  (if (not (company-complete))
      (company-other-backend)))

(eval-after-load "company"
  '(progn
     (define-key company-active-map (kbd "C-n") 'company-select-next-or-abort)
     (define-key company-active-map (kbd "C-p")
       'company-select-previous-or-abort)
     (define-key company-active-map (kbd "M-/") 'company-select-next-or-abort)
     (define-key company-active-map (kbd "RET") 'company-complete-selection)
     (define-key company-active-map (kbd "TAB") 'company-complete-common)
     (setq company-backends
           '(company-elisp  company-dabbrev  company-clang company-xcode
             company-files
             (company-gtags company-etags company-dabbrev-code company-keywords)
             company-nxml company-css company-oddmuse company-semantic
             company-eclim company-ropemacs))))
(add-hook 'after-init-hook 'global-company-mode)

;; Auto-complete
;; (require 'auto-complete-config)
;; (add-to-list 'ac-dictionary-directories
;;              (concat (getenv "HOME")
;;                      "/.emacs.d/auto-complete/ac-dict"))
;; (ac-config-default)
;; (add-to-list 'ac-modes 'ConTeXt-mode)
;; (ac-set-trigger-key "M-/")
;; (setq ac-auto-show-menu nil)
;; (setq ac-sources '(ac-source-words-in-same-mode-buffers
;;                    ac-source-symbols
;;                    ac-source-filename
;;                    ac-source-functions
;;                    ac-source-yasnippet
;;                    ac-source-variables
;;                    ac-source-symbols
;;                    ac-source-features
;;                    ac-source-abbrev
;;                    ac-source-dictionary))

;; (setq ac-use-menu-map t)
;; ;; Default settings
;; (define-key ac-menu-map "\C-n" 'ac-next)
;; (define-key ac-menu-map (kbd "M-/") 'ac-next)
;; (define-key ac-menu-map "\C-p" 'ac-previous)
;; ;; Disable enter completion
;; (define-key ac-completing-map (kbd "RET") nil)

;; MlDonkey
(require 'mldonkey)

;; Ascii table
(require 'ascii-table)

;; Restore previous session
(require 'session)
(add-hook 'after-init-hook 'session-initialize)

;; add the last component of the path to the filename to distinguish
;; different files with the same name
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Google interface
;; (load-library "g")
;; (setq g-user-email user-mail-address)

(if linuxp
    ((lambda ()
      (autoload 'maxima-mode "maxima" "Maxima editing mode" t)
      (autoload 'maxima "maxima" "Running Maxima interactively" t)
      (autoload 'imaxima "imaxima" "Frontend for maxima with Image support" t)
      (autoload 'imath-mode "imath" "Imath mode for math formula input" t)
)))

;; Yet another snippet extension for Emacs.
(require 'yasnippet) ;; not yasnippet-bundle
(yas/global-mode 1)
;; (require 'yasnippet)
;; (yas/initialize)
;; (yas/load-directory (expand-file-name "~/.emacs.d/yasnippet/snippets"))

(require 'two-mode-mode)
(require 'asy-mode)
(require 'quake-mode)

;; Key binding quiz
(require 'keywiz)

;; Pastie.org
(require 'pastie)

;; Typing practice
(load-library "typing-practice")

;; `shell-command' with completion
(require 'shell-command)
(shell-command-completion-mode)

;; Display line number
(require 'linum)
(global-linum-mode t)
(defun toggle-linum ()           ; Toggle line numbering
  (interactive)
  (if linum-mode (linum-mode -1)
    (linum-mode t)))

;; Predictive abbreviation
;; (require 'pabbrev)
;; (add-hook 'text-mode-hook
;;        (lambda () (pabbrev-mode 1)))

;; W3M
(if (or linuxp macp) (require 'w3m-load))
;; Code folding
;; (load "folding" 'nomessage 'noerror)
;; (folding-mode-add-find-file-hook)

;; Anything
(require 'anything)

;; Protect buffers
(require 'keep-buffers)
(keep-buffers-erase-on-kill nil)
(keep-buffers-protect-buffer "*scratch*")
(keep-buffers-protect-buffer "*Messages*")

;; Pymacs
;; (if linuxp
;;     ((lambda ()
;;        (autoload 'pymacs-apply "pymacs")
;;        (autoload 'pymacs-call "pymacs")
;;        (autoload 'pymacs-eval "pymacs" nil t)
;;        (autoload 'pymacs-exec "pymacs" nil t)
;;        (autoload 'pymacs-load "pymacs" nil t)
;; ;;(eval-after-load "pymacs"
;; ;;  '(add-to-list 'pymacs-load-path YOUR-PYMACS-DIRECTORY"))
;; )))

;; Ido
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t) ;; enable fuzzy matching

;; Smex: M-x enhanced.  Built apon Ido
(require 'smex)
(smex-initialize)

;; Twittering-mode
(require 'twittering-mode)
(setq twittering-use-master-password t)

;; Use Emacs with Mutt
;; Enter `message-mode'
(add-to-list 'auto-mode-alist '("/mutt.*" . message-mode))
(setq mail-header-separator "")
(add-hook 'message-mode-hook
          (lambda ()
            (progn
              (turn-on-auto-fill)
              (message-goto-body)       ; Go to the beginning of message body.
              (local-unset-key "\C-c\C-c")
              ;; Redefine `C-c' as `C-x #'.
              (define-key message-mode-map "\C-c\C-c" '(lambda ()
                                                         "save and exit quickly"
                                                         (interactive)
                                                         (save-buffer)
                                                         (server-edit))))))

;; Command-log mode
(autoload 'command-log-mode "command-log-mode"
  "Load command-log-mode minor mode")
(autoload 'global-command-log-mode "command-log-mode"
  "Load command-log-mode global mode")

;; Multiple cursors
(autoload 'mc/edit-lines "multiple-cursors" "Load multiple cursors" t)
(autoload 'mc/mark-next-like-this "multiple-cursors" "Load multiple cursors" t)
(autoload 'mc/mark-previous-like-this "multiple-cursors"
  "Load multiple cursors" t)
(autoload 'mc/mark-all-like-this "multiple-cursors" "Load multiple cursors" t)

;;=============== global bindings ====================>
(if linux-x-p
    (global-set-key (kbd "<C-XF86KbdBrightnessUp>") 'list-bookmarks)
  (global-set-key (kbd "C-<f10>") 'list-bookmarks))
(global-set-key (kbd "s-p") 'previous-buffer)
(global-set-key (kbd "s-n") 'next-buffer)
(global-set-key (kbd "M-SPC") 'set-mark-command)
(global-set-key (kbd "C-x C-k") 'kill-this-buffer)
(global-set-key (kbd "M-w") 'copy-line-or-region)
(global-set-key (kbd "s-v") 'clipboard-yank)
(global-set-key (kbd "C-x M-f") 'find-file-at-point)
(global-set-key (kbd "C-x M-r") 'revert-buffer)
(global-set-key (kbd "C-x =") 'balance-windows)
(global-set-key (kbd "C-x +") 'what-cursor-position)
(global-set-key (kbd "C-x C-a") 'anything)
(global-set-key (kbd "M-n") 'toggle-linum)
(global-set-key (kbd "C-<f12>") 'org-remember)
(global-set-key (kbd "C-=") 'toggle-hiding)
(global-set-key (kbd "C-+") 'toggle-selective-display)
(global-set-key (kbd "M-RET") 'toggle-line-wrap)
(global-set-key "%" 'match-paren)
(global-set-key (kbd "M-C") 'compile)
(global-set-key (kbd "s-,") (lambda () (interactive) (find-file "~/.emacs.el")))
(global-set-key (kbd "M-/") 'complete-with-all-backends)

(global-set-key (kbd "C-<return>") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
;; Smex
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-M-x")
                'execute-extended-command) ; Used to be `eval-defun'.
;; Git-gutter
;; Jump to next/previous hunk
(global-set-key (kbd "C-x p") 'git-gutter:previous-hunk)
(global-set-key (kbd "C-x n") 'git-gutter:next-hunk)
;; Revert current hunk
(global-set-key (kbd "C-x r") 'git-gutter:revert-hunk)

;; =============== Look and Feel ===============>
(ignore-errors
  (cond
   (linuxp (setq fancy-splash-image my-splash-linux))
   (macp (setq fancy-splash-image my-splash-mac))))

;; Shorter modeline
(defvar mode-line-cleaner-alist
  `((auto-complete-mode . " α")
    (yas/minor-mode . " υ")
    (git-gutter-mode . " γ")
    (company-mode . " κ")
    (abbrev-mode . "")
    ;; Major modes
    (lisp-interaction-mode . "λ")
    (hi-lock-mode . "")
    (python-mode . "π")
    (emacs-lisp-mode . "Λ")
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

(add-hook 'after-change-major-mode-hook 'clean-mode-line)

(eval-after-load "whitespace"
  '(progn
     (set-face-attribute
      'whitespace-trailing nil
      :foreground "unspecified"
      :background (dv-color dv-type-fg))
     (set-face-attribute
      'whitespace-line nil
      :background "black"
      :foreground "grey"
      :inherit 'default)))

(if macp
    (progn
      ;; (add-to-list 'default-frame-alist '(height . 60))
      ;; (add-to-list 'default-frame-alist '(width . 110))
      (add-to-list 'default-frame-alist '(fullscreen . fullboth))
      (if window-system
          (load-theme 'Deviant t)
        (load-theme 'manoj-dark t))
      ;; Main font
      (set-face-attribute
       'default nil
       :family "The Sans Mono Condensed"
       :height 125
       :weight 'light)
      ;; CJK
      (set-fontset-font
       (frame-parameter nil 'font)
       'han
       (font-spec :family "Hiragino Sans GB"))
      ;; Font by charset
      ;; Punctuations
      (set-fontset-font
       (frame-parameter nil 'font)
       '(#x2000 . #x206f)
       (font-spec :family "The Sans Mono Condensed"))
      ;; en-dash, em-dash, and horizontal bar
      (set-fontset-font
       (frame-parameter nil 'font)
       '(#x2013 . #x2015)
       (font-spec :family "Hiragino Sans GB"))
      ;; Greek letters
      (set-fontset-font
       (frame-parameter nil 'font)
       '(#x0391 . #x03c9)
       (font-spec :family "Menlo"))
      ;; Arrows
      (set-fontset-font
       (frame-parameter nil 'font)
       '(#x2190 . #x21ff)
       (font-spec :family "Asana Math"))
      ;; Math symbols
      (set-fontset-font
       (frame-parameter nil 'font)
       '(#x2200 . #x22ff)
       (font-spec :family "Asana Math"))))

(if linux-x-p
    ((lambda ()
       ;; color themes
       (require 'color-theme)
       (color-theme-initialize)
       (color-theme-deviant)
       ;; Color theme in console
       (add-hook 'after-make-frame-functions
                 (lambda (frame)
                   (set-variable 'color-theme-is-global nil)
                   (select-frame frame)
                   (if (not window-system)
                       (color-theme-charcoal-black))))

       ;; Change cursor color when buffer is readonly or in `overwrite-mode'.
       (setq hcz-set-cursor-color-color "")
       (setq hcz-set-cursor-color-buffer "")
       (defun hcz-set-cursor-color-according-to-mode ()
         "change cursor color according to some minor modes."
         ;; set-cursor-color is somewhat costly, so we only call it when needed:
         (let ((color
                (if buffer-read-only "white"
                  (if overwrite-mode "red"
                    "green"))))
           (unless (and
                    (string= color hcz-set-cursor-color-color)
                    (string= (buffer-name) hcz-set-cursor-color-buffer))
             (set-cursor-color (setq hcz-set-cursor-color-color color))
             (setq hcz-set-cursor-color-buffer (buffer-name)))))
       (add-hook 'post-command-hook 'hcz-set-cursor-color-according-to-mode)

       ;; Font for info
       (add-hook 'Info-mode-hook
                 (lambda ()
                   (set-face-attribute
                    'info-title-1 nil
                    :height 1.728
                    :family "URW Palladio L")
                   (set-face-attribute
                    'info-title-2 nil
                    :height 1.44
                    :family "URW Palladio L")
                   (set-face-attribute
                    'info-header-xref nil
                    :foreground "SeaGreen2"
                    :weight 'bold
                    :family "URW Palladio L")))

       ;; Misc. color
       (set-face-attribute
        'default nil
        :foreground "#dddddd"
        :background "#2e3735")
       (set-face-attribute
        'mode-line nil
        :family "Inconsolata"
        :background "#afc81c" :foreground "#2e3735"
        ;;   :height 80
        :box nil)
       (set-face-attribute
        'mode-line-inactive nil
        :family "Inconsolata"
        :background "#152525" :foreground "#808080"
        :box nil)
       (set-face-attribute
        'mode-line-buffer-id nil
        :background "#afc81c" :foreground "#2e3735"
        :box nil)
       (set-face-attribute
        'fringe nil
        :background "#333333")
       (set-face-attribute
        'region nil
        :background "#afc81c" :foreground "#2e3735")
       (global-hl-line-mode t)
       (set-face-attribute
        'hl-line nil
        :inherit 'highlight :background "#333333")
       (set-face-attribute
        'variable-pitch nil
        :family "Helvetica LT Std")
       ;; line number
       (if linuxp
           (set-face-attribute
            'linum nil
            :foreground "#556664"))
       ))
)

(require 'powerline)
;; (defpowerline status "%*%Z")
;; (defpowerline global global-mode-string)
(defun powerline-deviant-theme ()
  "Setup the default mode-line."
  (interactive)

  (set-face-attribute
   'powerline-active2 nil
   :foreground (dv-color dv-default-bg))
  (set-face-attribute
   'powerline-active1 nil
   :foreground (dv-color dv-default-fg))
  (set-face-attribute
   'powerline-inactive2 nil
   :foreground (dv-color dv-default-fg))
  (set-face-attribute
   'powerline-active1 nil
   :foreground (dv-color dv-default-fg))

  (defvar some-image (create-image "~/programs/nyan.png" 'png nil
                                   :ascent 'center))

  (setq-default
   mode-line-format
   '("%e"
     (:eval
      (let* ((active (powerline-selected-window-active))
             (mode-line (if active 'mode-line 'mode-line-inactive))
             (face1 (if active 'powerline-active1 'powerline-inactive1))
             (face2 (if active 'powerline-active2 'powerline-inactive2))
             (separator-left
              (intern (format "powerline-%s-%s"
                              powerline-default-separator
                              (car powerline-default-separator-dir))))
             (separator-right
              (intern (format "powerline-%s-%s"
                              powerline-default-separator
                              (cdr powerline-default-separator-dir))))
             (lhs (list (powerline-raw "%*%z" nil 'l)
                        ;; (powerline-buffer-size nil 'l)
                        ;; (powerline-raw mode-line-mule-info nil 'l)
                        (powerline-buffer-id nil 'l)
                        (when (and (boundp 'which-func-mode) which-func-mode)
                          (powerline-raw which-func-format nil 'l))
                        (powerline-raw " ")
                        (funcall separator-left mode-line face1)
                        (powerline-major-mode face1 'l)
                        (powerline-process face1)
                        (powerline-minor-modes face1 'l)
                        (powerline-narrow face1 'l)
                        (powerline-raw " " face1)
                        (funcall separator-left face1 face2)
                        (powerline-vc face2 'r)
                        (when (boundp 'erc-modified-channels-object)
                          (powerline-raw erc-modified-channels-object
                                         face2 'l))))
             (rhs (list (powerline-raw global-mode-string face2 'r)
                        (funcall separator-right face2 face1)
                        (powerline-raw "%l" face1 'l)
                        (powerline-raw ":" face1 'l)
                        (powerline-raw "%c" face1 'r)
                        (funcall separator-right face1 mode-line)
                        (powerline-raw " ")
                        (powerline-raw "%4p" nil 'r)
                        (if active (powerline-raw
                                    (propertize "unimportant"
                                                'display some-image)))
                        ;; (powerline-hud face2 face1))))
                        )))
        (concat (powerline-render lhs)
                (powerline-fill face2 (powerline-width rhs))
                (powerline-render rhs)))))))

(powerline-deviant-theme)

(eval-after-load (expand-file-name "~/.emacs-erc.el")
  '(progn
     (set-face-attribute
      'erc-notice-face nil
      :foreground (dv-color dv-builtin-fg)
      :weight 'unspecified)))

;; =============== add other file ====================>
(autoload 'erc-start (expand-file-name "~/.emacs-erc.el")
  "Load my ERC configuration" t)
(if linuxp (load-file (expand-file-name "~/.emacs-dict.el")))
(if linuxp (load-file (expand-file-name "~/.emacs-slime.el")))
(load-file (expand-file-name "~/.emacs-muse.el"))
;; (load-file (expand-file-name "~/.emacs-icicles.el"))
(load-file (expand-file-name "~/.emacs-org.el"))
(load-file (expand-file-name "~/.emacs-calendar.el"))
(load-file (expand-file-name "~/.emacs-skeletons.el"))
(if linuxp (load-file (expand-file-name "~/.emacs-emms.el")))
(load-file (expand-file-name "~/.emacs-tex.el"))
;; (load-file "/home/corsair/.emacs-blogmax.el")
;; (load-file "~/.emacs-predictive.el")
(load custom-file 'noerror)

;; (if macp (progn (make-frame) (ns-toggle-fullscreen)))
(delete-other-windows)
(split-window-right)
(server-start)
