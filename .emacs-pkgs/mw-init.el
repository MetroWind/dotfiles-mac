(require 'cl)

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
(defconst use-pkg-p (>= emacs-major-version 24))

(if (boundp 'my-emacsd-location)
    (if my-emacsd-location
        (setq-default user-emacs-directory my-emacsd-location)))

(if (<= emacs-major-version 22)
    (progn
      (set-selection-coding-system 'utf-8)
      (set-clipboard-coding-system 'utf-8)
      (set-keyboard-coding-system 'utf-8) ; if utf8, unable to use input method.
      (set-language-environment 'utf-8)
      (setq locale-coding-system 'utf-8)
      (setq current-language-environment "utf-8")))

;; ========== Add load path =========================================>
(if (>= emacs-major-version 24)
    (add-to-list 'custom-theme-load-path
                 (concat ModeDir "/themes/")))
(add-to-list 'load-path (concat ModeDir "/notmuch"))
(add-to-list 'load-path (concat ModeDir "/lilypond"))

;; ========== Packages ==============================================>
(defun read-obj-from-file (filename)
  "Read and return a lisp object from file at `filename'. If
fail, return nil."
  (condition-case nil
      (with-temp-buffer
        (insert-file-contents filename)
        (read (current-buffer)))
    (error nil)))

(defun save-obj-to-file (filename obj)
  "Save lisp object `obj' to file at `filename'."
  (condition-case nil
      (with-temp-buffer
        (print obj (current-buffer))
        (set-visited-file-name filename)
        (save-buffer)
        t)
    (error nil)))

(defconst mw-denied-packages-file
  (if (boundp 'user-emacs-directory)
      (concat user-emacs-directory "/denied-packages.el")
    "~/.emacs.d/denied-packages.el"))

(setq debug-on-message "Wrong type argument")

(if use-pkg-p
    (progn
      (require 'package)
      (setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                               ("melpa" . "https://melpa.org/packages/")))
      (setq package-enable-at-startup nil)
      (package-initialize)
      ;; http://stackoverflow.com/a/10095853/782130
      (defun ensure-package-installed (&rest packages)
        "Assure every package is installed, ask for installation if it’s not.
Return a list of installed packages or nil for every skipped package."
        (let ((denied (read-obj-from-file mw-denied-packages-file))
              (new-denied nil))
          (let ((result
                 (mapcar
                  (lambda (package)
                    ;; (package-installed-p 'evil)
                    (if (memq package denied)
                        ;; Previously the use chose to not install this
                        ;; package. Return nil directly.
                        nil
                      (if (package-installed-p package)
                          t
                        (if (y-or-n-p (format "Package %s is missing. Install it? " package))
                            (package-install package)
                          ;; Use chose to not install this package. Update
                          ;; denied file.
                          (set 'new-denied t)
                          (set 'denied (cons package denied))
                          nil))))
                  packages)))

          (if new-denied
              (save-obj-to-file mw-denied-packages-file denied))

          result)))

      (defun ensure-single-package-installed (package)
        (car (ensure-package-installed package)))

      ;; make sure to have downloaded archive description.
      ;; Or use package-archive-contents as suggested by Nicolas Dudebout
      (or (file-exists-p package-user-dir)
          (package-refresh-contents))

      (if (not (package-installed-p 'use-package))
          (package-install 'use-package))
      ))

(eval-when-compile (require 'use-package))
(setq use-package-verbose t)
(setq use-package-always-defer t)

(provide 'mw-init)
