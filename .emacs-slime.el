;; Slime
(setq inferior-lisp-program "/usr/bin/clisp")
(require 'slime)
(slime-setup)

;; Slime48
(autoload 'slime "slime"
  "Start an inferior^_superior Lisp and connect to its Swank server."
  t)

(autoload 'slime-mode "slime"
  "SLIME: The Superior Lisp Interaction Mode for Emacs (minor-mode)."
  t)

(eval-after-load "slime"
  '(progn
     (slime-setup)
     (setq slime-lisp-implementations
           `((s48 ("scheme48") :init slime48-init-command)
             ,@slime-lisp-implementations))))

(autoload 'slime48-init-command "slime48"
  "Return a string to initialize Scheme48 running under SLIME.")

;; This snippet lets you specify a scheme48-package local variable,
;; in a file's -*- line or local variables section, and have SLIME48
;; automatically evaluate code in the right package.  For instance,
;; all of my Scheme48 source files start with:
;;   ;;; -*- Mode: Scheme; scheme48-package: ... -*-
(eval-after-load "slime48"
  '(add-hook 'slime-mode-hook
             (lambda ()
               (if (and (boundp 'scheme48-package)
                        scheme48-package)
                   (setq slime-buffer-package scheme48-package)))))

;; This disables a feature of SLIME that is not implemented in
;; SLIME48.  The feature was harmless before, but recent changes to
;; it in SLIME CVS made it harmful.
(setq slime-space-information-p nil)
