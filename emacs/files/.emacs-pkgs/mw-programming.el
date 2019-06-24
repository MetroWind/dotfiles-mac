(use-package cc-mode
  ;; key bindings for all supported languages.  We can put these in
  ;; c-mode-base-map because c-mode-map, c++-mode-map, objc-mode-map,
  ;; java-mode-map, idl-mode-map, and pike-mode-map inherit from it.
  :bind (:map c-mode-base-map
              ("C-m" . c-context-line-break))
  ;; we like auto-newline and hungry-delete
  :hook (c-mode-common . (lambda () (c-toggle-auto-hungry-state 1)))
  :config
  ;; CC Indention
  ;; offset customizations not in my-c-style
  (setq-default c-offsets-alist '((member-init-intro . ++)))

  (setq-default c-basic-offset 4)
  ;; add my personal style and set it for the current buffer
  (setq-default c-default-style "bsd"
                c-basic-offset 4)

  ;; Whether the indentation should be controlled by the syntactic context.
  (setq-default c-syntactic-indentation t))

(use-package vc-git
  :config
  (add-to-list 'vc-handled-backends 'git))

(use-package magit
  :commands magit-status
  :config
  (setq magit-log-margin '(t "%Y-%m-%d" magit-log-margin-width t 14))
  (setq magit-bury-buffer-function 'quit-window)

  (if (boundp 'transient-default-level)
      ;; Transient replaces the deprecated magit popup.
      (progn
        ;; The default GPG sign option prompt for a key choice (the
        ;; option is at transient level 5, so it’s hidden by default).
        ;; Change the enabling key to “=S”.
        (transient-suffix-put 'magit-commit "-S" :key "=S")
        (transient-suffix-put 'magit-rebase "-S" :key "=S")
        (transient-suffix-put 'magit-cherry-pick "-S" :key "=S")
        ;; Add “-S” to sign using the default GPG key specified in Git
        ;; config.
        (transient-insert-suffix 'magit-commit "=S"
          '(1 "-S" "Sign using GPG’s default key" "--gpg-sign"))
        (transient-insert-suffix 'magit-rebase "=S"
          '(1 "-S" "Sign using GPG’s default key" "--gpg-sign"))
        (transient-insert-suffix 'magit-cherry-pick "=S"
          '(1 "-S" "Sign using GPG’s default key" "--gpg-sign"))

        (setq transient-display-buffer-action '(display-buffer-below-selected)))
    ;; Magit popup is deprecated
    (progn
      (magit-define-popup-switch 'magit-commit-popup ?S
        "Sign commit using default key" "--gpg-sign")
      (magit-define-popup-switch 'magit-rebase-popup ?S
        "Sign commit using default key" "--gpg-sign")
      (magit-define-popup-switch 'magit-cherry-pick-popup ?S
        "Sign commit using default key" "--gpg-sign"))))

(use-package compile
  :bind ("M-C" . compile)
  :config
  ;; Make make command print directory information
  (setq-default compile-command "make -w"))

(use-package subword
  :hook ((c-mode-common . subword-mode)
         (python-mode . subword-mode)))

(use-package whitespace
  :after (:any cc-mode python yaml-mode)
  :hook ((emacs-lisp-mode c-mode-common python-mode yaml-mode)
         . whitespace-mode)
  :config
  ;; (if (not use-pkg-p)
  ;;          (progn
  ;;            (autoload 'whitespace-mode "whitespace" nil t)
  ;;            (autoload 'whitespace-cleanup "whitespace" nil t)))
  (defvar my-whitespace-style '(face tab-mark trailing lines-tail space-before-tab))
  (setq-default whitespace-style my-whitespace-style)
  (setq-default whitespace-active-style my-whitespace-style)
  (setq-default whitespace-line-column 80))

(use-package highlight-indent-guides
  :hook (yaml-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'fill)
  ;; Let theme determine indent highlight color
  (setq highlight-indent-guides-auto-enabled nil))

;; (use-package indent-guide
;;   :hook (yaml-mode . indent-guide-mode)
;;   :config
;;   (setq indent-guide-char "⎸"))

(use-package python
  ;; There’s a bug in python-mode that fills the 1st line of a
  ;; docstring incorrectly. The fill width is counted from the """,
  ;; where for the rest of the lines, the fill is counted from the
  ;; beginning of the line. See bugs
  ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=20860 and
  ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=21254, and my
  ;; showcase
  :hook (python-mode . (lambda () (setq-local fill-column 80)))
  :mode ("\\.py\\'" . python-mode)
  :config
;; (setq python-environment-virtualenv
;;       (append python-environment-virtualenv
;;               '("--python" "/Users/sun/Python3/bin/python3")))
  (setq python-fill-docstring-style 'pep-257-nn))

(use-package anaconda-mode
  :disabled
  :if (ensure-single-package-installed 'anaconda-mode)
  :after (python eldoc)
  :hook (python-mode
         (python-mode . anaconda-eldoc-mode)))

;; Hide show mode (from emacswiki)
(use-package hideshow
  :hook ((c-mode-common emacs-lisp-mode java-mode lisp-mode perl-mode sh-mode
                        python-mode) . hs-minor-mode)
  :bind (("C-=" . toggle-hiding)
         ("C-+" . toggle-selective-display))

  :config
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
      (toggle-selective-display column))))

(use-package edebug
  :init
  ;; Prohibit emacs-lisp-mode-map to use C-x C-a as a prefix.
  (setq edebug-inhibit-emacs-lisp-mode-bindings t))

(use-package flycheck
  :if (package-installed-p 'flycheck)
  :hook (python-mode . flycheck-mode)
  :config
  (setq flycheck-mode-line
        '(:eval (format "x%d" (length flycheck-current-errors))))
  (setq flycheck-python-pycompile-executable "python3")
  (setq-default flycheck-clang-language-standard "c++17"))

(use-package nxml-mode
  :if (package-installed-p 'nxml-mode)
  :magic "<?[Xx][Mm][Ll] "
  ;; Add XSLT 2.0 and 3.0 schemas
  :hook (nxml-mode-hook
         .
         (lambda ()
           (setq rng-schema-locating-files
                 (append
                  ;; Schemas.xml in current dir.
                  (list (car rng-schema-locating-files))
                  (list (expand-file-name
                         "~/.emacs-pkgs/schemas/xslt-relax-ng/xslt.rnc"))
                  ;; Emacs' schemas.xml
                  (cdr rng-schema-locating-files))))))

(use-package web-mode
  :mode "\\.html?$"
  :if (package-installed-p 'web-mode)
  :config
  (setq-default web-mode-markup-indent-offset 2))

(use-package git-commit
  :if (package-installed-p 'git-commit)
  :commands git-commit-mode
  :mode ("COMMIT_EDITMSG" . git-commit-mode)
  :hook (git-commit-mode
         . (lambda () (yas-activate-extra-mode 'git-commit-mode))))

;; I need to change my habit *a lot* to use this. Disable for now.
(use-package mwim
  :hook prog-mode
  :disabled
  :bind (:map prog-mode-map
              ("C-a" . mwim-beginning)
              ("C-e" . mwim-end)))

(use-package hungry-delete
  :if (package-installed-p 'hungry-delete)
  ;; For some reason C-d is binded by default, but backspace is not...
  :bind (:map hungry-delete-mode-map
              ("DEL" . hungry-delete-backward))
  :hook ((prog-mode . hungry-delete-mode)
         (python-mode . (lambda () (hungry-delete-mode -1)))
         (cc-mode . (lambda () (hungry-delete-mode -1)))))

(use-package gdb
  :init
  (setq gud-key-prefix (kbd "C-M-G")))

(provide 'mw-programming)
