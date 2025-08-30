(require 'mw-lib-generic)

(use-package helm-mode
  :ensure helm
  :bind
  (("C-x C-f" . helm-find-files)
   ("M-x" . helm-M-x)
   ("M-." . helm-gtags-dwim) ; Default: find-tag
   ("M-y" . helm-show-kill-ring)
   ("C-x C-a" . helm-mini)
   ("M-s o" . helm-occur)
   ("C-x b" . helm-buffers-list)
   ("C-x r b" . helm-filtered-bookmarks))

  :hook (after-init . (lambda () (helm-mode 1)))
  :init
  (if (not use-pkg-p)
      (require 'helm-config))

  :config
  ;; Let C-n go across sources (sections).
  (setq helm-move-to-line-cycle-in-source nil)
  (setq helm-buffer-max-length 40))

;; Ido
(use-package ido
  :disabled
  :config
  (ido-mode t)
  ;; enable fuzzy matching
  (setq ido-enable-flex-matching t))

;; Company
(use-package company
  :when (equal my-inline-completion-frontend 'company)
  :ensure t
  :hook (after-init . global-company-mode)
  ;; :bind ("M-/" . complete-with-all-backends)
  :bind (("M-/" . company-complete)
         :map company-active-map
         ("C-n" . company-select-next-or-abort)
         ("C-p" . company-select-previous-or-abort)
         ("M-/" . company-select-next-or-abort)
         ("RET" . company-complete-selection)
         ("TAB" . company-complete-common))

  :config
  ;; (autoload 'company-mode "company" nil t)
  (defun complete-with-all-backends ()
    (interactive)
    (if (not (company-complete))
        (company-other-backend)))
  (setq company-dabbrev-ignore-case nil)
  (setq company-dabbrev-downcase nil)
  (setq company-async-timeout 5)
  (setq company-idle-delay 2)
  (setq company-clang-arguments '("-std=c++17"))
  (setq company-backends
        '(company-capf
          (company-cmake company-clang company-xcode)
          (company-gtags company-etags)
          company-nxml company-css
          (company-files company-keywords)
          (company-dabbrev-code company-dabbrev)))
  (use-package company-quickheemlp
    :disabled
    :ensure t
    :demand t
    :config
    (company-quickhelp-mode 1))
  )

(use-package corfu
  :if (equal my-inline-completion-frontend 'corfu)
  :ensure t
  :bind (("M-/" . completion-at-point))
  :init
  (global-corfu-mode)
  :custom
  (corfu-cycle t) ;; Enable cycling for `corfu-next/previous'
  )

;; For python, python-language-server is required. Install:
;;
;;   pip install 'python-language-server[pycodestyle]'
;;
;; For C++, clangd (which is part of clang) should be installed, and
;; `lsp-clangd' should be loaded in Emacs.
(use-package lsp-mode
  :if (and (not (null-or-unboundp 'my-inline-completion-frontend))
           (equal my-inline-completion-backend 'lsp-mode))
  :ensure t
  :hook ((python-mode rust-mode c++-mode) . lsp)
  :init
  (setq lsp-keymap-prefix "M-S-l")
  :config
  ;; Don’t use yasnippet to expand function calls.
  (setq lsp-pyls-plugins-jedi-completion-include-params nil)
  ;; Disable style checking
  (setq lsp-pyls-plugins-pycodestyle-enabled nil)
  (setq lsp-prefer-flymake :none)
  (setq lsp-signature-render-documentation nil) ; Only show sig, not doc.
  ;; In general don’t use LSP for `indent-region'.
  (setq lsp-enable-indentation nil)
  )

(use-package eglot
  :if (and (not (null-or-unboundp 'my-inline-completion-frontend))
           (equal my-inline-completion-backend 'eglot))
  :hook ((python-mode rust-mode c++-mode) . eglot-ensure)
  :hook (eglot-managed-mode . (lambda () (eglot-inlay-hints-mode -1))))

(use-package company-lsp
  :if (and (equal (with-default 'my-inline-completion-frontend nil) 'company)
           (equal my-inline-completion-backend 'lsp-mode))
  :commands company-lsp)

(use-package jedi-core
  :disabled
  :config
  ;; Use pip3 instead of pip.
  (setq jedi:install-server--command
        (cons "pip3" (cdr jedi:install-server--command)))

  (setq jedi:server-args
        (list "--sys-path" (expand-file-name "~/.Python")
              "--virtual-env" (expand-file-name "~/Python3")
              "--virtual-env" (expand-file-name "~/Python"))))

(use-package completion-preview
  :hook ((prog-mode text-mode comint-mode) . completion-preview-mode)
  :bind (:map completion-preview-active-mode-map
              ("M-n" . completion-preview-next-candidate)
              ("M-p" . completion-preview-prev-candidate))
  :config
  ;; Show the preview already after two symbol characters
  (setq completion-preview-minimum-symbol-length 2))

(provide 'mw-completion)
