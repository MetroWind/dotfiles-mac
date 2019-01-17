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
  (setq company-clang-arguments '("-std=c++14"))
  (setq company-backends
        '(company-elisp
          company-anaconda
          (company-clang company-xcode)
          (company-gtags company-etags)
          company-nxml company-css
          (company-files company-keywords)
          company-capf
          (company-dabbrev-code company-dabbrev)))
  (use-package company-quickhelp
    :ensure t
    :demand t
    :config
    (company-quickhelp-mode 1))

  ;; Eliminate anaconda’s “too many open files” error
  (add-to-list 'url-proxy-services
               '("no_proxy" . "^\\(127.0.0.1\\|localhost\\|10.*\\)"))
)

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

(provide 'mw-completion)
