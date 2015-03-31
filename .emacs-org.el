;; (require 'org-install)

(message "Loading org configurations...")

;; (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(add-hook 'org-mode-hook 'turn-on-font-lock)

(setq org-log-done t)
(setq org-agenda-include-diary t)
(setq org-icalendar-include-todo t)
(setq org-startup-folded nil)           ;Show all content on startup.
(setq org-todo-keywords '((sequence "TODO" "|" "DONE" "CANCELED")))
(setq org-agenda-dim-blocked-tasks t)
(setq org-enforce-todo-dependencies t)

;; When creating a sparse tree from matching, show the whole content
;; of the sparse tree.  This is for convenient LaTeX export.
(setq org-show-entry-below '((default . nil) (tags-tree . t) (occur-tree . t)))

;; Set to the location of your Org files on your local system
(setq org-directory
      (cond
       (linuxp "/mnt/shared/text/Non-books/Misc/org")
       (macp "/Volumes/Stuff/text/Non-books/Misc/org")))

(cond
 (linuxp
  (setq org-agenda-files `(,(concat org-directory "/plan.org")
                           "~/document/physics/lattice/doc/tech.org")))
 (macp
  (setq org-agenda-files `(,(concat org-directory "/plan.org")
                           "/Volumes/Stuff/document/physics/lattice/doc/tech.org"))))

;; Set to the name of the file where new notes will be stored
(setq org-mobile-inbox-for-pull (concat org-directory "/plan.org"))
;; Set `org-capture'
(setq org-default-notes-file (concat org-directory "/plan.org"))
;; Set to <your Dropbox root directory>/MobileOrg.
(setq org-mobile-directory "~/Dropbox/MobileOrg")

(add-hook 'org-mode-hook
          (lambda () (define-key org-mode-map (kbd "C-c C-a") 'show-all)))

(setq org-hide-leading-stars t)
;; (if (window-system)
;;     (add-hook 'org-mode-hook
;;               (lambda ()
;;                 (set-face-attribute
;;                  'org-hide nil
;;                  :foreground "#2e3735"))))

;; Export settings
;; Use better engine for LaTeX
(setq org-latex-pdf-process
      (make-list 3 "xelatex -interaction nonstopmode -output-directory %o %f"))

(defun my-latex-filter-headline-done (text backend info)
  "Ensure dots in headlines."
  (when (org-export-derived-backend-p backend 'latex)
    (save-match-data
      (when (let ((case-fold-search t))
              (string-match "\\\\\\([a-z]+\\){\\(.*DONE.*\\)}"
                            text))
        (if (not (string-match ".*hsout.*" text))
            (replace-match "\\\\\\1{\\\\hsout{\\2}}"
                       t nil text))))))
(defun my-latex-filter-headline-canceled (text backend info)
  "Ensure dots in headlines."
  (when (org-export-derived-backend-p backend 'latex)
    (save-match-data
      (when (let ((case-fold-search t))
              (string-match "\\\\\\([a-z]+\\){\\(.*CANCELED.*\\)}"
                            text))
        (if (not (string-match ".*hsout.*" text))
            (replace-match "\\\\\\1{\\\\hsout{\\2}}"
                       t nil text))))))

(eval-after-load 'ox
  '(progn
     (add-to-list 'org-export-filter-headline-functions
                  'my-latex-filter-headline-done)
     (add-to-list 'org-export-filter-headline-functions
                  'my-latex-filter-headline-canceled)
     (setq
      org-format-latex-header
      (concat
       org-format-latex-header
       "\n\\DeclareRobustCommand{\\hsout}[1]{\\texorpdfstring{\\sout{#1}}{#1}}"))))

(setq org-latex-default-packages-alist
  '((""   "fontenc"   t)
    (""     "fixltx2e"  t)
    ("" "xltxtra" nil)
    ("" "newunicodechar" t)
    (""     "graphicx"  t)
    (""     "longtable" nil)
    (""     "float"     nil)
    (""     "wrapfig"   nil)
    (""     "rotating"  nil)
    ("normalem" "ulem"  t)
    (""     "amsmath"   t)
    (""     "textcomp"  t)
    (""     "marvosym"  t)
    (""     "wasysym"   t)
    (""     "amssymb"   t)
    (""     "hyperref"  nil)
    "\\tolerance=1000"
    "\\DeclareRobustCommand{\\hsout}[1]{\\texorpdfstring{\\sout{#1}}{#1}}"
  ))

(setq org-html-doctype "html5")
(setq org-html-html5-fancy t)
(setq org-html-head
      "<link rel=\"stylesheet\" href=\"style.css\">")

;; (require 'org-install)
;; (message "Loading org configurations...")
;; (require 'ox-latex)
;; (require 'org-table)
