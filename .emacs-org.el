;; (require 'org-install)

(message "Loading org configurations...")
(require 'ox-latex)
(require 'org-table)

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

(setq org-export-html-style
      "<style type=\"text/css\">
html
{ 
    font-family: sans-serif;
    font-size: 12pt;
    background-color: #262626;
}

body
{
    width: 540pt;
    padding: 20pt;
    margin-left: auto;
    margin-right: auto;
    margin-top: 0pt;
    color: #bbb;
    border-top: 10pt solid #ffbc00;
}

a
{
    color: #ffbc00;
}

b
{
    font-family: monospace;
    font-weight: normal;
    font-size: 80%;
}

ul
{
    list-style-image: url(data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAQAAAAECAYAAACp8Z5+AAAABHNCSVQICAgIfAhkiAAAAAlwSFlzAAAAfwAAAH8BuLbMiQAAABl0RVh0U29mdHdhcmUAd3d3Lmlua3NjYXBlLm9yZ5vuPBoAAAAWSURBVAiZYzx16tR/BiTAxIAGCAsAAN1QA2Xx3HxpAAAAAElFTkSuQmCC);
}

h1, h2, h3, h4
{
    font-weight: normal;
}

h3
{
    font-family: serif;
    font-variant: italic;
}

.title
{
    text-align: center;
    font-family: serif;
}

.todo, .done, .timestamp, .timestamp-kwd
{
    font-family: monospace;
    font-size: 50%;
    background-color: transparent;
}

.tag
{
    font-family: monospace;
    font-size: 70%;
    color: #666;
    background-color: transparent;
}

pre, code
{
    background-color: #303030;
    color: #b6beb4;
    border: none; 
    font-family: monospace;
    font-size: 80%;
}

</style>")
