;; (require 'org-install)

(require 'mw-lib-generic)

(use-package org
  :demand t
  :commands org-open
  :mode ("\\.org$" . org-mode)
  :bind (("C-<f12>" . org-remember)
         :map org-mode-map
         ("C-c C-a" . outline-show-all)
         ("C-c w" . refile-subtree-to-new-file))
  :chords (("oc" . org-capture))
  :config

;; (define-key global-map "\C-cl" 'org-store-link)
;; (define-key global-map "\C-ca" 'org-agenda)
  (add-hook 'org-mode-hook 'turn-on-font-lock)

  (setq org-log-done t)
  (setq org-agenda-include-diary t)
  (setq org-icalendar-include-todo t)
  (setq org-startup-folded nil)           ;Show all content on startup.
  (setq org-todo-keywords
        '((sequence
           "TODO(t)"
           "SOMEDAY(s)"                    ; Basically means “never will”
           "WAIT(w@)" "|"
           "POTTED(p@)"                    ; 已甩锅
           "DONE(d)"
           "CANCELED(c)")))
  (setq org-agenda-dim-blocked-tasks t)
  (setq org-enforce-todo-dependencies t)
  (setq org-clock-idle-time 10)
  (setq org-clock-string-limit 16)
  (setq org-export-headline-levels 5)
  (setq org-highlight-latex-and-related '(latex))
  (setq org-outline-path-complete-in-steps nil)

  (setq org-fontify-done-headline nil)
  (setq org-fontify-todo-headline nil)

  ;; When creating a sparse tree from matching, show the whole content
  ;; of the sparse tree.  This is for convenient LaTeX export.
  (setq org-show-entry-below '((default . nil) (tags-tree . t) (occur-tree . t)))
  ;; Set to the location of your Org files on your local system
  (setq org-agenda-files my-org-files)
  (if (bound-and-true 'my-org-dir)
      (setq org-directory my-org-dir))
  ;; Set `org-capture'
  (if (bound-and-true 'my-org-capture-file)
      (setq org-default-notes-file my-org-capture-file))
  ;; Don’t mess with my windows!!!!!!!!!!!!!!
  (setq org-agenda-window-setup 'other-window)

  (setq org-hide-leading-stars t)
  ;; (if (window-system)
  ;;     (add-hook 'org-mode-hook
  ;;               (lambda ()
  ;;                 (set-face-attribute
  ;;                  'org-hide nil
  ;;                  :foreground "#2e3735"))))

  ;; Enable virtual indentation globally. This will indent the lines
  ;; according to head level, with virtual spaces. This also means
  ;; `org-adapt-indentation’ being nil. This looks awful for files
  ;; with hard-coded indentation. To turn this off in a file, write
  ;; “#+STARTUP: noindent”.
  (setq org-startup-indented t)

  ;; Enable code execution
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t)
     (python . t)
     (sqlite . t)))

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

  (defun org-open ()
    "Open my org files."
    (interactive)
    (dolist (f my-org-files)
      (find-file-noselect f)))

  (defun refile-subtree-to-new-file ()
    "Refile the current subtree into a new file under
 `org-directory'. The filename is derived from the subtree title."
    (interactive)
    (let* (
           ;; Get title of current heading
           (title (org-element-interpret-data
                   (org-element-property :title (org-element-at-point))))
           (slug (sluggify title))
           (filename (format "%s-%s.org"
                             (format-time-string "%Y-%m-%d")
                             slug))
           (filepath (expand-file-name filename org-directory)))
      (if (string-empty-p slug)
          (error "Slug is empty.")
        (org-copy-subtree nil t)
        (find-file-other-window filepath)
        (org-paste-subtree))))
)

(use-package org-agenda
  :bind (([remap org-agenda-goto-today] . org-agenda-schedule-today)
         ([remap org-agenda-date-prompt] . org-agenda-schedule-tomorrow)
         ([remap org-agenda-filter] . org-agenda-schedule-next-monday)
         ("C-c ." . org-agenda-goto-today)
         ("C-c >" . org-agenda-date-prompt)
         ("C-c /" . org-agenda-filter))
  :init
  (defun org-agenda-schedule-today (arg)
    "Schedule the current time to today."
    (interactive "P")
    (org-agenda-schedule arg (current-time)))

  (defun org-agenda-schedule-tomorrow (arg)
    "Schedule the current time to tomorrow."
    (interactive "P")
    (org-agenda-schedule
     arg
     (encode-time (decoded-time-add (decode-time)
                                    (make-decoded-time :day 1)))))

  (defun org-agenda-schedule-next-monday (arg)
    "Schedule the current time to next Monday."
    (interactive "P")
    (let* ((time (decode-time))
           (dow (decoded-time-weekday time))
           (delta (if (= dow 0)
                      1
                    (+ (- 7 dow) 1)))
           (new-time (decoded-time-add time (make-decoded-time :day delta))))
      (org-agenda-schedule arg (encode-time new-time))))
  )

;; Org export
(use-package ox
  :config
  (add-to-list 'org-export-filter-headline-functions
               'my-latex-filter-headline-done)
  (add-to-list 'org-export-filter-headline-functions
               'my-latex-filter-headline-canceled)
  (setq
   org-format-latex-header
   (concat
    org-format-latex-header
    "\n\\DeclareRobustCommand{\\hsout}[1]{\\texorpdfstring{\\sout{#1}}{#1}}"))

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
  (setq org-html-htmlize-output-type 'css)

  ;; Embed inline CSS read from a file.
  (defun my-org-inline-css-hook (exporter)
    "Insert custom inline css"
    (when (eq exporter 'html)
      (let* ((dir (ignore-errors (file-name-directory (buffer-file-name))))
             (path (concat dir "style.css"))
             (homestyle (and (or (null dir) (null (file-exists-p path)))
                             (bound-and-true 'my-org-inline-css-file)))
             (final (if homestyle my-org-inline-css-file path)))
        (if (file-exists-p final)
            (progn
              (setq-local org-html-head-include-default-style nil)
              (setq-local org-html-head (concat
                                         "<style type=\"text/css\">\n"
                                         "<!--/*--><![CDATA[/*><!--*/\n"
                                         (with-temp-buffer
                                           (insert-file-contents final)
                                           (buffer-string))
                                         "/*]]>*/-->\n"
                                         "</style>\n")))))))

  (add-hook 'org-export-before-processing-hook 'my-org-inline-css-hook)
  )

(use-package org-roam
  :demand t
  :ensure t
  :chords (("ob" . org-roam-buffer-toggle))
  :config
  (if (bound-and-true 'my-roam-dir)
      (progn
        (message (concat "Setting roam dir to " my-roam-dir))
        (setq org-roam-directory my-roam-dir)
        (org-roam-db-autosync-mode))
    (message ("Not setting roam dir.")))
  (setq org-roam-dailies-directory "journal/")
  ;; Default daily template but unnarrow.
  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry "* %?" :target
           (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>
#+CATEGORY: journal")
           :unnarrowed nil)))

  (setq org-roam-capture-templates
        '(("d" "Random thoughts" plain nil :target
           (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                      "#+title: ${title}
#+CATEGORY: thoughts\n")
           :unnarrowed t)
          ("r" "Reading note" plain nil :target
           (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                      ":PROPERTIES:
:ROAM_REFS: %^{Source}
:SourceType: %^{Source type|article|book|video|podcast|microblog|paper}
:END:
#+TITLE: ${title}
#+FILETAGS: :Draft:
#+CATEGORY: reading\n")
           :unnarrowed t)
          ("p" "Permanent note" plain nil :target
           (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                      "#+title: ${title}
#+FILETAGS: :Draft:
#+CATEGORY: permanent\n")
           :unnarrowed t)))

  (setq org-roam-node-display-template "${category:10} ${tags:10} ${title:*}")
  )

(provide 'mw-org)
