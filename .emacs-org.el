(require 'org-install)

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(add-hook 'org-mode-hook 'turn-on-font-lock)

(setq org-log-done t)
(setq org-agenda-include-diary t)
(setq org-icalendar-include-todo t)
(setq org-startup-folded nil)           ;Show all content on startup.

(cond
 (linuxp
  (setq org-agenda-files '("/mnt/shared/text/misc/org/plan.org"
                           "/mnt/shared/text/misc-study/abroad/apply.org")))
 (aquap
  (setq org-agenda-files '("/Volumes/Shared/text/misc/org/plan.org"
                           "/Volumes/Shared/text/misc-study/abroad/apply.org")))
)

;; Set to the location of your Org files on your local system
(setq org-directory "/mnt/shared/text/misc/org")
;; Set to the name of the file where new notes will be stored
(setq org-mobile-inbox-for-pull (concat org-directory "/plan.org"))
;; Set to <your Dropbox root directory>/MobileOrg.
(setq org-mobile-directory "~/Dropbox/MobileOrg")

(add-hook 'org-mode-hook
          (lambda () (define-key org-mode-map (kbd "C-c C-a") 'show-all)))

(setq org-hide-leading-stars t)
(if (window-system)
    (add-hook 'org-mode-hook
              (lambda ()
                (set-face-attribute
                 'org-hide nil
                 :foreground "#2e3735"))))

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

.todo, .done, .timestamp
{
    font-family: monospace;
    font-size: 80%;
    background-color: #ddd;
}

pre, code
{
    background-color: #303030;
    color: #b6beb4;
    border: none; 
    font-family: monospace;
    font-size: 85%;
}

</style>")
