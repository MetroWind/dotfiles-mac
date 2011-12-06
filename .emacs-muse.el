(add-to-list 'load-path "~/.emacs.d/muse")

(require 'muse-mode)
(require 'muse-publish)
(require 'muse-html)
(require 'muse-journal)

(cond
 (linuxp
  (muse-derive-style "wiki-xhtml" "xhtml"
                     :header "/mnt/shared/text/wiki/.wiki/header.html"
                     :footer "/mnt/shared/text/wiki/.wiki/footer.html"
                     ))
 (aquap
  (muse-derive-style "wiki-xhtml" "xhtml"
                     :header "/Volumes/Shared/text/wiki/.wiki/header.html"
                     :footer "/Volumes/Shared/text/wiki/.wiki/footer.html"
                     ))
)

(cond
 (linuxp
  (setq muse-project-alist
        '(("muse-test"
           ("/mnt/shared/text/muse-test" :default "index")
           (:base "wiki-xhtml" :path "/mnt/shared/text/muse-test/public_html"))
          ("Wiki"
           ("/mnt/shared/text/wiki" :default "index")
           (:base "wiki-xhtml"
                  :path "/mnt/shared/text/wiki/publish"
                  :force-publish ("WikiIndex"))))))
 (aquap
  (setq muse-project-alist
        '(("muse-test"
           ("/Volumes/Shared/text/muse-test" :default "index")
           (:base "wiki-xhtml" :path "/Volumes/Shared/text/muse-test/public_html"))
          ("Wiki"
           ("/Volumes/Shared/text/wiki" :default "index")
           (:base "wiki-xhtml"
                  :path "/Volumes/Shared/text/wiki/publish"
                  :force-publish ("WikiIndex"))))))
)

(setq muse-colors-inline-image-method 
      'muse-colors-use-publishing-directory)
(setq muse-html-style-sheet "styles/deviant/style.css")
;; (setq muse-html-header "~/.emacs.d/muse/header.wiki")
;; (setq muse-html-footer "~/.emacs.d/muse/footer.wiki")
;; (setq muse-xhtml-header "~/.emacs.d/muse/header.wiki")
;; (setq muse-xhtml-footer "~/.emacs.d/muse/footer.wiki")
(setq muse-html-meta-http-equiv "Content-Type")
(setq muse-html-meta-content-type "application/xhtml+xml")
(setq muse-html-meta-content-encoding 'detect)
(setq muse-html-charset-default "utf-8")
(setq muse-html-encoding-default "utf-8")
(setq muse-journal-date-format "%Y年%B%e日, %A")

;; (setq muse-before-publish-hook
;;       'color-theme-blippblopp)
;; (setq muse-after-publish-hook
;;       (lambda () 
;;         (color-theme-deep-blue)
;;         (set-face-attribute
;;          'mode-line nil
;;          :background "#dedede" :foreground "#000000"
;;          :height 80
;;          :box nil)))
      
;; Faces
(add-hook 'muse-mode-hook
          (lambda ()
            (set-face-attribute
             'muse-header-1 nil
             :weight 'bold
             :height 1.4
             :family (cond (linuxp "URW Palladio L") (aquap "Palatino")))))
