(require 'mw-lib-generic)

;; Mail
(use-package message
  ;; :hook (message-setup . choose-from-by-to)
  :mode ("/mutt-.*-[0-9\\-]+" . message-mode)

  :config
  (setq message-directory "~/mail")
  (setq mail-host-address my-mail-host)
  (setq message-sendmail-envelope-from 'header)

  ;; Signature
  (defun get-random-element (list)
    "Returns a random element of LIST."
    (if (and list (listp list))
        (nth (random (1- (1+ (length list)))) list)
      nil))

  (defun get-string-from-file (filePath)
    "Return filePath's file content."
    (condition-case nil
        (with-temp-buffer
          (insert-file-contents filePath)
          (buffer-string))
      (error nil)))

  (defun choose-sig-by-type (type)
    (cond ((eq type 'file)
           (get-string-from-file my-signature-file))
          ((eq type 'dir)
           (get-string-from-file
            (get-random-element
             (directory-files-recursively my-signature-dir ".*"))))
          ((eq type 'fortune) (fortune))
          ((eq type 'cookie) (cookie nil))))

  (defun choose-sig-by-condition (condition)
    (if (null condition)
        nil
      (let ((this-cond (car condition)))
        (if (string-match
             (cdr (assoc 'pattern this-cond))
             (message-fetch-field (cdr (assoc 'header this-cond))))
            (choose-sig-by-type (cdr (assoc 'sig-type this-cond)))
          (choose-sig-by-condition (cdr condition))))))

  (defun choose-sig ()
    (interactive)
    (choose-sig-by-condition my-sig-condition))

  (defun choose-from-by-to ()
    (interactive)
    (let ((to-field (message-fetch-field "to")))
      (if to-field
          (let ((correct-from
                 (cdr (assoc-regexp to-field my-from-by-to))))
            (if correct-from
                (message-replace-header "From" correct-from))))))

  (add-hook 'message-signature-setup-hook
            (lambda () (interactive)
              (choose-from-by-to)
              (setq message-signature 'choose-sig)
              (message-insert-signature)
              (setq message-signature nil)))
  )

(use-package sendmail
  :commands mail-send
  :config
  (setq mail-specify-envelope-from t)
  (setq mail-envelope-from 'header)
  (setq send-mail-function 'sendmail-send-it)
  (if (not (null-or-unboundp 'my-sendmail))
      (setq sendmail-program my-sendmail))
  (setq mail-default-headers
        (concat "X-Operating-System: "
                ;; This ends with a ‘\n’.
                (shell-command-to-string "uname -s -r -m")
                "X-Useless-Header: Use the Force, Luke!")))

(use-package notmuch
  :commands notmuch
  :hook (message-send . my-fcc-header-setup)
  :bind (:map notmuch-tree-mode-map
              ("g" . notmuch-show-reply)
              ("r" . notmuch-show-reply-sender)
              :map notmuch-show-mode-map
              ("g" . notmuch-show-reply)
              ("r" . notmuch-show-reply-sender)
              :map notmuch-search-mode-map
              ("RET" . mw-notmuch-read-thread))

  :config
  ;; If send from work, manually FCC sent_items, because of weird reasons.
  (setq notmuch-fcc-dirs my-fcc-dirs)
  (defun assoc-regexp (key dict)
    (if (null dict)
        nil
      (if (string-match (car (car dict)) key)
          (car dict)
        (assoc-regexp key (cdr dict)))))

  (defun my-fcc-header-setup ()
    (let ((subdir (cdr (assoc-regexp (message-fetch-field "from") notmuch-fcc-dirs))))
      (if subdir
          (message-add-header (concat "Fcc: " subdir)))))
  (setq message-fcc-handler-function
        '(lambda (destdir)
           (notmuch-maildir-fcc-write-buffer-to-maildir destdir t)))
  (setq notmuch-hello-thousands-separator "")
  (if (boundp 'my-notmuch-searchs)
      (if my-notmuch-searchs
          (setq notmuch-saved-searches my-notmuch-searchs)))

  (defun mw-notmuch-read-thread ()
    "Used in a notmuch-search buffer to read the current-selected thread.

  By default when you press RET in a notmuch-search
  buffer (`notmuch-search-show-thread'), it shows the whole
  thread as a big email, in the same buffer. This is very slow.
  `mw-notmuch-read-thread' shows a thread tree and a single
  email, in two buffers, which is like Mutt's behavior. This is
  much faster."

    (interactive)
    (notmuch-tree (notmuch-search-find-thread-id)
                  (notmuch-search-get-query)
                  nil nil t))
  )

(provide 'mw-mail)
