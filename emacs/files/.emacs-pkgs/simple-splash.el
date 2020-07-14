(defgroup simple-splash nil "Customize Simple Splash")

(defcustom simple-splash-image nil
  "The image file used in the splash"
  :type '(string))

(defcustom simple-splash-user-name user-full-name
  "The name of user"
  :type '(string))

(defcustom simple-splash-show-version t
  "Whether to display Emacs version"
  :type '(boolean))

(defcustom simple-splash-content nil
  "Custom text to be displayed"
  :type '(string))

(define-derived-mode simple-splash-mode fundamental-mode "Simple Splash"
  "A mode to display a simple splash buffer"
  (setq inhibit-startup-screen t))

(defun simple-splash-invade-current-buffer ()
  (if (and simple-splash-image (display-graphic-p))
      (let* ((img-spec (create-image simple-splash-image))
             (img-width (car (image-size img-spec))))
        (insert (make-string
                 (max 0 (floor (/ (- (window-body-width (get-buffer-window))
                                     img-width)
                                  2)))
                 ?\ ))
        (insert-image img-spec)
        (insert "\n\n")))

  (if simple-splash-user-name
      (insert (concat "Welcome to " simple-splash-user-name "’s GNU Emacs!\n"))
    (insert "Welcome to GNU Emacs!\n"))

  (if simple-splash-show-version
      (insert (concat "GNU Emacs " emacs-version " on "
                      system-configuration "\n")))

  (if simple-splash-content
      (insert (concat "\n" simple-splash-content)))

  (simple-splash-mode)
  (goto-char (point-min))
  (read-only-mode))

(defun simple-splash-invade-buffer (buffer)
  (with-current-buffer buffer (simple-splash-invade-current-buffer)))

(defun simple-splash-create ()
  (interactive)
  (let ((the-buffer (get-buffer-create "*Simple Splash*")))
    (simple-splash-invade-buffer the-buffer)
    (switch-to-buffer the-buffer)))

;;;###autoload
(defun simple-splash-setup-startup-hook ()
  "Setup post initialization hooks."
  (if (< (length command-line-args) 2 )
      (progn
        (add-hook 'after-init-hook (lambda ()
                                     ;; Display useful lists of items
                                     (simple-splash-create))))))

(provide 'simple-splash)
