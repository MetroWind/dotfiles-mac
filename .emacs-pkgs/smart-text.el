(defgroup smart-text nil "Customization for smart-text")
(defcustom smart-text-in-comment t "Whether to enable smart text in comments"
  :type 'boolean)
(defcustom smart-text-in-string nil "Whether to enable smart text in strings"
  :type 'boolean)

(defun insert-pair-and-retreat (str)
  "Inserts `str' and go back one character."
  (insert str)
  (backward-char))

(defun should-be-smart-p ()
  (or (derived-mode-p 'text-mode)
      (let* ((syntax (syntax-ppss))
             (beg (nth 8 syntax)))
        (and beg
             (or (and smart-text-in-comment (nth 4 syntax))
                 (and smart-text-in-string (nth 3 syntax)))))))

(defun smart-text-insert-single-quotes (arg)
  "Inserts a proper pair of single quotes."
  (interactive "P")
  (if (should-be-smart-p)
      ;; If the last char is "\", inserts a literal char.
      (if (search-backward "\\" (- (point) 1) t)
          (progn (forward-char) (insert "'"))
        ;; We need to detect if the quote is for, for example, "I'm",
        ;; "It's", "Bob's", etc, or for quotation.
        (if (re-search-backward "[A-Za-z]" (- (point) 1) t)
            (progn (forward-char) (insert "’"))
          (insert-pair-and-retreat "‘’")))
      (self-insert-command (prefix-numeric-value arg))))

(defun smart-text-insert-double-quotes (arg)
  "Inserts a proper pair of double quotes."
  (interactive "P")
  (if (should-be-smart-p)
      ;; If the last char is "\", inserts a literal char.
      (if (search-backward "\\" (- (point) 1) t)
          (progn (forward-char) (insert "\""))
        (insert-pair-and-retreat "“”"))
    (self-insert-command (prefix-numeric-value arg))))

(defun smart-text-auto-insert-and-convert-dash (arg)
  "Converts two dashes into an en-dash, or converts a en-dash
followed by a dash to an em-dash."
  (interactive "P")
  (if (should-be-smart-p)
      ;; If the last char is "\", inserts a literal char.
      (if (search-backward "\\" (- (point) 1) t)
          (progn (forward-char) (insert "-"))
        (progn
          (insert "-")
          (if (search-backward "--" (- (point) 2) t)
              (replace-match "–"))
          (if (search-backward "–-" (- (point) 2) t)
              (replace-match "—"))))
    (self-insert-command (prefix-numeric-value arg))))

(define-minor-mode smart-text-mode "Type smart quotes and smart dashes."
  :lighter "“"
  :keymap
  '(("'" . smart-text-insert-single-quotes)
    ("\"" . smart-text-insert-double-quotes)
    ("-" . smart-text-auto-insert-and-convert-dash)))

(provide 'smart-text)
