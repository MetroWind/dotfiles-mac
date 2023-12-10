(require 'cl)

(defun null-or-unboundp (symbol)
  (if (boundp symbol)
      (null (symbol-value symbol))
    t))

(defun bound-and-true (symbol)
  (not (null-or-unboundp symbol)))

(defun with-default (symbol default)
  (if (boundp symbol)
      (symbol-value symbol)
    default))

(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
    ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
    (t (self-insert-command (or arg 1)))))

;; Unfill buffer
(defun unfill-buffer ()
  "Unfill current buffer."
  (interactive "")
  (setq m (point-marker))
  (beginning-of-buffer)
  (while (re-search-forward "\\([^ ]+\\) *
 *\\([^ ]\\)" nil t)
    (replace-match "\\1 \\2"))
  (set-marker m 0 (current-buffer)))

;; Word count
(defun word-count nil "Count words in buffer" (interactive)
  (shell-command-on-region (point-min) (point-max) "wc -w"))

;; Turn off my monitor
;; (defun turn-off-monitor ()
;;   (interactive)
;;   (shell-command "sleep 1; xset dpms force off"))

;; Find chars that not belong to the charset, copied from
;; http://ann77.stu.cdut.edu.cn/EmacsChineseCalendar.html
(defun find-invalid-char ()
  (interactive)
  (let (c m)
    (save-excursion
      (widen)
      (condition-case nil
          (progn
            (setq c (following-char))
            (while c
              (if (and (>= c 128)
                       (<= c 256))
                  (error ""))
              (if ( >= (point) (point-max))
                  (error ""))
              (goto-char (1+ (point)))
              (setq c (following-char))))
        (error (setq m (point)))))
    (goto-char m)))

;; From http://www.zeuux.org/science/learning-emacs.cn.html
(defun copy-line (&optional arg)
  "Save current line into Kill-Ring without mark the line"
  (interactive "P")
  (let ((beg (line-beginning-position))
        (end (line-end-position arg)))
    (copy-region-as-kill beg end)))

(defun copy-line-or-region (&optional n)
  "Save current line or region into Kill-Ring.  If the mark is
deactivated in current buffer, Save current line; otherwise save
the region."
  (interactive "p")
  (if mark-active
      (kill-ring-save
       (region-beginning) (region-end))
    (copy-line n)))

;; Insert current date
(defun insert-date ()
  "Insert the current date according to the variable
  \"insert-date-format\"."
  (interactive "*")
  (insert (format-time-string "%Y-%m-%d")))

(defun toggle-line-wrap ()
  "Toggle `visual-line-mode'."
  (interactive)
  (if auto-fill-function
      ((lambda ()
         (turn-off-auto-fill)
         (visual-line-mode 1)))
    ((lambda ()
       (turn-on-auto-fill)
       (visual-line-mode -1)))))

(defun cua-or-multicursor ()
  (interactive)
  (if (use-region-p)
      (mc/edit-lines)
    (cua-rectangle-mark-mode)))

(defun sum-cua-rectangle ()
  ;; Treat the content of current cua rectangle as numbers, and
  ;; calculate sum.
  (interactive)
  (message
   (number-to-string
    (reduce (lambda (x y) (+ x y))
            (mapcar (lambda (n) (string-to-number n))
                    (cua--extract-rectangle))))))

(defun comment-sectional ()
  "Start a sectional comment if at empty line, otherwise finish
the sectional comment."
  (interactive)
  (defun chomp (str)
    "Chomp leading and tailing whitespace from STR."
    (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'" str)
      (setq str (replace-match "" t t str))) str)
  (let* ((line (buffer-substring-no-properties
                (line-beginning-position) (line-end-position)))
         (len-line (length line))
         (len-line-bare (length (chomp line))))
    (if (= len-line-bare 0)
        (progn
          (comment-dwim nil)
          (insert "========== "))
      (progn
        (insert " ")
        (insert (make-string (- fill-column len-line 2) ?=))
        (insert ">")))))

;; For log viewing conveniences.
(define-generic-mode 'python-log-mode
  '()                                   ; comments
  '("INFO" "WARN" "CRIT" "ERRO")          ; Keywords
  '(("[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\},[0-9]+" . 'font-lock-constant-face)
    ("ERROR?" . 'error)
    ("FATAL" . 'error)
    ("[nN]ot expected" . 'error)
    ("[uU]nexpected" . 'error))
  '()
  nil
  "A mode for python log")

(defun yank-with-indent ()
  "Yank and indent the region to the current position."
  (interactive)
  (let ((indent
         (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
    (message indent)
    (yank)
    (narrow-to-region (mark t) (point))
    (pop-to-mark-command)
    (replace-string "\n" (concat "\n" indent))
    (widen)))

;; Timestamp in messages
(defun current-time-microseconds ()
  "Return the current time formatted to include microseconds."
  (let* ((nowtime (current-time))
         (now-ms (nth 2 nowtime)))
    (concat (format-time-string "[%Y-%m-%d %T" nowtime) (format ".%d]" now-ms))))

(defun current-time-no-microseconds ()
  "Return the current time formatted to include microseconds."
  (let* ((nowtime (current-time))
         (now-ms (nth 2 nowtime)))
    (format-time-string "[%Y-%m-%d %T]" nowtime)))

(defun ad-timestamp-message (FORMAT-STRING &rest args)
  "Advice to run before `message' that prepends a timestamp to each message.

Activate this advice with:
(advice-add 'message :before 'sh/ad-timestamp-message)"
  (unless (string-equal FORMAT-STRING "%s%s")
    (let ((deactivate-mark nil)
          (inhibit-read-only t))
      (with-current-buffer "*Messages*"
        (goto-char (point-max))
        (if (not (bolp))
          (newline))
        (insert (current-time-no-microseconds) " ")))))

(defun close-and-delete-file ()
  "Close the current buffer and delete the associated file."
  (interactive)
  (let ((filename (buffer-file-name)))
    (save-buffer)
    (kill-this-buffer)
    (if (not (null filename))
        (delete-file filename))))

(defun sluggify (s)
  "Convert a string to one that is suitable as an “identifier”,
 by down-casing the string, and converting all non-alphanumeric
 characters to underscore. Consecutive underscores are merged."
  (replace-regexp-in-string
   (rx (>= 2 "_")) "_"
   (replace-regexp-in-string
    (rx (not alnum)) "_" (downcase s))))

(defun -apply-window-configuration (conf window)
  (defun -split-to-windows (conf window)
    (if (null conf)
        nil
      (let* ((this-split (car conf))
            (new-window
             (cond ((eq (car this-split) 'column)
                    (split-window-right nil window))
                   ((eq (car this-split) 'row)
                    (split-window-below nil window))
                   (t (error (format "Invalid window split direction %s"
                                     this-split))))))
        (cons new-window (-split-to-windows (cdr conf) new-window)))))

  (defun split-to-windows (conf window)
    (cons window (-split-to-windows (cdr conf) window)))

  (defun apply-configuration-in-sub-windows (conf sub-windows)
    (if (null conf)
        nil
      (-apply-window-configuration (cdr (car conf)) (car sub-windows))
      (apply-configuration-in-sub-windows (cdr conf) (cdr sub-windows))))

  (if (null conf)
      nil
    (apply-configuration-in-sub-windows
     conf
     (split-to-windows conf window))))

(defun apply-window-configuration (conf)
  "Apply a configuration to the current frame. A configuration is a
list of columns or rows with their own configuration. For
example:

- ((row) (row))

  Splits the current frame into two rows.

- ((column) (column) (column))

  Splits the current frame into three columns.

- ((column) (column . ((row) (row))))

  Splits the current frame into this configuration:

  ┌─────┬─────┐
  │     │     │
  │     ├─────┤
  │     │     │
  └─────┴─────┘

- ((row)
   (row . ((column)
           (column . ((row) (row)))
           (column)))
   (row))

  Splits the current frame into this configuration:

  ┌────────────┐
  │            │
  ├───┬────┬───┤
  │   │    │   │
  │   ├────┤   │
  │   │    │   │
  ├───┴────┴───┤
  │            │
  └────────────┘
"
  (delete-other-windows)
  (-apply-window-configuration conf (selected-window))
  (balance-windows))

(provide 'mw-lib-generic)
