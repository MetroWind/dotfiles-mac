(require 'cl)

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

;; Insert a proper pair of quotes
(defun insert-pair-and-retreat (str)
  "Inserts `str' and go back one character."
  (insert str)
  (backward-char))

(defun insert-single-quotes ()
  "Inserts a proper pair of single quotes."
  (interactive)
  ;; If the last char is "\", inserts a literal char.
  (if (search-backward "\\" (- (point) 1) t)
      (progn (forward-char) (insert "'"))
    ;; We need to detect if the quote is for, for example, "I'm",
    ;; "It's", "Bob's", etc, or for quotation.
    (if (re-search-backward "[A-Za-z]" (- (point) 1) t)
        (progn (forward-char) (insert "’"))
      (insert-pair-and-retreat "‘’"))))

(defun insert-double-quotes ()
  "Inserts a proper pair of double quotes."
  (interactive)
  ;; If the last char is "\", inserts a literal char.
  (if (search-backward "\\" (- (point) 1) t)
      (progn (forward-char) (insert "\""))
    (insert-pair-and-retreat "“”")))

(defun auto-insert-and-convert-dash ()
  "Converts two dashes into an en-dash, or converts a en-dash
followed by a dash to an em-dash."
  (interactive)
  ;; If the last char is "\", inserts a literal char.
  (if (search-backward "\\" (- (point) 1) t)
      (progn (forward-char) (insert "-"))
    (progn
      (insert "-")
      (if (search-backward "--" (- (point) 2) t)
          (replace-match "–"))
      (if (search-backward "–-" (- (point) 2) t)
          (replace-match "—")))))

(defun enable-smart-quote-in-map (mode-map)
  (define-key mode-map "-" 'auto-insert-and-convert-dash)
  (define-key mode-map "'" 'insert-single-quotes)
  (define-key mode-map (kbd "C-'") (lambda () (interactive) (insert ?\")))
  (define-key mode-map "\"" 'insert-double-quotes))

(use-package markdown-mode
  :bind (:map markdown-mode-map
         ("'" . insert-single-quotes)
         ("\"" . insert-double-quotes)))

(use-package nxml-mode
  :bind (:map nxml-mode-map
              ("\"" . nil)
              ("-" . nil)))

(use-package text-mode
  :config (enable-smart-quote-in-map text-mode-map))

(use-package scribble-mode
  :config (enable-smart-quote-in-map scribble-mode-map))

(use-package twittering-mode
  :config (enable-smart-quote-in-map twittering-edit-mode-map))

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

(provide 'mw-lib-generic)