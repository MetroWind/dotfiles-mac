(use-package tex
  :if (package-installed-p 'auctex)
  :mode ("\\.tex\\'" . TeX-mode)
  ;; :ensure auctex
  :config
  ;; The tab functionality copied (and modified a bit) from CDLaTeX.
  (defun cdlatex-tab ()
    "This function is intended to do many cursor movements.
It is bound to the tab key since tab does nothing useful in a TeX file.
The function first tries to expand any command keyword before point.
If there is none, it cleans up short subscripts and superscripts at point.
I.e. it changes a^{2} into a^2, since this is more readable.  This feature
can be disabled by setting `cdlatex-simplify-sub-super-scripts' to nil.
Then it jumps to the next point in a LaTeX text where one would reasonably
expect that more input can be put in.
To do that, the cursor is moved according to the following rules:
The cursor stops...
- before closing brackets if preceding-char is any of -({[]})
- after  closing brackets, but not if following-char is any of ({[_^
- just after $, if the cursor was before that $.
- at end of non-empty lines
- at the beginning of empty lines
- before a SPACE at beginning of line
- after first of several SPACE
Sounds strange? Try it out.
"
    (interactive)
    (catch 'stop

      ;; try command expansion
      (let ((pos (point)) exp math-mode)
        (backward-word 1)
        (while (eq (following-char) ?$) (forward-char 1))
        (goto-char pos))

      ;; Check for simplification of sub and superscripts
      (cond
       ((looking-at "}\\|\\]\\|)")
        (forward-char -3)
        (if (looking-at "[_^]{[0-9a-zA-Z]}")
            ;; sub/super script
            (progn (forward-char 1)
                   (delete-char 1)
                   (forward-char 1)
                   (delete-char 1))
          (forward-char 4))
        (if (looking-at "[^_\\^({\\[]")
            ;; stop after closing bracket, unless ^_[{( follow
            (throw 'stop t)))
       ((= (following-char) ?$)
        (while (= (following-char) ?$) (forward-char 1))
        (throw 'stop t))
       ((= (following-char) ?\ )
        ;; stop after first of many spaces
        (forward-char 1)
        (re-search-forward "[^ ]")
        (if (/= (preceding-char) ?\n) (forward-char -1)))
       (t
        (forward-char 1)))

      ;; move to next possible stopping site and check out the place
      (while (re-search-forward "[ )}\n]\\|\\]" (point-max) t)
        (forward-char -1)
        (cond
         ((= (following-char) ?\ )
          ;; stop at first space or b-o-l
          (if (not (bolp)) (forward-char 1)) (throw 'stop t))
         ((= (following-char) ?\n)
          ;; stop at line end, but not after \\
          (if (and (bolp) (not (eobp)))
              (throw 'stop t)
            (if (equal "\\\\" (buffer-substring-no-properties
                               (- (point) 2) (point)))
                (forward-char 1)
              (throw 'stop t))))
         (t
          ;; Stop before )}] if preceding-char is any parenthesis
          (if (or (= (char-syntax (preceding-char)) ?\()
                  (= (char-syntax (preceding-char)) ?\))
                  (= (preceding-char) ?-))
              (throw 'stop t)
            (forward-char 1)
            (if (looking-at "[^_\\^({\\[]")
                ;; stop after closing bracket, unless ^_[{( follow
                (throw 'stop t))))))))

  (defun insert-balanced (left right)
    "Insert a left, right delmiter pair and be poised to type inside them."
    (interactive)
    (insert left)
    (save-excursion
      (insert right)))

  (defun insert-context-single-quotes ()
    "Inserts a proper pair of single quotes in ConTeXt."
    (interactive)
    ;; If the last char is "\", inserts a literal char.
    (if (search-backward "\\" (- (point) 1) t)
        (progn (forward-char) (insert "'"))
      ;; We need to detect if the quote is for, for example, "I'm",
      ;; "It's", "Bob's", etc, or for quotation.
      (if (re-search-backward "[A-Za-z]" (- (point) 1) t)
          (progn (forward-char) (insert "’"))
        (insert-pair-and-retreat "\\quote{}"))))

  (defun insert-context-double-quotes ()
    "Inserts a proper pair of double quotes in ConTeXt"
    (interactive)
    ;; If the last char is "\", inserts a literal char.
    (if (search-backward "\\" (- (point) 1) t)
        (progn (forward-char) (insert "\""))
      (insert-pair-and-retreat "\\quotation{}")))

  (defun deal-with-latex-quote ()
    "If in math mode, insert a literal ', \", or -.  If in normal
  mode, insert a proper pair of quotes, or dash."
    (define-key LaTeX-mode-map "'"
      (lambda ()
        (interactive)
        (if (texmathp)
            (insert "'")
          (insert-single-quotes))))

    (define-key LaTeX-mode-map "\""
      (lambda ()
        (interactive)
        (if (texmathp)
            (insert "\"")
          (insert-double-quotes))))

    (define-key LaTeX-mode-map "-"
      (lambda ()
        (interactive)
        (if (texmathp)
            (insert "-")
          (auto-insert-and-convert-dash)))))

  (defun deal-with-context-quote ()
    "If in math mode, insert a literal ', \", or -.  If in normal
  mode, insert a proper pair of quotes, or dash."
    (define-key ConTeXt-mode-map "'"
      (lambda ()
        (interactive)
        (if (texmathp)
            (insert "'")
          (insert-context-single-quotes))))

    (define-key ConTeXt-mode-map "\""
      (lambda ()
        (interactive)
        (if (texmathp)
            (insert "\"")
          (insert-context-double-quotes))))

    (define-key ConTeXt-mode-map "-"
      (lambda ()
        (interactive)
        (if (texmathp)
            (insert "-")
          (auto-insert-and-convert-dash)))))

  (defun start-context-math ()
    (interactive)
    (let* ((start (max (point-min) (- (point) 1)))
           (stop  (min (point-max) (+ (point) 1))))
                                        ; if in the middle of a $$, turn inline math into context display math
      (if (equal "$$" (buffer-substring-no-properties start stop))
          (progn
            (delete-region start stop)  ;get rid of the $$
                                        ; delete preceding spaces, if any
            (while (and (< (point-min) (point))
                        (equal (buffer-substring-no-properties (- (point) 1)
                                                               (point))
                               " "))
              (backward-delete-char 1))
                                        ; delete a preceding newline, if any
            (if (equal (buffer-substring-no-properties (- (point) 1)
                                                       (point))
                       "\n")
                (backward-delete-char 1))
                                        ; ConTeXt's display math with automatic equation numbering
            (insert "\n\\startformula\n")
            (save-excursion (insert "\n\\stopformula")))
                                        ; else: just doing inline math
        (insert-balanced ?\$ ?\$))))

  (add-hook
   'TeX-mode-hook
   (lambda ()
     (LaTeX-math-mode)
     (setq TeX-electric-sub-and-superscript t)

     (local-set-key "$"
                    '(lambda ()
                       (interactive)
                       (insert-balanced ?\$ ?\$)))
     (local-set-key "{"
                    '(lambda ()
                       (interactive)
                       (insert-balanced ?\{ ?\})))
     (local-set-key "["
                    '(lambda ()
                       (interactive)
                       (insert-balanced ?\[ ?\])))
     (local-set-key "("
                    '(lambda ()
                       (interactive)
                       (insert-balanced ?\( ?\))))

     (define-key TeX-mode-map (kbd "M-q") 'LaTeX-fill-paragraph)
     (define-key LaTeX-mode-map (kbd "C-c /") 'LaTeX-close-environment)
     (define-key LaTeX-mode-map (kbd "C-TAB") 'indent-according-to-mode)))


  (add-hook
   'ConTeXt-mode-hook
   (lambda ()
     (define-key ConTeXt-mode-map (kbd "TAB") 'cdlatex-tab)
     ;; Don't prefer breaking line at the beginning and end of inline
     ;; math.
     (setq LaTeX-fill-break-at-separators '(\\\[ \\\]))

     (setq LaTeX-math-list
           ;; Symbols
           '((?\] "Rightarrow")
             (?\[ "Leftarrow")
             (?} "to")
             (?{ "gets")
             (?= "approx")
             ;; Greek
             (?q "theta")
             (?Q "Theta")
             (?c "chi")
             (?o "omega")
             (?O "Omega")
             ;; Complicated stuff
             (?/ (lambda () (interactive) (insert "\\Slash{}") (backward-char)))))
     ;; This will properly append `LaTeX-math-list' to
     ;; `LaTeX-math-default'.
     (LaTeX-math-initialize)

     (require 'texmathp)
     (add-to-list 'texmathp-tex-commands '("\\startformula" sw-on))
     (add-to-list 'texmathp-tex-commands '("\\stopformula" sw-off))
     (texmathp-compile)

     (add-to-list 'TeX-command-list
                  '("MkIV"
                    "~/bin/context-minimal-exec.sh context --nonstopmode --purgeall %t"
                    TeX-run-TeX nil t))
     (add-to-list 'TeX-command-list
                  '("XeLaTeX" "xelatex -halt-on-error -interaction=nonstopmode %t" TeX-run-TeX nil
                    (latex-mode doctex-mode)
                    :help "Run LaTeX"))

     (deal-with-context-quote)))

  (add-hook
   'LaTeX-mode-hook
   (lambda ()
     (message "LaTeX-mode loaded.")
     (define-key LaTeX-mode-map (kbd "TAB") 'cdlatex-tab)
     ;; Don't prefer breaking line at the beginning and end of inline
     ;; math.
     (setq LaTeX-fill-break-at-separators '(\\\[ \\\]))
     (deal-with-latex-quote)

     (setq LaTeX-math-list
           ;; Symbols
           '((?\] "Rightarrow")
             (?\[ "Leftarrow")
             (?} "to")
             (?{ "gets")
             (?= "approx")
             ;; Greek
             (?q "theta")
             (?Q "Theta")
             (?c "chi")
             (?o "omega")
             (?O "Omega")
             ;; Complicated stuff
             (?/ (lambda () (interactive) (insert "\\Slash{}") (backward-char)))))
     ;; This will properly append `LaTeX-math-list' to
     ;; `LaTeX-math-default'.
     (LaTeX-math-initialize)
     ))
  )

(provide 'mw-tex)
