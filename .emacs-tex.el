;; AucTEX
;; (require 'tex)
(load "auctex.el" nil t t)
;; (load "~/.emacs.d/auctex/preview/preview-latex.el" nil t t)
(setq TeX-parse-self t)                 ; Enable parse on load.
(setq TeX-auto-save t)                  ; Enable parse on save.
(setq-default TeX-master nil)
; make PDF by default (can toggle with C-c C-t C-p
(add-hook 'TeX-mode-hook (lambda () (TeX-PDF-mode 1)))
;; Math mode symbols
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

(require 'latex)			; defines LaTeX-math-mode
(require 'context)
(add-hook 'TeX-mode-hook 'LaTeX-math-mode)
(setq TeX-electric-sub-and-superscript t)
;; What mode to use for new TeX file
(setq TeX-default-mode 'ConTeXt-mode)
; Emacs help for \label, \ref, \cite.  Normally used only with
; LaTeX-mode but also useful with plain TeX + eplain and with ConTeXt, so:
(setq reftex-plug-into-AUCTeX t)
(add-hook 'TeX-mode-hook 'reftex-mode)

(require 'texmathp)
(add-to-list 'texmathp-tex-commands '("\\startformula" sw-on))
(add-to-list 'texmathp-tex-commands '("\\stopformula" sw-off))
(texmathp-compile)

(defun insert-balanced (left right)
  "Insert a left, right delmiter pair and be poised to type inside them."
  (interactive)
  (insert left)
  (save-excursion
    (insert right)))

(defun start-context-math ()
  (interactive)
  (let* ((start (max (point-min) (- (point) 1)))
	 (stop  (min (point-max) (+ (point) 1))))
    ; if in the middle of a $$, turn inline math into context display math
    (if (equal "$$" (buffer-substring-no-properties start stop))
	(progn
	  (delete-region start stop)	;get rid of the $$
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

(add-hook 'TeX-mode-hook
	  '(lambda ()
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
			       (insert-balanced ?\( ?\))))))

(add-hook 'ConTeXt-mode-hook
	  '(lambda ()
	     (local-set-key "$" 'start-context-math)))

(setq TeX-XeTeX-mode nil)       ; Don't know why need this for
                                ; `ConTeXt' compilation to work...
;; (TeX-add-style-hook
;;  "beamer"
;;  (lambda () (setq TeX-PDF-mode t)))
(setq TeX-output-view-style
      `(("^dvi$"
         ("^landscape$" "^pstricks$\\|^pst-\\|^psfrag$")
         "%(o?)dvips -t landscape %d -o && gv %f")
        ("^dvi$" "^pstricks$\\|^pst-\\|^psfrag$" "%(o?)dvips %d -o && gv %f")
        ("^dvi$"
         ("^a4\\(?:dutch\\|paper\\|wide\\)\\|sem-a4$" "^landscape$")
         "%(o?)xdvi %dS -paper a4r -s 0 %d")
        ("^dvi$" "^a4\\(?:dutch\\|paper\\|wide\\)\\|sem-a4$" "%(o?)xdvi %dS -paper a4 %d")
        ("^dvi$"
         ("^a5\\(?:comb\\|paper\\)$" "^landscape$")
         "%(o?)xdvi %dS -paper a5r -s 0 %d")
        ("^dvi$" "^a5\\(?:comb\\|paper\\)$" "%(o?)xdvi %dS -paper a5 %d")
        ("^dvi$" "^b5paper$" "%(o?)xdvi %dS -paper b5 %d")
        ("^dvi$" "^letterpaper$" "%(o?)xdvi %dS -paper us %d")
        ("^dvi$" "^legalpaper$" "%(o?)xdvi %dS -paper legal %d")
        ("^dvi$" "^executivepaper$" "%(o?)xdvi %dS -paper 7.25x10.5in %d")
        ("^dvi$" "." "%(o?)xdvi %dS %d")
        ("^pdf$" "."
         ,(cond (linuxp "dpv %o")
                (macp "open %o")))
        ("^html?$" "." "firefox %o")))

(add-hook 'LaTeX-mode-hook 
          (lambda()
            (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t))
            (setq TeX-command-default "XeLaTeX")))
(add-hook 'ConTeXt-mode-hook 
          (lambda()
            (add-to-list 'TeX-command-list 
                         '("xConTeXt" "texexec --batch --xtx %t" 
                           TeX-run-TeX nil t))
            (add-to-list 'LaTeX-verbatim-macros-with-delims "type")
            (add-to-list 'LaTeX-verbatim-macros-with-braces "type")
            (add-to-list 'LaTeX-verbatim-environments "typing")
))
(add-hook 'ConTeXt-mode-hook 
          (lambda()
            (add-to-list 'TeX-command-list 
                         '("MkIV"
                           "~/bin/context-minimal-exec.sh context --nonstopmode --purgeall %t"
                           TeX-run-TeX nil t))))

;; (defun set-tex-faces ()
;;   (set-face-attribute
;;    'font-latex-superscript-face nil
;;    :height 1.0)
;;   (set-face-attribute
;;    'font-latex-subscript-face nil
;;    :height 1.0)
;; )

;; (add-hook 'LaTeX-mode-hook
;;           (lambda ()
;;             ;; Some faces
;;             (set-face-attribute
;;              'font-latex-verbatim-face nil
;;              :inherit 'fixed-pitch 
;;              :foreground "burlywood" 
;;              :family "Courier New")
;; ))

;; (add-hook 'ConTeXt-mode-hook
;;           (lambda ()
;;             (set-face-attribute
;;              'font-latex-sedate-face nil
;;              :foreground "#66ccff")
;;             (set-tex-faces)
;; ))

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

(add-hook 'LaTeX-mode-hook
          (lambda ()
            (message "LaTeX-mode loaded.")
            (deal-with-latex-quote)))
(add-hook 'ConTeXt-mode-hook
          (lambda ()
            (message "ConTeXt-mode loaded.")
            (deal-with-context-quote)))

(add-hook 'tex-mode-hook
          '(lambda ()
             (message "tex-mode loaded.")
             (define-key LaTeX-mode-map (kbd "C-c /") 'LaTeX-close-environment)
             (define-key LaTeX-mode-map (kbd "C-TAB") 'indent-according-to-mode)))

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

(add-hook 'ConTeXt-mode-hook
          (lambda () (define-key ConTeXt-mode-map (kbd "TAB") 'cdlatex-tab)))

;; Pretty display for some symbols
(defun gen-tex-macro-regex (macro)
  (concat "\\(\\\\" macro "\\)[^a-zA-Z]+"))

(defun TeX-unicode ()
  (interactive)
  (substitute-patterns-with-unicode
   `((,(gen-tex-macro-regex "to") . right-arrow)
     (,(gen-tex-macro-regex "emptyset") . nil)
     (,(gen-tex-macro-regex "Rightarrow") . Right-arrow)
     (,(gen-tex-macro-regex "int") . integral)
     (,(gen-tex-macro-regex "oint") . oint)
     (,(gen-tex-macro-regex "sum") . sum)
     (,(gen-tex-macro-regex "prod") . product)
     (,(gen-tex-macro-regex "infty") . infinity)
     (,(gen-tex-macro-regex "neq") . not-equal)
     (,(gen-tex-macro-regex "approx") . approximately)
     (,(gen-tex-macro-regex "leq") . less-than-or-equal-to)
     (,(gen-tex-macro-regex "geq") . greater-than-or-equal-to)
     (,(gen-tex-macro-regex "forall") . for-all)
     (,(gen-tex-macro-regex "exists") . there-exists)
     (,(gen-tex-macro-regex "in") . element-of)
     (,(gen-tex-macro-regex "dagger") . dagger)
     (,(gen-tex-macro-regex "cdot") . cdot)
     (,(gen-tex-macro-regex "ll") . much-less-than)
     (,(gen-tex-macro-regex "gg") . much-greater-than)
     (,(gen-tex-macro-regex "langle") . angle-left)
     (,(gen-tex-macro-regex "rangle") . angle-right)
     (,(gen-tex-macro-regex "<") . angle-left)
     (,(gen-tex-macro-regex ">") . angle-right)
     ;,; Greek letters
     (,(gen-tex-macro-regex "lambda") . lambda)
     (,(gen-tex-macro-regex "alpha") . alpha)
     (,(gen-tex-macro-regex "beta") . beta)
     (,(gen-tex-macro-regex "gamma") . gamma)
     (,(gen-tex-macro-regex "delta") . delta)
     (,(gen-tex-macro-regex "epsilon") . epsilon)
     (,(gen-tex-macro-regex "zeta") . zeta)
     (,(gen-tex-macro-regex "eta") . eta)
     (,(gen-tex-macro-regex "theta") . theta)
     (,(gen-tex-macro-regex "kappa") . kappa)
     (,(gen-tex-macro-regex "mu") . mu)
     (,(gen-tex-macro-regex "nu") . nu)
     (,(gen-tex-macro-regex "xi") . xi)
     (,(gen-tex-macro-regex "pi") . pi)
     (,(gen-tex-macro-regex "rho") . rho)
     (,(gen-tex-macro-regex "sigma") . sigma)
     (,(gen-tex-macro-regex "tau") . tau)
     (,(gen-tex-macro-regex "phi") . phi)
     (,(gen-tex-macro-regex "chi") . chi)
     (,(gen-tex-macro-regex "psi") . psi)
     (,(gen-tex-macro-regex "omega") . omega))))

(add-hook 'LaTeX-mode-hook 'TeX-unicode)
(add-hook 'ConTeXt-mode-hook 'TeX-unicode)
