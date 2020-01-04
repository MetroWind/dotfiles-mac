(defvar have-prettify-symbols
  (not (version< emacs-version "24.4")))

;; Replace stuff like `lambda', `->' with actual unicode chars.
(defun unicode-symbol (name)
  "Translate a symbolic name for a Unicode character -- e.g., LEFT-ARROW
  or GREATER-THAN into an actual Unicode character code. "
  (decode-char 'ucs (case name
                      ;; arrows
                      ('left-arrow 8592)
                      ('up-arrow 8593)
                      ('right-arrow 8594)
                      ('down-arrow 8595)
                      ('Right-arrow #x21d2)
                      ;; Math symbols
                      ('integral #x222b)
                      ('oint #x222e)
                      ('sum #x2211)
                      ('product #x220f)
                      ('infinity #x221e)
                      ('equal #X003d)
                      ('not-equal #X2260)
                      ('approximately #x2248)
                      ('identical #X2261)
                      ('not-identical #X2262)
                      ('less-than #X003c)
                      ('greater-than #X003e)
                      ('less-than-or-equal-to #X2264)
                      ('greater-than-or-equal-to #X2265)
                      ('much-less-than #x226a)
                      ('much-greater-than #x226b)
                      ('logical-and #X2227)
                      ('logical-or #X2228)
                      ('logical-neg #X00AC)
                      ('nil #X2205)
                      ('for-all #X2200)
                      ('there-exists #X2203)
                      ('element-of #X2208)
                      ('not-element-of #X2209)
                      ('cdot #x22c5)
                      ('two-ldots #x2025)
                      ('ldots #x2026)
                      ;; boxes
                      ('double-vertical-bar #X2551)
                      ;; relational operators
                      ;; logical operators
                      ;; misc
                      ('horizontal-ellipsis #X2026)
                      ('double-exclamation #X203C)
                      ('prime #X2032)
                      ('double-prime #X2033)
                      ('dagger #x2020)
                      ;; mathematical operators
                      ('square-root #X221A)
                      ('squared #X00B2)
                      ('cubed #X00B3)
                      ('colon-colon #x2237)
                      ;; letters
                      ('lambda #X03BB)
                      ('alpha #X03B1)
                      ('beta #X03B2)
                      ('gamma #X03B3)
                      ('delta #X03B4)
                      ('epsilon #X03B5)
                      ('zeta #X03B6)
                      ('eta #X03B7)
                      ('theta #X03B8)
                      ('iota #X03B9)
                      ('kappa #X03BA)
                      ('mu #X03BC)
                      ('nu #X03BD)
                      ('xi #X03BE)
                      ('pi #X03C0)
                      ('rho #X03C1)
                      ('sigma #X03C3)
                      ('tau #X03C4)
                      ('phi #X03C6)
                      ('chi #X03C7)
                      ('psi #X03C8)
                      ('omega #X03C9))))

(use-package emacs
  :unless have-prettify-symbols
  :config
  (defun substitute-pattern-with-unicode (pattern symbol)
    "Add a font lock hook to replace the matched part of PATTERN with the
  Unicode symbol SYMBOL looked up with UNICODE-SYMBOL."
    (interactive)
    (font-lock-add-keywords
     nil `((,pattern (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                             ,(unicode-symbol symbol))
                             nil))))))

  (defun substitute-patterns-with-unicode (patterns)
    "Call SUBSTITUTE-PATTERN-WITH-UNICODE repeatedly."
    (mapcar #'(lambda (x)
                (substitute-pattern-with-unicode (car x)
                                                 (cdr x)))
            patterns)))

(use-package elisp-mode
  :commands emacs-lisp-mode
  :unless have-prettify-symbols
  :hook (emacs-lisp-mode . elisp-unicode)
  :config
  (defun elisp-unicode ()
    (interactive)
    (substitute-patterns-with-unicode
     (list (cons "(\\(lambda\\>\\)" 'lambda)
           (cons " +\\(nil\\)[ )]" 'nil)))))

(use-package elisp-mode
  :commands emacs-lisp-mode
  :if have-prettify-symbols
  :hook ((emacs-lisp-mode . prettify-symbols-mode)
         (emacs-lisp-mode . (lambda () (setq prettify-symbols-alist elisp-unicode))))
  :config
  (defvar elisp-unicode
     (list (cons "lambda" (unicode-symbol 'lambda))
           (cons "nil" (unicode-symbol 'nil)))))

(use-package python
  :commands python-mode
  :if have-prettify-symbols
  :hook ((python-mode . prettify-symbols-mode)
         (python-mode . (lambda () (setq prettify-symbols-alist python-unicode))))
  :config
  (message "Setting python symbols...")
  (defvar python-unicode
    (append
     (list (cons "not in" (unicode-symbol 'not-element-of))
           (cons "in" (unicode-symbol 'element-of))
           (cons "and" (unicode-symbol 'logical-and))
           (cons "or" (unicode-symbol 'logical-or))
           (cons "not" (unicode-symbol 'logical-neg)))
     (if truemacp
         nil
       (list
        (cons "!=" (unicode-symbol 'not-equal))
        (cons "==" (unicode-symbol 'identical))
        (cons ">=" (unicode-symbol 'greater-than-or-equal-to))
        (cons "<=" (unicode-symbol 'less-than-or-equal-to))
        ))))
  )

(use-package rust-mode
  :if (and have-prettify-symbols (not truemacp))
  :hook ((rust-mode . prettify-symbols-mode)
         (rust-mode . (lambda () (setq prettify-symbols-alist rust-unicode))))
  :config
  (message "Setting Rust symbols...")
  (defvar rust-unicode
    (list (cons "!=" (unicode-symbol 'not-equal))
          (cons "==" (unicode-symbol 'identical))
          (cons ">=" (unicode-symbol 'greater-than-or-equal-to))
          (cons "<=" (unicode-symbol 'less-than-or-equal-to))
          (cons "->" (unicode-symbol 'right-arrow))
          (cons "=>" (unicode-symbol 'Right-arrow))
          (cons ".." (unicode-symbol 'two-ldots))
          ))
  )


(provide 'mw-unicode-symbols-subst)
