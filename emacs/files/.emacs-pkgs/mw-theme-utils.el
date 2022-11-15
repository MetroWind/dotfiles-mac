(require 'mw-lib-generic)

(defun mw-apply-theme (theme theme-hooks auto-sml-theme-excludes)
  "Apply the theme indicated by `theme' (symbol). The main difference
  between this and `load-theme' is this takes a `theme-hooks'
  argument, which contains functions to call before and after loading
  the theme. The function also set the appropriate smart-mode-line
  theme after that.

  `Theme-hooks' is a list, whose elements are in the form of

    (REGEXP . (PRE-HOOK . POST-HOOK))

  REGEXP is a regular expression that matches against the name of the
  theme, whereas PRE-HOOK and POST-HOOK are callables. If the regular
  expression is a full match, `apply-theme' calls the PRE-HOOK, loads
  the theme, and calls the POST-HOOK. If the none of the regular
  expressions matches, the theme is loaded without hooks.

  If the theme is not in `auto-sml-theme-excludes', a smart-mode-line
  theme is also loaded. It could be `smart-mode-line-light' or
  `smart-mode-line-dark', depending on the `background-type' of the
  current frame. This is for themes that do not explicitly set
  smart-mode-line faces, which could make the mode line look out of
  place. If a theme explicitly sets smart-mode-line faces, it should
  be in `auto-sml-theme-excludes', so that these setting would not be
  overridden."

  (defun full-match (regexp s)
    (let ((match-idx (string-match regexp s)))
      (if (null match-idx)
          nil
        (if (not (= match-idx 0))
            nil
          (= (match-end 0) (length s))))))

  (message "Applying the %s theme..." theme)
  (let ((has-hook nil))
    (dolist (theme-hook theme-hooks)
      (if (full-match (car theme-hook) (symbol-name theme))
          (progn
            (setq has-hook t)
            (if (functionp (car (cdr theme-hook)))
                (funcall (car (cdr theme-hook))))
            (load-theme theme t nil)
            (if (functionp (cdr (cdr theme-hook)))
                (funcall (cdr (cdr theme-hook)))))))
    (if (not has-hook)
        (load-theme theme t nil)))
  (if (not (memq theme auto-sml-theme-excludes))
      (case (frame-parameter nil 'background-mode)
        ('dark (load-theme 'smart-mode-line-dark t nil))
        ('light (load-theme 'smart-mode-line-light t nil)))))

(defun mw-apply-random-theme (excludes theme-hooks auto-sml-theme-excludes)
  "Apply a random theme, but never the themes listed in `excludes'.
  This calls `mw-apply-theme' with `theme-hooks' and
  `auto-sml-theme-excludes'."

  (defun pick-random-theme (themes)
    (let ((theme (nth (random (length themes)) themes)))
      (if (memq theme excludes)
          (pick-random-theme themes)
        theme)))

  (mw-apply-theme (pick-random-theme (custom-available-themes)) theme-hooks
                  auto-sml-theme-excludes))

(provide 'mw-theme-utils)
