;; Maximize and restore window.
(defvar window-confs (make-hash-table))
;; window-confs looks like this:
;;
;; { frame1: {"zoomed": false, "conf": <some conf>},
;;   frame2: {"zoomed": true, "conf": <some other conf>} }

(defun save-window-conf ()
  "Save current window configuration of current frame."
  (let ((this-frame (selected-frame))
        (this-conf (current-window-configuration)))
    (if (gethash this-frame window-confs)
        (puthash 'conf this-conf
                 (gethash this-frame window-confs))
      (let ((new-state (make-hash-table)))
        (puthash 'zoomed nil new-state)
        (puthash 'conf this-conf new-state)
        (puthash this-frame new-state window-confs)))))

(defun load-window-conf ()
  "Load saved window configuration of current frame."
  (let* ((this-frame (selected-frame))
         (saved-state (gethash this-frame window-confs)))
    (if saved-state
        (set-window-configuration (gethash 'conf saved-state)))))

(defun maximize-or-restore-window ()
  "If current window is not maximized, or no window in current
frame has been maximized before, store window configuration, and
maximize current window.  If current window is maximized, and
there's a saved window configuration for current frame, restore
that window configuartion."
  (interactive)
  (let* ((this-frame (selected-frame))
         (saved-state (gethash this-frame window-confs)))
    (if saved-state
        (if (gethash 'zoomed saved-state)
            ;; Already maximized.  Restore.
            (progn
              (load-window-conf)
              (puthash 'zoomed nil saved-state))
          ;; Not maximized.  Save window conf and maximize.
          (progn
            (save-window-conf)
            (delete-other-windows)
            (puthash 'zoomed t saved-state)))

      ;; Never maximized before.  Save window conf and maximize.
      (progn
        (puthash this-frame (make-hash-table) window-confs)
        (maximize-or-restore-window)))))

(provide 'mw-lib-window-focus)
