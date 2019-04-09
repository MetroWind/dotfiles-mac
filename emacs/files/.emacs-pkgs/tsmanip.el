(defconst tsmanip-timestamp-pattern "\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)\\( +\\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\)\\(:\\([0-9]\\{2\\}\\)\\)?\\)?"
)

(defconst tsmanip-timestamp-groups
  #s(hash-table
     size 10 test eq
     data
     (all 0
      year 1
      month 2
      day 3
      hour 5
      minute 6
      second 8)))

(defun tsmanip-timestamp-groups-query (part)
  (gethash part tsmanip-timestamp-groups nil))

(defun tsmanip-in-regexp (regexp &optional nlines visually)
  "Check if point is inside a match of REGEXP.

Normally only the current line is checked, but you can include
NLINES extra lines around point into the search.  If VISUALLY is
set, require that the cursor is not after the match but really
on, so that the block visually is on the match.

Return nil or a cons cell (BEG . END) where BEG and END are,
respectively, the positions at the beginning and the end of the
match."
  (catch :exit
    (let ((pos (point))
          (eol (line-end-position (if nlines (1+ nlines) 1))))
      (save-excursion
    (beginning-of-line (- 1 (or nlines 0)))
    (while (and (re-search-forward regexp eol t)
            (<= (match-beginning 0) pos))
      (let ((end (match-end 0)))
        (when (or (> end pos) (and (= end pos) (not visually)))
          (throw :exit (cons (match-beginning 0) (match-end 0))))))))))

(defsubst tsmanip-pos-in-match-range (pos n)
  (and (match-beginning n)
       (<= (match-beginning n) pos)
       (>= (match-end n) pos)))

(defun tsmanip-at-timestamp-p ()
  (let* ((pos (point))
         (match? (tsmanip-in-regexp tsmanip-timestamp-pattern)))

    ;; Debug regexp groups
    ;; (mapcar (lambda (i) (message (format "%d %s" i (match-string-no-properties i))))
    ;;         (number-sequence 0 10))

    (cond
     ((not match?) nil)
     ((= pos (match-end 0)) 'after)
     ((tsmanip-pos-in-match-range pos (tsmanip-timestamp-groups-query 'year))
      'year)
     ((tsmanip-pos-in-match-range pos (tsmanip-timestamp-groups-query 'month))
      'month)
     ((tsmanip-pos-in-match-range pos (tsmanip-timestamp-groups-query 'day))
      'day)
     ((tsmanip-pos-in-match-range pos (tsmanip-timestamp-groups-query 'hour))
      'hour)
     ((tsmanip-pos-in-match-range pos (tsmanip-timestamp-groups-query 'minute))
      'minute)
     ((tsmanip-pos-in-match-range pos (tsmanip-timestamp-groups-query 'second))
      'second)
     (t nil))))

(defun tsmanip-match-to-time ()
  (let ((second-str (match-string-no-properties
                     (tsmanip-timestamp-groups-query 'second)))
        (minute-str (match-string-no-properties
                     (tsmanip-timestamp-groups-query 'minute)))
        (hour-str (match-string-no-properties
                   (tsmanip-timestamp-groups-query 'hour))))
    (list (if (null second-str) 0 (string-to-number second-str))
          (if (null minute-str) 0 (string-to-number minute-str))
          (if (null hour-str) 0 (string-to-number hour-str))
          (string-to-number (match-string-no-properties
                             (tsmanip-timestamp-groups-query 'day)))
          (string-to-number (match-string-no-properties
                             (tsmanip-timestamp-groups-query 'month)))
          (string-to-number (match-string-no-properties
                             (tsmanip-timestamp-groups-query 'year)))
          nil nil nil)))

(defun tsmanip-match-range (count)
  (if (null (match-beginning count))
      nil
    (cons (match-beginning count) (match-end count))))

(defun tsmanip-timestamp-match ()
  (let ((data (make-hash-table :test 'eq)))
    (puthash 'all (tsmanip-match-range
                   (tsmanip-timestamp-groups-query 'all))
             data)
    (puthash 'year (tsmanip-match-range
                    (tsmanip-timestamp-groups-query 'year))
             data)
    (puthash 'month (tsmanip-match-range
                     (tsmanip-timestamp-groups-query 'month))
             data)
    (puthash 'day (tsmanip-match-range
                   (tsmanip-timestamp-groups-query 'day))
             data)
    (puthash 'hour (tsmanip-match-range
                    (tsmanip-timestamp-groups-query 'hour))
             data)
    (puthash 'minute (tsmanip-match-range
                      (tsmanip-timestamp-groups-query 'minute))
             data)
    (puthash 'second (tsmanip-match-range
                      (tsmanip-timestamp-groups-query 'second))
             data)
    data))

(defun tsmanip-timestamp-match-query (ts-match part)
  (gethash part ts-match))

(defun tsmanip-time-add (time0 delta unit)
  "Return TIME0 plus DELTA number of UNIT. Argument TIME0 is a
time list in the form of (SECOND MINUTE HOUR DAY MONTH YEAR) (see
`encode-time'); DELTA is a number; UNIT is one of `second',
`minute', `hour', `day', `month', or `year'.

Return a time value."

  (let ((second0 (car time0))
        (minute0 (nth 1 time0))
        (hour0 (nth 2 time0))
        (day0 (nth 3 time0))
        (month0 (nth 4 time0))
        (year0 (nth 5 time0)))

    (cond ((eq unit 'month)
           (let* ((month-new (+ month0 delta)))
             ;; Function `encode-time’ supports out-of-range values.
             ;; So we don’t worry about month-new > 12 in here.
             (encode-time second0 minute0 hour0 day0 month-new year0)))

          ((eq unit 'year)
           (let* ((year-new (+ year0 delta)))
             ;; Function `encode-time’ supports out-of-range values.
             ;; So we don’t worry about leap year here.
             (encode-time second0 minute0 hour0 day0 month0 year-new)))

          (t
           (time-add
            (apply #'encode-time time0)
            (cond ((eq unit 'second) (seconds-to-time delta))
                  ((eq unit 'minute) (seconds-to-time (* 60 delta)))
                  ((eq unit 'hour) (seconds-to-time (* 3600 delta)))
                  ((eq unit 'day) (days-to-time delta))))))))

(defun tsmanip-replace-timestamp (ts-match new-time)
  (cl-flet ((replace-part
             (part time-str)
             (let ((part-region (tsmanip-timestamp-match-query ts-match part)))
               (if (not (null part-region))
                   (progn
                     (delete-region (car part-region) (cdr part-region))
                     (goto-char (car part-region))
                     (insert (format-time-string time-str new-time)))))))
    (replace-part 'year "%Y")
    (replace-part 'month "%m")
    (replace-part 'day "%d")
    (replace-part 'hour "%H")
    (replace-part 'minute "%M")
    (replace-part 'second "%S")))

(defun tsmanip-timestamp-change (n)
  "Change the time in the time stamp at point. The time will be
changed by N units, where the unit is defined by the cursor
location."
  (let ((timestamp? (tsmanip-at-timestamp-p)))
    (unless timestamp? (user-error "Not at a timestamp"))

    (let* ((the-match (tsmanip-timestamp-match))
           (unit (if (eq timestamp? 'after)
                     (if (null (tsmanip-timestamp-match-query the-match 'hour))
                         'day
                       (if (null (tsmanip-timestamp-match-query the-match 'second))
                           'minute
                         'second))
                   timestamp?))
           (time0 (tsmanip-match-to-time))
           (changing-part-range (tsmanip-timestamp-match-query
                                 the-match unit))
           ;; Calculate new time
           (time-new (tsmanip-time-add time0 n unit)))

      (save-excursion
        ;; Delete old time part, and insert new.
        (tsmanip-replace-timestamp the-match time-new)))))

;;;###autoload
(defun tsmanip-timestamp-increase (&optional n)
  "Change the time in the time stamp at point. The time will be
changed forward by N units, where the unit is defined by the
cursor location."
  (interactive "p")
  (tsmanip-timestamp-change (if (null n) 1 n)))

;;;###autoload
(defun tsmanip-timestamp-decrease (&optional n)
  "Change the time in the time stamp at point. The time will be
changed backward by N units, where the unit is defined by the
cursor location."
  (interactive "p")
  (tsmanip-timestamp-change (* -1 (if (null n) 1 n))))

(provide 'tsmanip)
