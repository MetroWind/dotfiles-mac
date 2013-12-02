;;every settings about manage time

(require 'calendar)
(require 'cal-china)
;; (require 'chinese-calendar)

(calendar-set-date-style 'iso)
(global-set-key (kbd "C-<f11>") 'calendar)

;;path settings
(cond
 (linuxp
  (setq diary-file "/mnt/shared/text/Non-books/Misc/calendar/diary")
  (setq todo-file-do "/mnt/shared/text/Non-books/Misc/calendar/todo-do")
  (setq todo-file-done "/mnt/shared/text/Non-books/Misc/calendar/todo-done")
  (setq todo-file-top "/mnt/shared/text/Non-books/Misc/calendar/todo-top"))
 (macp
  (setq diary-file "/Volumes/Stuff/text/Non-books/Misc/calendar/diary")
  (setq todo-file-do "/Volumes/Stuff/text/Non-books/Misc/calendar/todo-do")
  (setq todo-file-done "/Volumes/Stuff/text/Non-books/Misc/calendar/todo-done")
  (setq todo-file-top "/Volumes/Stuff/text/Non-books/Misc/calendar/todo-top")))

;;Appointment Settings
(setq appt-issue-message t)
(add-hook 'diary-hook 'appt-make-list)
(setq appt-display-format 'window)

;; make emacs be able to tell me when sunrise and sunset
(setq calendar-latitude +39.54)
(setq calendar-longitude +116.28)
(setq calendar-location-name "Beijing")

;; make the infomation more readable to me
(setq chinese-calendar-celestial-stem
      ["甲" "乙" "丙" "丁" "戊" "己" "庚" "辛" "壬" "癸"])
(setq chinese-calendar-terrestrial-branch
      ["子" "丑" "寅" "卯" "辰" "巳" "戊" "未" "申" "酉" "戌" "亥"])

(setq calendar-remove-frame-by-deleting t)
(setq mark-diary-entries-in-calendar t)
(setq mark-holidays-in-calendar t)
;; (setq view-calendar-holidays-initially nil)

;; my advice for exit-calendar : save and close the _diray buffer
(defadvice exit-calendar (before my-advice-close-diary ())
  "save and close the _diary buffer to avoid open it when
starting emacs next time."
  (let ((bf (find-file-noselect diary-file t)))
    (and bf
         (save-excursion
           (with-current-buffer bf (save-buffer) (kill-buffer nil))))))

(ad-activate 'exit-calendar)

;; ========== Four more diary function ====================>
(defun diary-block-weekday (m1 d1 y1 m2 d2 y2 w &optional mark)
  "Block diary entry.
Entry applies if date is between, or on one of, two dates and the day of week is W.
The order of the parameters is
M1, D1, Y1, M2, D2, Y2 if `european-calendar-style' is nil, and
D1, M1, Y1, D2, M2, Y2 if `european-calendar-style' is t.

An optional parameter MARK specifies a face or single-character string to
use when highlighting the day in the calendar."

  (let ((date1 (calendar-absolute-from-gregorian
                (if european-calendar-style
                    (list d1 m1 y1)
                  (list m1 d1 y1))))
        (date2 (calendar-absolute-from-gregorian
                (if european-calendar-style
                    (list d2 m2 y2)
                  (list m2 d2 y2))))
        (d (calendar-absolute-from-gregorian date)))
    (if (and (<= date1 d) (<= d date2) (= w (calendar-day-of-week date)))
        (cons mark entry))))

(defun diary-cyclic-weekday (n month day year w &optional mark)
"Cycle diary entry--entry applies every N days starting at MONTH,
DAY, YEAR if the day of week is W.  If `european-calendar-style'
is t, parameters are N, DAY, MONTH, YEAR.  ENTRY can contain `%d'
or `%d%s'; the %d will be replaced by the number of repetitions
since the MONTH DAY, YEAR and %s will be replaced by the ordinal
ending of that number (that is, `st', `nd', `rd' or `th', as
appropriate.

An optional parameter MARK specifies a face or single-character
string to use when highlighting the day in the calendar."
(let* ((d (if european-calendar-style
              month
            day))
       (m (if european-calendar-style
              day
            month))
       (diff (- (calendar-absolute-from-gregorian date)
                (calendar-absolute-from-gregorian
                 (list m d year))))
       (cycle (/ diff n)))
  (if (and (>= diff 0) (zerop (% diff n)) (= w (calendar-day-of-week date)))
      (cons mark (format entry cycle (diary-ordinal-suffix cycle))))))

(defun diary-block-cyclic (n m1 d1 y1 m2 d2 y2 &optional mark)
"Applies if date is betweek M1 D1 Y1 and M2 D2 Y2 every N days."
(let* ((d (if european-calendar-style
              m1
            d1))
       (m (if european-calendar-style
              d1
            m1))
       (diff (- (calendar-absolute-from-gregorian date)
                (calendar-absolute-from-gregorian
                 (list m d y1))))
       (cycle (/ diff n))
       (date1 (calendar-absolute-from-gregorian
               (if european-calendar-style
                   (list d1 m1 y1)
                 (list m1 d1 y1))))
       (date2 (calendar-absolute-from-gregorian
               (if european-calendar-style
                   (list d2 m2 y2)
                 (list m2 d2 y2))))
       (dd (calendar-absolute-from-gregorian date)))

  (if (and (>= diff 0) (zerop (% diff n)) (<= date1 dd) (<= dd date2))
      (cons mark (format entry cycle (diary-ordinal-suffix cycle))))))


(defun diary-block-cyclic-weekday (n m1 d1 y1 m2 d2 y2 w &optional mark)
"Applies if date is betweek M1 D1 Y1 and M2 D2 Y2 every N days if
the day of week is W."
(let* ((d (if european-calendar-style
              m1
            d1))
       (m (if european-calendar-style
              d1
            m1))
       (diff (- (calendar-absolute-from-gregorian date)
                (calendar-absolute-from-gregorian
                 (list m d y1))))
       (cycle (/ diff n))
       (date1 (calendar-absolute-from-gregorian
               (if european-calendar-style
                   (list d1 m1 y1)
                 (list m1 d1 y1))))
       (date2 (calendar-absolute-from-gregorian
               (if european-calendar-style
                   (list d2 m2 y2)
                 (list m2 d2 y2))))
       (dd (calendar-absolute-from-gregorian date)))

  (if (and (>= diff 0) (zerop (% diff n)) (<= date1 dd) (<= dd date2) (= w (calendar-day-of-week date)))
      (cons mark (format entry cycle (diary-ordinal-suffix cycle))))))
;; ========== end ====================>

;; ========== Chinese Holidays start ====================>
(defun holiday-chinese (cmonth cday string)
  "Chinese calendar holiday, month and day in Chinese calendar (CMONTH, CDAY).
If corresponding MONTH and DAY in gregorian calendar is visible,
the value returned is the list \(((MONTH DAY year) STRING)).
Returns nil if it is not visible in the current calendar window."
  (let* ((m displayed-month)
         (y displayed-year)
         (gdate (calendar-gregorian-from-absolute
                 (+ (cadr (assoc cmonth (calendar-chinese-year y))) (1- cday))))
         (gm (car gdate))
         (gd (cadr gdate))
         (gy (caddr gdate)))
    (increment-calendar-month m y (- 11 gm))
    (if (> m 9)
        (list (list (list gm gd gy) string)))))

;;;定义中国的节日
(setq china-general-holidays 
      '((holiday-fixed 1 1 "元旦")
        (holiday-chinese-new-year )
        (holiday-fixed 3 8 "妇女节")
        (holiday-fixed 3 12 "植树节")
        (holiday-fixed 5 1 "劳动节")
        (holiday-fixed 5 4 "青年节")
        (holiday-fixed 6 1 "儿童节")
        (holiday-fixed 9 10 "教师节")
        (holiday-fixed 10 1 "国庆节")))

(setq china-holidays
      '((holiday-chinese 1 15 "元宵节")
        (holiday-fixed 4 5 "清明节")
        (holiday-chinese 5 5 "端午节")
        (holiday-chinese 7 7 "七夕节")
        (holiday-chinese 9 9 "重阳节")
        (holiday-chinese 8 15 "中秋节")))

;; (setq christian-holidays nil)
;; (setq hebrew-holidays  nil)
;; (setq islamic-holidays nil)

(setq other-holidays 
      '((holiday-fixed 2 14 "情人节")
        (holiday-fixed 4 1 "愚人节")
        (holiday-fixed 12 25 "圣诞节")
        (holiday-float 5 0 2 "母亲节")
        (holiday-float 6 0 3 "父亲节")))

(setq calendar-holidays
      (append general-holidays china-general-holidays china-holidays other-holidays))

(defun holiday-chinese-new-year ()
  "Date of Chinese New Year."
  (let ((m displayed-month)
        (y displayed-year))
    (increment-calendar-month m y 1)
    (if (< m 5)
        (let ((chinese-new-year
               (calendar-gregorian-from-absolute
                (car (cdr (assoc 1 (calendar-chinese-year y)))))))
          (if (calendar-date-is-visible-p chinese-new-year)
          (list
           (list chinese-new-year
                 (format "%s年春节"
                         (calendar-chinese-sexagesimal-name (+ y 57))))))))))
;; ========== Chinese Holidays end ====================>

;; Fancy diary display
(add-hook 'diary-display-hook 'fancy-diary-display)
(add-hook 'list-diary-entries-hook 'sort-diary-entries t)

;; enable appoitment alert.
(appt-activate 1)
