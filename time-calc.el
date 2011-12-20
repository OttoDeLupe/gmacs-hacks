;;; This is a time calculation system for GNU Emacs
;;;
;;;
;;;				NO WARRANTY
;;;
;;; This software is distributed free of charge and is in the public domain.
;;; Anyone may use, duplicate or modify this program.  Thinking Machines
;;; Corporation does not restrict in any way the use of this software by
;;; anyone.
;;; 
;;; Thinking Machines Corporation provides absolutely no warranty of any kind.
;;; The entire risk as to the quality and performance of this program is with
;;; you.  In no event will Thinking Machines Corporation be liable to you for
;;; damages, including any lost profits, lost monies, or other special,
;;; incidental or consequential damages arising out of the use of this program.
;;;
;;; You must not restrict the distribution of this software.

;;9/89
;;This is copied from completion.el, to eliminate dependencies

(require 'cl)
(provide 'time-calc)

;;;-----------------------------------------------
;;; Time 
;;;-----------------------------------------------
;;; What a backwards way to get the time ! Unfortunately, GNU Emacs
;;; doesn't have an accessible time function.

(defconst tc-hours-per-day  24)
(defconst tc-hours-per-year  (* 365 tc-hours-per-day))
(defconst tc-hours-per-4-years  (+ (* 4 tc-hours-per-year)
				    tc-hours-per-day))
(defconst tc-days-since-start-of-year
    '(0 31 59 90 120 151 181 212 243 273 304 334))
(defconst tc-days-since-start-of-leap-year
    '(0 31 60 91 121 152 182 213 244 274 305 335))
(defconst tc-months
    '("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")
  )

(defun tc-hours-since-1900-internal (month day year hours)
  "Month is an integer from 1 to 12.  Year is a two digit integer (19XX)"
  (+ ;; Year
    (* (/ (1- year) 4) tc-hours-per-4-years)
    (* (1+ (mod (1- year) 4)) tc-hours-per-year)
    ;; minus two to account for  1968 rather than 1900
    ;; month
    (* tc-hours-per-day
       (nth (1- month) (if (zerop (mod year 4))
			   tc-days-since-start-of-leap-year
			   tc-days-since-start-of-year)))
    (* (1- day) tc-hours-per-day)
    hours
    ))

(defun tc-month-from-string (month-string)
  "Month string is a three char. month string"
  (let ((count 1))
    (do ((list tc-months (cdr list))
	 )
	((or (null list) (string-equal month-string (car list))))
      (setq count (1+ count)))
    (if (> count 12)
	(error "Unknown month - %s" month-string))
    count))

(defun tc-hours-since-1900 (&optional time-string)
  "String is a string in the format of current-time-string (the default)."
  (let* ((string (or time-string (current-time-string)))
	 (month (tc-month-from-string (substring string 4 7)))
	 (day (string-to-int (substring string 8 10)))
	 (year (string-to-int (substring string 22 24)))
	 (hour (string-to-int (substring string 11 13)))
	 )
    (tc-hours-since-1900-internal month day year hour)
    ))

;;; Tests -
;;;(tc-hours-since-1900 "Wed Jan  1 00:00:28 1900") --> 0
;;;(tc-hours-since-1900 "Wed Nov  2 23:00:28 1988") --> 778751
;;;(tc-hours-since-1900 "Wed Jan 23 14:34:28 1988") --> 771926
;;;(tc-hours-since-1900 "Wed Feb 23 14:34:28 1988") --> 772670
;;;(tc-hours-since-1900 "Wed Mar 23 14:34:28 1988") --> 773366
;;;(tc-hours-since-1900 "Wed Apr 23 14:34:28 1988") --> 774110
;;;(tc-hours-since-1900 "Wed May 23 14:34:28 1988") --> 774830
;;;(tc-hours-since-1900 "Wed Jun 23 14:34:28 1988") --> 775574
;;;(tc-hours-since-1900 "Wed Jul 23 14:34:28 1988") --> 776294
;;;(tc-hours-since-1900 "Wed Aug 23 14:34:28 1988") --> 777038
;;;(tc-hours-since-1900 "Wed Sep 23 14:34:28 1988") --> 777782
;;;(tc-hours-since-1900 "Wed Oct 23 14:34:28 1988") --> 778502
;;;(tc-hours-since-1900 "Wed Nov 23 14:34:28 1988") --> 779246
;;;(tc-hours-since-1900 "Wed Dec 23 14:34:28 1988") --> 779966
;;;(tc-hours-since-1900 "Wed Jan 23 14:34:28 1957") --> 500198
;;;(tc-hours-since-1900 "Wed Feb 23 14:34:28 1957") --> 500942
;;;(tc-hours-since-1900 "Wed Mar 23 14:34:28 1957") --> 501614
;;;(tc-hours-since-1900 "Wed Apr 23 14:34:28 1957") --> 502358
;;;(tc-hours-since-1900 "Wed May 23 14:34:28 1957") --> 503078
;;;(tc-hours-since-1900 "Wed Jun 23 14:34:28 1957") --> 503822
;;;(tc-hours-since-1900 "Wed Jul 23 14:34:28 1957") --> 504542
;;;(tc-hours-since-1900 "Wed Aug 23 14:34:28 1957") --> 505286
;;;(tc-hours-since-1900 "Wed Sep 23 14:34:28 1957") --> 506030
;;;(tc-hours-since-1900 "Wed Oct 23 14:34:28 1957") --> 506750
;;;(tc-hours-since-1900 "Wed Nov 23 14:34:28 1957") --> 507494
;;;(tc-hours-since-1900 "Wed Dec 23 14:34:28 1957") --> 508214
