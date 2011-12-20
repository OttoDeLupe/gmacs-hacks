;;; -*- Mode: Lisp; Package: USER ; Base: 10.; Syntax: Common-lisp -*-

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
;;; 9/18/88
;;;
;;; $Id: site-hacks.el,v 1.22 1993/05/07 15:57:35 fad Exp $

;;; This file contains only site specific hacks.
;;;

(dfs-load-n "fax-hacks.el")

;;printer hacks:
;; problem: lpr does not work on my printer
;; enscript does not like -T switch from lpr-buffer!

;; Solution: replace one lpr function so -T is not sent.
;; it redefines the print-region-1 function, sets some switches
;; Eventually do this with advice on lpr.




(setq lpr-command "enscript")


;(require 'lpr)
; require lpr didn't seem to provide lpr, but was not preloaded?
(load "lpr.elc")
(defun print-region-1 (start end switches page-headers)
  (let ((name (concat (buffer-name) " Emacs buffer"))
	(width tab-width))
    (save-excursion
      (message "Spooling...")
      (if (/= tab-width 8)
	  (progn
	    (print-region-new-buffer start end)
	    (setq tab-width width)
	    (save-excursion
	      (goto-char end)
	      (setq end (point-marker)))
	    (untabify (point-min) (point-max))))
      (if page-headers
	  (if (eq system-type 'usg-unix-v)
	      (progn
		(print-region-new-buffer start end)
		(call-process-region start end "pr" t t nil))
	    ;; On BSD, use an option to get page headers.
	    (setq switches (cons "-p" switches))))
      (apply (or print-region-function 'call-process-region)
	     (nconc (list start end lpr-command
			  nil nil nil)
		    (nconc (and (eq system-type 'berkeley-unix)
				(list "-J" name))
			   (and (not (equal lpr-command "enscript"))
				(eq system-type 'berkeley-unix)
				(list "-T" name))
			   switches)))
      (if (markerp end)
	  (set-marker end nil))
      (message "Spooling...done"))))


(defun lpr-set-switches-default ()
  "Set switches for lpr to gaudy base"
  (interactive)
  (setq lpr-switches (list "-G")))

;; gaudy two col, rotated
(defun lpr-set-switches-two-col-rotated ()
  (interactive)
  (lpr-set-switches-default)
  (setq lpr-switches (append '("-2r") lpr-switches)))

(defun lpr-set-switches-one-col ()
  (interactive)
  (lpr-set-switches-default))

;; init switches for gaudy
(lpr-set-switches-one-col)

