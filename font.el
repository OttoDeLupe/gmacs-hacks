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
;;; 5/3/89 fad
;;;
;;; $Id: font.el,v 1.3 1990/04/27 11:06:13 mjab Exp $

;;; functions to change the font on emacs running on X

(defun font-big () "Set font to largest size if running X."
  (interactive)
  (mouse-set-font "-adobe-courier-medium-r-normal--18-180-75-75-m-110-iso8859-1"))

(defun font-medium () "Set font to medium size if running X."
  (interactive)
  (mouse-set-font "9x15"))

(defun font-small () "Set font to small size if running X."
  (interactive)
  (mouse-set-font "8x13"))

(defun font-normal () "Set font to normal size if running X."
  (interactive)
  (mouse-set-font "fixed"))

(defun font-tiny () "Set font to smallest size if running X."
  (interactive)
  (mouse-set-font "6x12"))

(defun font-apl () "Set font to APL"
       (interactive)
       (mouse-set-font "kaplcour"))
