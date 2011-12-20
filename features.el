;;; $Id: features.el,v 1.1 1988/12/27 17:06:20 fad Exp $

;; features.el -- provide Common Lisp (semi)compatible features
;; Copyright (C) 1988 Free Software Foundation, Inc. and Robert Krawitz

;; This file might become part of GNU Emacs

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.

;;; Written by Robert Krawitz <rlk> 7/10/88

(require 'cl)
(provide 'features)

;;; In Common Lisp this would be defparameter.

(defconst *features*
  '(emacs tmc lucid cm-5\.0)
  "*Same as common lisp *features*")

(defun features-regexp ()
  "Returns a regexp matching all *features* only"
  (mapconcat (function (lambda (symbol) (regexp-quote (symbol-name symbol))))
	     *features*
	     "\\|"))

(defun evaluate-conditionals (form symbols)
  "Evaluates FORM as a boolean expression based on the presence of symbols in
SYMBOLS.  Useful for common lisp read-time conditionals.
In any sane lisp system, this could be done much more reasonably.  It's
amazing how much case sensitivity breaks things."
  (catch 'return
    (cond ((symbolp form)
	   (if (memq (intern (downcase (symbol-name form))) symbols)
	       t nil))
	  ((listp form)
	   (case (car form)
	     (not (not (evaluate-conditionals (cadr form) symbols)))
	     (or (dolist (subform (cdr form))
		   (when (evaluate-conditionals subform symbols)
		     (throw 'return t)))
		 nil)
	     (and (dolist (subform (cdr form))
		    (when (not (evaluate-conditionals subform symbols))
		      (throw 'return nil)))
		  t)
	     (t (error "Illegal conditional form %s" (prin1-to-string form)))))
	  (t (error "Illegal conditional form %s" (prin1-to-string form))))))

(defun strip-conditionals ()
  "Half-heartedly expand reader conditionals (#+ or #-) in buffer.  Doesn't
handle compound conditionals yet."
  (save-excursion
    (goto-char (point-min))
    (let* ((features-regexp (concat "\\(" (features-regexp) "\\)"))
	   (yes-features-regexp (concat "+" features-regexp))
	   (no-features-regexp (concat "-" features-regexp))
	   (start))
      (while (re-search-forward "\#[-+]" nil t)
	(let ((cond (char-after (1- (point)))))
	  (setq start (- (point) 2))
	  (let* ((form (read (current-buffer)))
		 (value (evaluate-conditionals form *features*)))
	    (delete-region start (point))
	    (when (or (and (= cond ?-) value)
		      (and (= cond ?+) (not value)))
	      (forward-sexp 1)
	      (delete-region start (point)))))))))
