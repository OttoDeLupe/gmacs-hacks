;;; -*-Emacs-Lisp-*- General command interpreter in a window stuff
;;; Copyright Olin Shivers (1988).
;;; Please imagine a long, tedious, legalistic 5-page gnu-style copyright
;;; notice appearing here to the effect that you may use this code any
;;; way you like, as long as you don't charge money for it, remove this
;;; notice, or hold me liable for its results.

;;; The changelog is at the end of this file.

;;; Please send me bug reports, bug fixes, and extensions, so that I can
;;; merge them into the master source.
;;;     - Olin Shivers (shivers@cs.cmu.edu)

;;; These are the less-commonly-used functions from comint.el

(require 'comint)
(provide 'comint-extra)

(defun comint-psearch-input ()
  "Search forwards for next occurrence of prompt and skip to end of line.
\(prompt is anything matching regexp comint-prompt-regexp)"
  (interactive)
  (if (re-search-forward comint-prompt-regexp (point-max) t)
      (end-of-line)
      (error "No occurrence of prompt found")))

(defun comint-msearch-input ()
  "Search backwards for previous occurrence of prompt and skip to end of line.
Search starts from beginning of current line."
  (interactive)
  (let ((p (save-excursion
	     (beginning-of-line)
	     (cond ((re-search-backward comint-prompt-regexp (point-min) t)
		    (end-of-line)
		    (point))
		   (t nil)))))
    (if p (goto-char p)
	(error "No occurrence of prompt found"))))

(defun comint-msearch-input-matching (str)
  "Search backwards for occurrence of prompt followed by STRING.
STRING is prompted for, and is NOT a regular expression."
  (interactive (let ((s (read-from-minibuffer 
			 (format "Command (default %s): "
				 comint-last-input-match))))
		 (list (if (string= s "") comint-last-input-match s))))
; (interactive "sCommand: ")
  (setq comint-last-input-match str) ; update default
  (let* ((r (concat comint-prompt-regexp (regexp-quote str)))
	 (p (save-excursion
	      (beginning-of-line)
	      (cond ((re-search-backward r (point-min) t)
		     (end-of-line)
		     (point))
		    (t nil)))))
    (if p (goto-char p)
	(error "No match"))))

(defun comint-next-similar-input (arg)
  "Reenters the next input that matches the string typed so far.  If repeated 
successively newer inputs are reentered.  If arg is -1, it will go back
in the history, if 1 it will go forward."
  (interactive "p")
  (setq this-command 'comint-previous-similar-input)
  (comint-previous-similar-input (- arg)))

;;; Change log:
;;; 31/3/92 Dave Smith dsmith@stats.adelaude.edu.au
;;;  - Created this file. It currently comprises the deprecated functions
;;;    from version 2.03 of comint.el which I happened to like. It is
;;;    currently the user's job to load this file when required, but maybe
;;;    comint.el could handle it with autoloads.
;;;  - Added comint-next-similar-input, ideal for binding to an arrow key.



;;;
;;; Similar input -- contributed by ccm and highly winning.
;;;
;;; Reenter input, removing back to the last insert point if it exists. 
;;;
(defvar comint-last-input-match ""
  "Last string searched for by comint input history search, for defaulting.
Buffer local variable.") 

(defun comint-init-last-input-match ()
  (make-local-variable 'comint-last-input-match)
  (setq comint-last-input-match ""))


;;; gary added fix
;;From: Jonny Goldman <jonny@Synopsys.COM> for Emacs 19.26 and transient-mark-mode
(defvar comint-input-mark)

(defvar comint-last-similar-string "" 
  "The string last used in a similar string search.")
(defun comint-previous-similar-input (arg)
  "Reenters the last input that matches the string typed so far.  If repeated 
successively older inputs are reentered.  If arg is 1, it will go back
in the history, if -1 it will go forward."
  (interactive "p")
  (if (not (comint-after-pmark-p))
      (error "Not after process mark"))
  (if (not (eq last-command 'comint-previous-similar-input))
      (setq comint-input-ring-index -1
	    comint-last-similar-string 
	    (buffer-substring 
	     (process-mark (get-buffer-process (current-buffer)))
	     (point))))
  (let* ((size (length comint-last-similar-string))
	 (len (ring-length comint-input-ring))
	 (n (+ comint-input-ring-index arg))
	 entry)
    (while (and (< n len) 
		(or (< (length (setq entry (ring-ref comint-input-ring n))) size)
		    (not (equal comint-last-similar-string 
				(substring entry 0 size)))))
      (setq n (+ n arg)))
    (cond ((< n len)
	   (setq comint-input-ring-index n)
	   (if (eq last-command 'comint-previous-similar-input)
	       (delete-region comint-input-mark (point)) ; repeat
	       (setq comint-input-mark (point)))	      ; 1st time
;; fix from jonny@Synopsys.com
;;;	       (delete-region (mark) (point)) ; repeat
;;;	       (push-mark (point)))	      ; 1st time
	   (insert (substring entry size)))
	  (t (message "Not found.") (ding) (sit-for 1)))
    (message "%d" (1+ comint-input-ring-index))))

  
(add-hook 'comint-mode-hook
	  (lambda () (comint-init-last-input-match)))
