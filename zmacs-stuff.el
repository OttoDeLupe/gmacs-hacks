;;; -*- Emacs-Lisp -*-

(defun find-unbalanced-parentheses ()
  "Check the buffer for unbalanced parentheses.  Stops at any that are 
unbalanced."
  (interactive)
  (let ((start-point (point))
	)
    (goto-char (point-min))
    (condition-case e
	 (while (/= (point) (point-max))
	   (forward-sexp)
	   )
      (error
	;; If this is an extra left paren error, we have to scan backwards to 
	;; find the exact left paren in error
	(cond ((and (eq (car e) 'error)
		    (string-equal (cadr e) "Unbalanced parentheses")
		    )
	       ;; left paren error
	       (goto-char (point-max))
	       (while (/= (point) (point-min))
		 (condition-case e (backward-sexp)
		    (error
		      (error "Probably an extra left parenthesis here.")))
		 ))
	      (t
	       (error "Probably an extra right parenthesis here.")
	       ))
	))
    (goto-char start-point)
    (message "All parentheses appear balanced.")
    ))



(defun switch-to-other-buffer  (&optional arg)
  "Switches to the other buffer, as determined by (other-buffer).
   With a non-null ARG (numeric prefix), switches to the nth buffer
   (1-origin) in the history.  As with ZMACS, 0 and 1 are special cases:
   0 means display a buffer list, while 1 means bury current buffer."
  (interactive "P")
  (cond
   ((null arg)
    (switch-to-buffer (other-buffer)))
   ((= 0 (setq arg
	       (prefix-numeric-value arg))) ;; In case C-U, etc.
    (electric-buffer-list nil))
   ((= 1 arg)
    (bury-buffer))
   (t (switch-to-buffer
       (or (nth-recent-buffer (1- arg))
	   (error "Not that many buffers."))))
   ))

(defun nth-recent-buffer (n)
  (let ((possibilities
	 (mapcan '(lambda (buf)
		    (if (or (eq ?\  (aref (buffer-name buf) 0)))
			nil
		      (list buf)))
		 (buffer-list))))
    (nth n possibilities)))


(defun split-window-vertically-other-buffer (arg)
  "Split current window into two windows, one above the other.
This window becomes the uppermost of the two, and gets
ARG lines.  No arg means split equally.
This selects the (other-buffer) in the other window."
  (interactive "P")
  (split-window-vertically arg)
  (other-window 1)
  (switch-to-buffer (other-buffer))
  )

