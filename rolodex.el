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
;;; Thu May  4 1989
;;;
;;; $Id: rolodex.el,v 1.23 1993/04/14 19:16:50 fad Exp $
;;;
;;;  This file contains a rolodex utility.
;;;  The database is very freeform
;;;
;;;  Gary Sabot 7/89
;;;  Franklin Davis 8/89
;;;  patch so rolo-sort leaves local vars at end of buffer.
;;;	Atherton.COM!bliven@ames.UUCP (Andy Bliven)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'rolodex)
;(require 'extras)

; To use this, type M-x rolo.  The file "rolofile" in your home 
; directory contains the rolodex data.  You can change this file
; with the variable rolo-file-name

; Suggested entry in your .emacs file (remove the semicolons):
;;  (autoload 'rolo "rolodex" "Edit rolodex file using rolo-mode" t)
;;  (autoload 'rolo-expand-name-to-email "rolodex" "Expand name before point
;;  		to rolodex email entry." t)
;;  (define-key ctl-x-4-map "e" 'rolo-expand-name-to-email)
;;  (autoload 'rolo-find "rolodex" "Find an entry in your rolodex file." t)
;;  (define-key ctl-x-4-map "r" 'rolo-find)

(defvar rolo-mode-map nil "Keybindings for rolo mode")
(defvar rolo-mode-abbrev-table nil "")

(defvar rolo-file-name "~/rolofile"
  "*Where to find the rolodex file for M-X rolo.  
It should contain entries that look like this:

Name:		somebody
Work Phone: 	(617) 555-1234
Home Phone: 	
Company: 	
Work Address: 	
Home Address: 	
Remarks: 	
Fax:		
Printing Info:	pocket
Date Updated: 	Wed Jul 12 1989

The only rules are that the Name field must be the first, and the Date Updated be the last.  So this is a legal entry:

Name: 		somebody
Date Updated: 	Wed Jul 12 1989

as is this:

Name: 		somebody
Email:	 	somebody@think.com
Date Updated: 	Wed Jul 12 1989

A nice style convention is to use semicolon to mark carriage returns in
long entries like addresses, in case you want to pretty-print the
rolodex (gary@think.com has a buggy rolodex->latex program in lucid 
lisp written, which will eventually be rewritten in emacs-lisp).  

For example:

Name: 		somebody
Work Address: 	10 Foo Street; New York, NY 11220
Date Updated: 	Wed Jul 12 1989

might pretty print as

Name: 		somebody
Work Address: 	10 Foo Street
              	New York, NY 11220
Date Updated: 	Wed Jul 12 1989
")

(defvar rolo-new-entry-string
  "Name:		
Work Phone:	
Home Phone:	
Company:	
Work Address:	
Home Address:	
Remarks:	
Email:		
Fax:		
Printing Info:	pocket
Date Updated:	
"
  "*String to insert for new rolodex entry.  Must not have blank lines.  
See rolo-mode.")

(defun rolo () "Edit user's rolodex file, defined by rolo-file-name 
(default ~/rolofile)."
  (interactive)
  (find-file-other-window rolo-file-name)
  (rolo-mode))

(if rolo-mode-map
    nil
  (setq rolo-mode-map (make-sparse-keymap))
  (define-key rolo-mode-map "\C-cn" 'rolo-new-entry)
  (define-key rolo-mode-map "\C-cu" 'rolo-update-entry)
  (define-key rolo-mode-map "\C-cs" 'rolo-sort)
  (define-key rolo-mode-map "\C-cf" 'rolo-find)
  (define-key rolo-mode-map "\C-cq" 'rolo-quit)
  (define-key rolo-mode-map "\^n" 'next-line) ; override tmc-next-line so 
  (define-key rolo-mode-map "\^p" 'previous-line) ; goal-column works 
  )

(define-abbrev-table 'rolo-mode-abbrev-table ())

;; Rolo mode is suitable only for specially formatted data.
(put 'rolo-mode 'mode-class 'special)

(defun rolo-mode ()
    "Turn on rolo-mode, a mode for manipulation of rolodex files.
Calls 'rolo-mode-hook' if it is defined.  These special commands are
enabled:  

C-C n	Insert new rolodex entry after current entry.
C-C s	Sort rolodex entries by name.
C-C f	Find an entry by name.
C-C u	Update the Date Updated field of current entry to today's date.

Other useful commands are:

M-[	Back one entry.
M-]	Forward one entry.

The rolodex file is defined by the variable rolo-file-name (default
~/rolofile).  It should contain entries that look like this:

Name:		somebody
Work Phone: 	(617) 555-1234
Home Phone: 	
Company: 	
Work Address: 	
Home Address: 	
Remarks: 	
Fax:		
Printing Info:	pocket
Date Updated: 	Wed Jul 12 1989

The only rules are that the Name field must be the first, and the Date
Updated be the last.  So this is a legal entry:

Name: 		somebody
Date Updated: 	Wed Jul 12 1989

as is this:

Name: 		somebody
Email:	 	somebody@think.com
Date Updated: 	Wed Jul 12 1989

A nice style convention is to use semicolon to mark carriage returns in
long entries like addresses, in case you want to pretty print the
rolodex (gary@think.com has a buggy rolodex->latex program in lucid 
lisp written, but it should be in emacs-lisp).  For example:

Name: 		somebody
Work Address: 	10 Foo Street; New York, NY 11220
Date Updated: 	Wed Jul 12 1989

might pretty print as

Name: 		somebody
Work Address: 	10 Foo Street
              	New York, NY 11220
Date Updated: 	Wed Jul 12 1989
"
  (interactive)
  (kill-all-local-variables)
  (rolo-mode-1)
  (rolo-variables)
  (run-hooks 'rolo-mode-hook))

(defun rolo-variables ()
  (make-local-variable 'goal-column)
  (setq goal-column 16))

(defun rolo-mode-1 ()
  (setq major-mode 'rolo-mode)
  (setq mode-name "Rolo")
  (use-local-map rolo-mode-map)
  (setq local-abbrev-table rolo-mode-abbrev-table)
  )

(defun rolo-new-entry (&optional arg) "Insert a new entry in a rolodex file.
If on Name: line, inserts before current entry.
Inserts after any comment lines (starting with ';').
Otherwise, inserts after current entry.
With ARG, insert ARG entries."
       (interactive "p")
       (or arg (setq arg 1))
       ;First, set up to position where new entry string begins
       (beginning-of-line 1)
       (while (looking-at "^;")		;skip comments
	 (forward-line 1))
       (if (looking-at "\nName:")	;blank line before entry
	   (forward-line 1))
       (or (looking-at "Name:")		;Now, either we're at Name: line
	   (progn			;or put us at end of current entry
	     (forward-paragraph 1)
	     (insert "\n")))

       (while (> arg 0)			;Insert arg entries, with date updated
	 (progn 
	   (beginning-of-line)
	   (insert rolo-new-entry-string)
	   (or (looking-at "\n")	;follow with blank line if none,
	       (eobp)			;or eob
	       (open-line 1))
	   (search-backward "Name:")
	   (end-of-line)
	   (rolo-update-entry)
	   (setq arg (1- arg)))))

(defun insert-date (and-time)
  "Insert today's date at point in the buffer.  If argument is non-null, use time too."
  ;; E.g., "Tue Sep 15 18:29:44 1987"
  (interactive "P")
  (let ((time (current-time-string)))
    (or and-time (setq time (concat (substring time 0 (string-match " *[0-9]+:.*" time))
				    (substring time (string-match " *[0-9]*$" time))
				    )))
    (insert time)))


;;;***********************************************
;;; Replace
;;;***********************************************

(defvar old-replace-match (symbol-function 'replace-match))
(defvar inside-replace-all nil)
(defvar replace-all-count)
(defvar replace-all-max nil)

(defun replace-match (newtext &optional fixedcase literal)
  (when inside-replace-all
    (when (and replace-all-max (>= replace-all-count replace-all-max))
      (throw 'replace-all-done nil))
    (incf replace-all-count)
    )
  (funcall old-replace-match newtext fixedcase literal)
  )

(defun replace-all-prompt ()
  (let* ((numtimes (and current-prefix-arg (prefix-numeric-value current-prefix-arg)))
	 thing replacement
	 (numtimes-prompt (if numtimes
			      (format "(%d) " numtimes)
			      ""))
	 )
    (setq thing (read-string (concat numtimes-prompt "Replace: ")))
    (setq replacement (read-string (concat numtimes-prompt "Replace " thing " with: ")))
    (list thing replacement numtimes)
    ))


(defun replace-all (from to &optional numtimes)
  "Like query recplace but replaces each occurance of FROM with TO 
with out asking. If given a numeric argument, they are only replaced that
many times.  Prints the number of replacements."
  (interactive (replace-all-prompt))
  (let ((saved-point (point))
	(inside-replace-all t)
	(replace-all-count 0)
	(replace-all-max numtimes)
	)
    (catch 'replace-all-done
      (perform-replace from to nil nil nil))
    (princ (if (= 1 replace-all-count) "1 replacement."
	       (format "%d replacements." replace-all-count)))
    (goto-char saved-point)
    ))

(defun rolo-update-entry () "Update 'date updated:' field in rolodex entry."
       (interactive)
       (save-excursion
	 (if (search-forward "Date Updated:")
	     (let ((start-date (point)))
	       (end-of-line)
	       (delete-region start-date (point))
	       (insert "	")
	       (insert-date nil)))))

(defun rolo-sort ()
  (interactive)
  (let ((fill-prefix nil)		;temporarily disable fill-prefix 
	(data-start nil))
    (beginning-of-buffer)
    (re-search-forward "Name:[ 	]")
    (beginning-of-line)
    (setq data-start (point))
    (forward-page)
    (sort-paragraphs nil data-start (point))))

(defun rolo-get-field-value (field)
  "Get value of FIELD from current rolo buffer entry"
       (interactive "sField: ")
       (forward-paragraph 1)
       (let ((end-entry (point)))
	 (backward-paragraph 1)
	 (re-search-forward (concat field ".*:[ 	]+") end-entry t)
	 (end-of-line 1)
	 (buffer-substring (match-end 0) (point))))

(defun rolo-expand-name-to-email ()
  "Turn name before point in current buffer into email address from rolo file."
  (interactive)
  (rolo-expand-name-to-field "[Ee]mail"))

(defun rolo-expand-name-to-field (field)
  "Expand name before point into a FIELD from rolo file from entry 
containing FIELD.  Error if name is not unique."
  (interactive "sField: ")
  (let (email-addr)
    (save-window-excursion
      (forward-word -1)
      (kill-word 1)
      (setq email-addr (and (rolo-find (car kill-ring))
			    (rolo-get-field-value field)))
      (bury-buffer (current-buffer)))
    (if email-addr
	(insert email-addr)
	(progn
	  (insert (car kill-ring))
	  (error (concat (car kill-ring)
			 " not found or not unique in rolo file.")))))
  ) ; defun rolo-expand-name-to-field

(defvar *rolo-last-search-name* nil "Name that was last searched for in rolodex.")

(defun rolo-find (name)
  "Find rolo entry matching NAME.  Returns nil if not found or not unique.
Marks entire entry."
  (interactive
    (let* ((prompt "Name to find in rolodex: ")
	   search-string)
      (if *rolo-last-search-name*
	  (setq prompt (concat prompt
			       "(default "
			       *rolo-last-search-name*
			       ") ")))
      (setq search-string (read-string prompt))
      (if (eq major-mode 'rolo-mode)
	  (if (equal search-string "")
	      (forward-paragraph 1)	;start with next entry
	      (goto-char (point-min))))	;restart file if new string
      (cond ((not (equal search-string ""))
	     (setq *rolo-last-search-name* search-string))
	    ((not *rolo-last-search-name*)
	     (error "No previous Rolo search string")))
      (list *rolo-last-search-name*)))

  (if (not (eq major-mode 'rolo-mode)) ;if not already in a rolo file,
      (rolo))			;go to rolo file

  (let ((found-it 
	 (re-search-forward		; Search only in Name: field
	   (concat "^Name:[ 	]+.*" name ".*$")
	   (point-max) t)))
    (if found-it
	(progn
	  ;; Make the window just tall enough for its contents.
	  (mark-paragraph)
	  (forward-line 1)
	  (recenter 0)
	  (narrow-to-region (point) (mark))
	  (let ((h (1- (window-height)))
		(l (count-lines (point-min) (point-max))))
	    (or (one-window-p t)
		(= h l)
		(if (< h l)
		    (enlarge-window (- l h))
		    (shrink-window (- h l))))
	    (widen)
	    )				; let
	  )				; progn
	(error (concat name " not found.")))
    found-it)
  )
	     
(defun rolo-quit () "Close the rolodex window, set point to top of rolodex 
buffer,  bury buffer in buffer list."
       (interactive)
       (let ((rolo-buf (current-buffer)))
	 (goto-char (point-min))
	 (shrink-window 999)
	 (bury-buffer rolo-buf)))

(defun rolo-kill-comments () "Kill all comment lines in rolodex buffer."
       (interactive)
       (save-excursion
	 (goto-char (point-min))	; kill all comment lines
	 (while (re-search-forward "^;" (point-max) t)
	   (beginning-of-line nil)
	   (kill-line 1))
	 (goto-char (point-min))
	 (while (re-search-forward "^" (point-max) t)
	   (beginning-of-line nil)
	   (kill-line 1))
	 ))

(defun rolo-extract-addresses (buf) 
  "Extract mailing addresses from current rolo buffer to BUFFER"
  (interactive "BOutput buffer name: ")
  (if (not (yes-or-no-p "This deletes the text in the current buffer; ok? "))
	   (error "Aborted."))
  (let (address-prefix other-prefix)
    (save-excursion			; create BUFFER
      (switch-to-buffer buf)
      (setq buffer-file-name buf))
    (rolo-kill-comments)
    (goto-char (point-min))
    (while (not (eobp))
      (mark-paragraph)			; select one entry
      (narrow-to-region (mark) (point))
      (save-excursion			; move all Name: lines
	(while (search-forward "Name:	" (point-max) t)
	  (beginning-of-line nil)
	  (kill-line 1)
	  (save-excursion
	    (set-buffer buf)
	    (goto-char (point-max))
	    (yank)
	    ))
	)				; save-excursion
      (save-excursion			; figure out which address
	(cond ((search-forward "Remarks:	" (point-max) t)
	       (if (search-forward "work" (point-max) t)
		   (setq address-prefix "Work Address:	"
			 other-prefix "Company:	")
		   (setq address-prefix "Home Address:	"
			 other-prefix nil)) ;else
	       ))			; cond
	)				; save-excursion
      (cond (other-prefix
	     (save-excursion		; move address
	       (cond ((search-forward other-prefix (point-max) t)
		      (progn
			(beginning-of-line nil)
			(kill-line 1)
			(set-buffer buf)
			(yank)
			(goto-char (point-max))
			)		; progn
		      ))		; cond
	       )			; save-excursion
	     ))				; cond
      (save-excursion			; move address
	(cond ((search-forward address-prefix (point-max) t)
	       (progn
		 (beginning-of-line nil)
		 (kill-line 1)
		 (set-buffer buf)
		 (yank)
		 (insert "\n")
		 (goto-char (point-max))
		 )			; progn
	       ))			; cond
	)				; save-excursion
      (delete-region (mark) (point))
      (widen)
      )					; while
    )					; let
  (switch-to-buffer buf)		; display results
  ) ; rolo-extract-addresses

(defun rolo-pretty-print-address ()
  "Pretty-print addresses in current rolodex buffer."
  (interactive)
  (let ((out-buf (concat (buffer-name) ".addresses")))
    (rolo-extract-addresses out-buf)
    (rolo-fix-name-order)
    (goto-char (point-min))
    (re-search-forward "^\\(Name:[	 ]*\\)")
    (let ((start-of-name (match-beginning 1)))
      (goto-char (point-max))
      (re-search-backward "\\(Address:[	 ]*\\)")
      (delete-rectangle start-of-name (match-end 1))
      )					; let
    (goto-char (point-min))
    (replace-all "; " "
")
    )					; let
  )

(defun rolo-fix-name-order ()
  "Fix the order of the last name in the Name: field of a rolodex file.
The last name must be followed by a comma and a space, followed by title,
first name, middle name."
  (goto-char (point-min))
  (while
      (re-search-forward
       "^Name:[ 	]*\\([a-zA-Z \.\-]*, \\).*$" (point-max) t)
    (progn
      (kill-region (match-beginning 1) (match-end 1)) ;kill last name
      (end-of-line nil)
      (just-one-space)
      (yank)
      (backward-delete-char 2)		; delete trailing comma and space
      )					; progn
    )					; while
  )
