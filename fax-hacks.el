;; gary hacks for dealing with flexfax

(defvar fax-mail-buffer-header 
  (format "From: %s (%s)\n" (user-login-name) (user-full-name))
  "*Default header that goes on top of fax-mail-buffer message")


(defvar fax-spool-dir "/usr/spool/fax/"
  "*Directory, normally /usr/spool/fax/")



(defun view-fax ()
  "*View a fax via ghostview.  Call this function when viewing RMAIL message 
from Flexfax about an incoming fax."

  (interactive)
  (let ((file (fax-msg-get-name)))
    (cond ((file-exists-p file)
	   (message (format "calling viewfax on %s" file))
	   (call-process "/usr/local/lib/fax/viewfax" nil 0 nil file))
	  (t
	   (beep)
	   (message (format "File %s does not exist!" file))))))

(defun delete-fax ()
  "*Delete a fax.  Call this function when viewing RMAIL message 
from Flexfax about an incoming fax.  Both the fax and the message are deleted."
  (interactive)
  (let ((file (fax-msg-get-name)))
    (cond ((file-exists-p file)
	   (message (format "deleting %s" file))
	   (delete-file file)
	   (rmail-delete-forward)
	   )
	  (t 
	   (beep)
	   (message (format "File %s does not exist!" file))))))

(defun lpr-fax ()
  "*Send the fax to printer.  Call this function when viewing RMAIL message 
from Flexfax about an incoming fax."
  (interactive)
  (let ((file (fax-msg-get-name)))
    (cond ((file-exists-p file)
	   (message (format "calling fax2ps | lpr on %s" file))
	   (call-process "/usr/local/lib/fax/lprfax" nil 0 nil file))
	  (t
	   (beep)
	   (message (format "File %s does not exist!" file))))))


(defun rename-fax (name)
  "*Change filename of fax.  Call this function when viewing RMAIL message 
from Flexfax about an incoming fax."
  (interactive (list 
		(read-file-name "New filename: " (concat fax-spool-dir "recvq/"))))
  (save-excursion
    (beginning-of-buffer)
    (search-forward "recvq")
    (forward-char)
    (let ((start (point)))
      (end-of-line)
;; -1 skips colon
      (let ((oldname (buffer-substring start (- (point) 1)))
	    (newname (substring name (+ (length "recvq/")
					(string-match "recvq/" name))))
	    (path (concat fax-spool-dir "recvq/")))

;; rename the file
	(rename-file (concat path oldname)
		     (concat path newname))

;; rename file name in message
	(rmail-edit-current-message)
	(beginning-of-buffer)      
	(replace-string (concat "recvq/" oldname) (concat "recvq/"newname))

;; stick new name in the subject line, in brackets
	(beginning-of-buffer)
	(search-forward "Subject: ")
	(end-of-line)
	(let ((stop (point)))
	  (beginning-of-line)
	  (let ((already-here (re-search-forward "\\[.*\\]" stop 'move)))
	    (cond (already-here
		   (replace-match (concat "[" newname "]")))
		  (t (insert (concat " [" newname "]"))))))
	(rmail-cease-edit)
	(message "Renamed %s to %s" oldname newname)))))


;; utility function for snarfing name from flexfax message
(defun fax-msg-get-name ()    
  (beginning-of-buffer)
  (search-forward "recvq")
  (beginning-of-line)
  (let ((start (point)))
    (end-of-line)
    (let* ((name (buffer-substring start (- (point) 1))); don't get colon
	   (fullname (concat fax-spool-dir name)))
      fullname)))



(defun insert-fax-header ()
  (interactive)
  (unless (search-backward "Date: " 1 t)
    (goto-char (point-min))
    (search-forward "Subject: " 1 t)
    (next-line 1)
    (insert "Date: " (mail-rfc822-date) "\n"))

  (goto-char (point-min))
  (search-forward mail-header-separator)
  (unless (search-backward "From: " 1 t)
    (goto-char (point-min))
    (search-forward "From: " 1 t)
    (next-line 1)
    ;;new long from line
    (insert fax-mail-buffer-header)
    )
)


;; if when is supplied, it is the time argument.   I've never used that, should
;; get rid of it.
(defun fax-mail-buffer (when)
  "*Send a fax.  Call this function when viewing *mail* buffer, with 
the To: field containing a !!!!SINGLE!!!! dest@phonenumber.fax.  Note that
other destinations like cc: are ignored; you must send separately to each
recipient"
  (interactive "P")
  (save-excursion

    ;; insertions may move the position of the header, so 
    ;; keep re-searching for it rather than just storing away its position
    (goto-char (point-min))
    (search-forward mail-header-separator)

    (let ((delayed-fax (not (listp when))))
      (setq when 
	    (if delayed-fax
		(format "%d" when)
	        (setq when "now")))

      (insert-fax-header)

      (goto-char (point-min))
      (search-forward mail-header-separator)
      (search-backward "To: ")
      (forward-char 4)
      (let ((dest-start (point)))
	(end-of-line)
	(search-backward ".fax")
	(let* ((dest (buffer-substring dest-start (point)))
	       (at-sign (position 64 dest)))
	  (when (position 34 dest :start at-sign)
	    (error "There is a quote in the phone number!"))

	  ;now temporarily get rid of separator
	  (goto-char (point-min))
	  (re-search-forward
	    (concat "^" (regexp-quote mail-header-separator) "\n"))
	  (replace-match "\n")

	  (let ((fax-send-function "/usr/local/lib/fax/mailsendfax")
		(archive-p t)
		(header-point (point))
		(dest-post-at (substring dest (string-match "@" dest))))

	    (when (or (string-equal dest-post-at "@preview")
		      (string-equal dest-post-at "@ghostview"))
	      (setq fax-send-function "/usr/local/lib/fax/mailpreviewfax")
	      (setq archive-p nil))

	    (apply 'call-process-region
		   (append (list (point-min) (point-max)
				 fax-send-function
				 nil 0 nil
				 dest
				 when)))

	    ;; put the separator back
	    (goto-char header-point)
	    (next-line -1)
	    (insert mail-header-separator)

	  ;;archive it if necessary
	    (when archive-p
	      (goto-char (point-min))
	      (search-forward mail-header-separator)
	      (when (search-backward "FCC: " 1 t)
		(let ((name-start (+ 5 (point))))
		  (end-of-line nil)
		  (let ((file (buffer-substring name-start (point))))
		    (unless (or (= (point-min) (point-max))
				(null file))
		      (message "Fast Fax archiving to %s..." 
			       file)
		      (save-excursion
			(switch-to-buffer "*fax-header*")
			(erase-buffer)
			(insert (format "From %s (via fax-mail-buffer)\n" (user-login-name)))
			(write-region (point-min) (point-max)
				      file
				      t)) ;;append header
		      (write-region (point-min) (point-max)
				    file
				    t) ;;append fax
		      (save-excursion
			(switch-to-buffer "*fax-header*")
			(erase-buffer)
			(insert (format "\n\n"))
			(write-region (point-min) (point-max)
				      file
				      t) ;;append 2 CRs
			(kill-buffer nil)
			))))))
	    ;; if sent, mark it saved like mail-send does
	    (if (not buffer-file-name)
		(progn
		  (set-buffer-modified-p nil)
		  (delete-auto-save-file-if-necessary t))))

	  (message (format "Mail buffer faxed to %s" dest))
	  )))))



(require 'sendmail)

;(unadvise 'mail-send)

(advise mail-send :around
	(save-excursion
	  (goto-char (point-min))
	  (search-forward mail-header-separator)
	  (search-backward "To: ")
	  (end-of-line)
	  (let ((fax-it (re-search-backward "@.*\\.fax" 1 t)))
	    (cond ((and (not fax-it)
			(re-search-backward "@[-0-9][-0-9][-0-9]" 1 t))
		   (when (y-or-n-p "Did you forget to include .fax in the name? ")
		     (end-of-line)
		     (insert ".fax")
		     (setq fax-it t))))
	    (cond (fax-it
;		   (message "fax it")
		   (fax-mail-buffer nil)
		   )
		  (t 
;		   (message "mail it")
		   :do-it
		   )))))


;(error "Try M-x fax-mail-buffer instead")
