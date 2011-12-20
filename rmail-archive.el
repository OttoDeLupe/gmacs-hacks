;;; $Header: /tmp_mnt/am/p7/utility/gmacs/f3/RCS/rmail-archive.el,v 1.7 89/04/11 11:47:14 gary Exp $

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
;;; 10/4/88
;;;

;; creon@nas.nasa.gov Sun Feb  6 20:19:22 PST 1994
;; merged with rmail-get-new-mail from emacs 19 release so that
;; it works with emacs 19.

;;I just completely rewrote this, the old version was slow --gary 3/8/89




;;this makes all new mail get written to a mail archive file.
;;
;; Just put (require 'rmail-archive) in your .emacs,
;; and setq mail-incoming-archive-file-name to the appropriate filename
;; -gary


;; Maybe this should replace the original definition of 
;; rmail-get-new-mail in rmail.el, since it won't archive unless
;; mail-incoming-archive-file-name is non-nil?

(provide 'rmail-archive)
(provide 'rmail-archive19)

(defvar mail-incoming-archive-file-name nil
  "* set this to the file where incoming mail should be archived, or
	nil if you don't want archiving.")


;;;; *** Rmail input ***

;; RLK feature not added in this version:
;; argument specifies inbox file or files in various ways.

(defun rmail-get-new-mail (&optional file-name)
  "Move any new mail from this RMAIL file's inbox files.
The inbox files can be specified with the file's Mail: option.  The
variable `rmail-primary-inbox-list' specifies the inboxes for your
primary RMAIL file if it has no Mail: option.  By default, this is
your /usr/spool/mail/$USER.

You can also specify the file to get new mail from.  In this case, the
file of new mail is not changed or deleted.  Noninteractively, you can
pass the inbox file name as an argument.  Interactively, a prefix
argument causes us to read a file name and use that file as the inbox."
  (interactive
   (list (if current-prefix-arg
	     (read-file-name "Get new mail from file: "))))
  (or (verify-visited-file-modtime (current-buffer))
      (progn
	(find-file (buffer-file-name))
	(setq buffer-read-only t)
	(if (verify-visited-file-modtime (current-buffer))
	    (rmail-forget-messages))))
  (rmail-maybe-set-message-counters)
  (widen)
  ;; Get rid of all undo records for this buffer.
  (or (eq buffer-undo-list t)
      (setq buffer-undo-list nil))
  ;;<<<>>> added1
  ;; kill buffer so when it gets UNIX appended to there is no version skew
  (if mail-incoming-archive-file-name
      (let ((buff (get-file-buffer mail-incoming-archive-file-name)))
	(when buff (kill-buffer buff))))
  ;;<<<>>> end of added code1
  (unwind-protect
      (let ((opoint (point))
	    (new-messages 0)
	    (delete-files ())
	    ;; If buffer has not changed yet, and has not been saved yet,
	    ;; don't replace the old backup file now.
	    (make-backup-files (and make-backup-files (buffer-modified-p)))
	    (buffer-read-only nil)
	    ;; Don't make undo records for what we do in getting mail.
	    (buffer-undo-list t))
	(goto-char (point-max))
	(skip-chars-backward " \t\n")	    ; just in case of brain damage
	(delete-region (point) (point-max)) ; caused by require-final-newline
	(save-excursion
	  (save-restriction
	    (narrow-to-region (point) (point))
	    ;; Read in the contents of the inbox files,
	    ;; renaming them as necessary,
	    ;; and adding to the list of files to delete eventually.
	    (if file-name
		(rmail-insert-inbox-text (list file-name) nil)
	      (setq delete-files (rmail-insert-inbox-text rmail-inbox-list t)))

	      ;;<<<>>> added2
	    (unless (or (= (point-min) (point-max))
			(null mail-incoming-archive-file-name))
	      (message "Fast RMAIL archiving to %s..." 
		       mail-incoming-archive-file-name)
	      (write-region (point-min) (point-max)
			    mail-incoming-archive-file-name
			    t)) ;;append
	      ;;<<<>>> end of added code2

	    ;; Scan the new text and convert each message to babyl format.
	    (goto-char (point-min))
	    (save-excursion
	      (setq new-messages (rmail-convert-to-babyl-format)))
	    (or (zerop new-messages)
		(let (success)
		  (widen)
;v19 change	  (search-backward "\n\^_")
		  (search-backward "\n\^_" nil t)
		  (narrow-to-region (point) (point-max))
		  (goto-char (1+ (point-min)))
		  (rmail-count-new-messages)
		  (save-buffer)))
	    ;; Delete the old files, now that babyl file is saved.
	    (while delete-files
	      (condition-case ()
		  ;; First, try deleting.
		  (condition-case ()
		      (delete-file (car delete-files))
		    (file-error
		     ;; If we can't delete it, truncate it.
		     (write-region (point) (point) (car delete-files))))
		(file-error nil))
	      (setq delete-files (cdr delete-files)))))
	(if (= new-messages 0)
	    (progn (goto-char opoint)
		   (if (or file-name rmail-inbox-list)
		       (message "(No new mail has arrived)")))
	  (if (rmail-summary-exists)
	      (rmail-select-summary
		(rmail-update-summary)))
	  (message "%d new message%s read"
		   new-messages (if (= 1 new-messages) "" "s"))
	  (and (boundp 'display-time-string)
;add for v19:
	       (stringp display-time-string)
	       (string-match " Mail" display-time-string)
	       (setq display-time-string
		     (concat
		      (substring display-time-string 0 (match-beginning 0))
		      (substring display-time-string (match-end 0))))
	       (force-mode-line-update 'all))))
    ;; Don't leave the buffer screwed up if we get a disk-full error.
    (rmail-show-message)))
