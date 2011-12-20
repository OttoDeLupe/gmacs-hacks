;; This file contains a utility for automatically deleting messages that
;; that you are not interested in, based on header information such as the 
;; sender and the subject

;; Author: Gary Sabot

;; TODO:
;; 0.  Make it not mark unseen messages as seen just because they have been scanned!
;; 0.5 when "g" is invoked with a summary window and rmail window, you end up with
;;     two rmail windows, fix it.
;; 1.  rmail-autokill-scan needs to be bound to a key and read/default its args
;; 2.  automatically timestamp and age the autokill-specs, check when reading in 
;;     file if they need updating
;; 3.  auto merge if file changes underneath
;; 4.  Eliminate trailing newlines and whitespace from templates?
;; 5.  Eliminate Date from header provided to user?

;; changed:
;; 2/91 Brewster changed the typeout message to a "message" message.

;(require 'lisp-hacks)  ; for make-string-for-cl to quote quotes/backslashes
(provide 'rmail-autokill)


(defun make-string-for-cl (string)
  "This takes a string that is going to be passed to CL and doubles 
all backslashes, then turns all quotes inside the string into 
backslash quote."
  (backslash-string-quotes-for-cl (double-backslashes-for-cl string)))

(defun backslash-string-quotes-for-cl (string)
  "turns all quotes inside the string into backslash quote."
  (do* ((start-position 0 (+ position-of-quote 2))
	(position-of-quote (string-match "\\\"" string) (string-match "\\\"" string start-position)))
      ((null position-of-quote) string)
    (setq string (concat (substring string 0 position-of-quote)
			 "\\"
			 (substring string position-of-quote)))))

(defun double-backslashes-for-cl (string)
  "doubles all backslashes in string"
  (do* ((start-position 0 (+ position-of-slash 2))
	(position-of-slash (string-match "\\\\" string) (string-match "\\\\" string start-position)))
      ((null position-of-slash) string)
    (setq string (concat (substring string 0 position-of-slash)
			 "\\"
			 (substring string position-of-slash)))))



;; user variables
(defvar rmail-autokill-on t
  "* automatically kill messages that are on rmail-autokill-specs
whenever new mail is read in")

(defvar rmail-autokill-duplicates t
  "* automatically kill messages that duplicate earlier messages.
Only effective if rmail-autokill-on is true.")

(defvar rmail-autokill-specs-file "~/.rmail-autokill-specs-file"
  "*name of file where rmail-autokill-specs information is stored")

(defvar rmail-autokill-specs nil
  "*list of regular expressions, kill headers that contain them.")


;; internal variables
(defvar rmail-autokill-initialized nil)

(defun rmail-get-header ()
  (rmail-widen-to-current-msgbeg
   '(lambda ()
      (goto-char (point-min))
      (forward-line 2) ;; Skip over ^L and labels
      (let ((start (point))
	    (header-end (re-search-forward "\n\\(*** EOOH ***\\)?\n"))
	    (end (match-beginning 0))
	    )
	(buffer-substring start end)))))

(defun rmail-get-msg-id ()
  (rmail-widen-to-current-msgbeg
   '(lambda ()
      (goto-char (point-min))
      (re-search-forward "\n\n")
      (let ((end-of-header (match-beginning 0)))
	(goto-char (point-min))
	(when (re-search-forward "^Message-id:.*<.+>.*$" end-of-header)
	  (buffer-substring (match-beginning 0) (match-end 0)))))))

; still buggy
(defun rmail-get-header-no-date ()
  (save-excursion
    (goto-char (point-min))
    (let* ((start (point))
	   (header-end (re-search-forward "\n\n"))
	   (end (match-beginning 0))
	   (datep (re-search-forward "^Date:" nil t))
	   (date-start (point)))
      (forward-line 1)
      (let ((date-end (point)))
	(concat (buffer-substring start date-start)
		(buffer-substring date-end end))))))


;; this version does not call the hook, so MIME doest get displayed
;; Fixed to handle unbound rmail-show-message-hook correctly
(defun rmail-show-message-autokill-internal (n)
  (setq rmail-show-message-hook-boundp (boundp 'rmail-show-message-hook))
  (when rmail-show-message-hook-boundp
    (setq save-rmail-show-message-hook rmail-show-message-hook)
    (setq rmail-show-message-hook nil))
  (rmail-show-message n)
  (when rmail-show-message-hook-boundp
    (setq rmail-show-message-hook save-rmail-show-message-hook))
  )


(defun rmail-goto-message (n &optional include-header-p)
  "A short version copied from rmail-show-message"
  (let ((beg (rmail-msgbeg n))
	(end (rmail-msgend n))
	)
    (widen)
    (goto-char beg)
    (unless include-header-p
      (search-forward "\n*** EOOH ***\n" end t))
    (narrow-to-region (point) end))
  )




(defun rmail-autokill-load-specs ()
  "Load rmail-autokill-specs and aux info from rmail-autokill-specs-file"
  (interactive)
  (if (file-exists-p rmail-autokill-specs-file)
      (load rmail-autokill-specs-file)
      (rmail-autokill-save-specs)))  ; create file, it will contain nil

(defun rmail-autokill-save-specs ()
  "Save rmail-autokill-specs and aux info to rmail-autokill-specs-file"
  (interactive)
  (save-excursion
    (rmail-autokill-print-specs nil)
    (save-buffer)
    (bury-buffer)))

(defun rmail-autokill-print-specs (new-spec)
  (find-file rmail-autokill-specs-file)
  (erase-buffer)
  (insert "(setq rmail-autokill-specs '(\n")
  (when new-spec (rmail-print-autokill-spec new-spec))
  (mapcar 'rmail-print-autokill-spec rmail-autokill-specs)
  (insert "\n ))\n\n"))



(defun rmail-print-autokill-spec (subject)
  "insert a string into the rmail-autokill-specs-file"
  (insert (format "\n\"%s\"\n" (make-string-for-cl subject))))


;; this puts it in a typeout window
;;  (defun rmail-autokill-display-killings (text)
;;    (save-window-excursion
;;      (split-window nil nil)
;;      (let ((old-buffer (current-buffer))
;;	    (typeout-window-buffer (get-buffer-create "*rmail-autokill-output*")))
;;	(switch-to-buffer typeout-window-buffer)
;;	(let ((buffer-read-only nil))
;;	  (delete-region (point-min) (point-max))
;;	  (insert text)
;;	  (shrink-window-if-larger-than-buffer (selected-window)))
;;	(setq buffer-read-only t)
;;	(view-mode)
;;	(bury-buffer typeout-window-buffer)
;;	)))

(defun rmail-autokill-display-killings (text)
  (message text))


(defun rmail-autokill-scan (start length)
  "kill all specified range that match elements on rmail-autokill-specs"
  (interactive "nStart autokill scan at message number:\nnNumber of messages to autokill scan:")
  (cond ((eql major-mode 'rmail-mode)
	 (rmail-maybe-set-message-counters)
	 (let* ((cur-msg rmail-current-message)
		(deleted-msgs
		 (rmail-autokill-internal 
		  start
		  length)))
	   (rmail-show-message-autokill-internal cur-msg)
	   (when deleted-msgs
	     (if (memq cur-msg deleted-msgs)
		 (rmail-next-undeleted-message 1))
	     (rmail-autokill-display-killings
	       (if (= 1  (length deleted-msgs))
		   (format "%d message was automatically deleted: %s" 
		      (length deleted-msgs) deleted-msgs)
		   (format "%d messages were automatically deleted: %s" 
		      (length deleted-msgs) deleted-msgs))))
	   ))
	(t
	 (beep)
	 (message "You are not in an RMAIL buffer!"))))



(defun rmail-autokill-forward ()
  (interactive)
  (cond ((eql major-mode 'rmail-mode)
	 (rmail-maybe-set-message-counters)
	 (rmail-autokill-scan 
	  rmail-current-message
	  (+ 1 (- rmail-total-messages rmail-current-message))))
	(t
	 (beep)
	 (message "You are not in an RMAIL buffer!"))))
	


(defun rmail-autokill-internal (start length)
  "scan LENGTH messages starting at start, and delete if subject is on rmail-autokill-specs"
  (let ((to-delete nil)
	(specs rmail-autokill-specs))
    (dotimes (n length)
      (let ((msg (+ start n)))
	(unless (rmail-message-deleted-p msg)
	  (rmail-select-message msg)
	  (let ((header (rmail-get-header)))
	    (if (rmail-autokillable header specs)
		(push msg to-delete)
	      (when rmail-autokill-duplicates
		(let ((msg-id (ignore-errors (rmail-get-msg-id))))
		  (when msg-id
		    (push (regexp-quote msg-id) specs)))))))))
    (rmail-delete-message-list to-delete)
    to-delete))

(defun rmail-select-message (n)
  "Narrow the buffer to message number N, but don't do interactive processing."
  (rmail-maybe-set-message-counters)
  (widen)
  (if (zerop rmail-total-messages)
      (progn (narrow-to-region (point-min) (1- (point-max)))
	     (goto-char (point-min))
	     (setq mode-line-process nil))
    (setq rmail-current-message n))
  (let ((beg (rmail-msgbeg n))
	(end (rmail-msgend n)))
    (goto-char beg)
    (forward-line 1)
    (if (= (following-char) ?0)
	;; Don't reformat, since that prevents later marking seen.
	(forward-line 1)
      (search-forward "\n*** EOOH ***\n" end t))
    (narrow-to-region (point) end)))



(defun rmail-delete-message-list (messages)
  "delete all rmail messages in messages"
  (mapcar 'rmail-delete-message-by-number messages))


(defun rmail-delete-message-by-number (number)
  "delete rmail message by number"
  (rmail-select-message number)
  (rmail-delete-message))

(defun rmail-autokillable (header kill-specs)
  "is header on the kill-specs list"
  (dolist (spec kill-specs)
    (when (string-match spec header)
      (return t))))

(defvar autokill-separator "---kill file follow this line---")



(defun rmail-autokill-current-msg ()
  (interactive)
  (save-window-excursion
    (let* ((text (regexp-quote (rmail-get-header)))
	   (old-buffer (current-buffer)))
      (rmail-autokill-print-specs text)
      (switch-to-buffer (get-file-buffer rmail-autokill-specs-file))
      (goto-char (point-min))
      (insert "Please edit the regular expressions in this kill file.
They will be matched against mail headers.  

Type ESC C-c when done editing, or C-] to abort.\n\n")
      (insert autokill-separator)
      (insert "\n")
      (re-search-forward "\"")
      (recursive-edit)
      (goto-char (point-min))
      (re-search-forward (concat "^" (regexp-quote autokill-separator) "\n"))
      (delete-region (point-min) (point))
;; if there are errors, this will catch them
      (let ((marker (cons "---Delete Me---" nil))
	    (old-specs rmail-autokill-specs))
	(setq rmail-autokill-specs marker)
	(eval-region (point-min) (point-max))
	(cond ((eq marker rmail-autokill-specs)
	       (setq rmail-autokill-specs old-specs)
	       (rmail-autokill-save-specs))
	      (t (save-buffer))))
      (bury-buffer))))






;; initializations:


;; run filter whenever new mail is read
(advise rmail-get-new-mail :around
	(cond ((and rmail-autokill-on rmail-autokill-specs)
	       (rmail-maybe-set-message-counters)
	       (let ((old-total rmail-total-messages))
		 :do-it
		 (rmail-maybe-set-message-counters)
		 (unless (= rmail-total-messages old-total)
		   (rmail-autokill-forward))))
	      (t :do-it)))


(unless rmail-autokill-initialized
  (setq rmail-autokill-initialized t)
  (rmail-autokill-load-specs))


(defun rmail-autokill-D-key ()
  "Builds an autokill template from the current RMAIL message,
then kills all messages that match from that message on until 
the end of the RMAIL buffer."
  (interactive)
  (rmail-autokill-current-msg)
  (rmail-autokill-forward))

(defun rmail-autokill-K-key ()
  "Kills messages from the current message forward"
  (interactive)
  (rmail-autokill-forward))

;; rmail-autokill-scan is not on a key; it takes a range and I don't know how
;; to get it its arguments

(add-hook 'rmail-mode-hook
	  (lambda ()
	    (define-key rmail-mode-map "D" 'rmail-autokill-D-key)
	    (define-key rmail-mode-map "K" 'rmail-autokill-K-key)
	    ))


