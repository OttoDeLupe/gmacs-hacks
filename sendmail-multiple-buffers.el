
(require 'sendmail)
(provide 'sendmail-multiple-buffers)


(defvar mail-buffer-name-format "*mail-%d*"
  "*Format string for names of mail buffers after the initial *mail*.
Will have one numeric parameter indicating the buffer number.")

(defun make-mail-buffer-name (index)
  (if (= index 1) "*mail*"
    (format  mail-buffer-name-format index)))

;;; Modified to uncomment the code that generates multiple mail buffers.

(defun mail (&optional noerase to subject in-reply-to cc replybuffer actions)
  "Edit a message to be sent.  Prefix arg means resume editing (don't erase).
When this function returns, the buffer `*mail*' is selected.
The value is t if the message was newly initialized; otherwise, nil.

By default, the signature file `~/.signature' is inserted at the end;
see the variable `mail-signature'.

\\<mail-mode-map>
While editing message, type \\[mail-send-and-exit] to send the message and exit.

Various special commands starting with C-c are available in sendmail mode
to move to message header fields:
\\{mail-mode-map}

If `mail-self-blind' is non-nil, a BCC to yourself is inserted
when the message is initialized.

If `mail-default-reply-to' is non-nil, it should be an address (a string);
a Reply-to: field with that address is inserted.

If `mail-archive-file-name' is non-nil, an FCC field with that file name
is inserted.

If `mail-setup-hook' is bound, its value is called with no arguments
after the message is initialized.  It can add more default fields.

When calling from a program, the second through fifth arguments
 TO, SUBJECT, IN-REPLY-TO and CC specify if non-nil
 the initial contents of those header fields.
 These arguments should not have final newlines.
The sixth argument REPLYBUFFER is a buffer whose contents
 should be yanked if the user types C-c C-y.
The seventh argument ACTIONS is a list of actions to take
 if/when the message is sent.  Each action looks like (FUNCTION . ARGS);
 when the message is sent, we apply FUNCTION to ARGS.
 This is how Rmail arranges to mark messages `answered'."
  (interactive "P")
  (let ((index 1)
	(initialized nil)
	buffer)
    ;; If requested, look for a mail buffer that is modified and go to it.
    (if noerase
	(progn
	  (while (and (setq buffer
			    (get-buffer (make-mail-buffer-name index)))
		      (not (buffer-modified-p buffer)))
	    (setq index (1+ index)))
	  (if buffer (switch-to-buffer buffer)
	    ;; If none exists, start a new message.
	    (setq index 1)
	    (setq noerase nil))))
    ;; Unless we found a modified message and are happy, start a new message.
    (if (not noerase)
	(progn
	  ;; Look for available mail buffer name
	  (while (get-buffer (make-mail-buffer-name index))
	    (setq index (1+ index)))
	  (setq buffer (get-buffer-create (make-mail-buffer-name index)))
	  ;; Go there and initialize it.
	  (switch-to-buffer buffer)
	  (erase-buffer)
          (setq default-directory (expand-file-name "~/"))
          (auto-save-mode auto-save-default)
          (mail-mode)
          (mail-setup to subject in-reply-to cc replybuffer actions)
	  (if (and buffer-auto-save-file-name
		   (file-exists-p buffer-auto-save-file-name))
	      (message "Auto save file for draft message exists; consider M-x mail-recover"))
	  (setq initialized t)))
;;;  (switch-to-buffer "*mail*")
;;;  (setq default-directory (expand-file-name "~/"))
;;;  (auto-save-mode auto-save-default)
;;;  (mail-mode)
;;;  (let (initialized)
;;;    (and (not noerase)
;;;	 (or (not (buffer-modified-p))
;;;	     (y-or-n-p "Unsent message being composed; erase it? "))
;;;	 (progn (erase-buffer)
;;;		(mail-setup to subject in-reply-to cc replybuffer actions)
;;;		(setq initialized t)))
;;;    (if (and buffer-auto-save-file-name
;;;	     (file-exists-p buffer-auto-save-file-name))
;;;	(message "Auto save file for draft message exists; consider M-x mail-recover"))
;;;    initialized)
    initialized))

(defun mail-other-window (&optional noerase to subject in-reply-to cc replybuffer sendactions)
  "Like `mail' command, but display mail buffer in another window."
  (interactive "P")
  (let ((pop-up-windows t)
	(temp-buffer nil))
    (unwind-protect
	(let ((temp-bufname " *mail-temp*"))
	  (setq temp-buffer (get-buffer-create temp-bufname))
	  (pop-to-buffer temp-bufname)
	  (mail noerase to subject in-reply-to cc replybuffer sendactions))
      (and temp-buffer
	   (kill-buffer temp-buffer)))))

(defun mail-other-frame (&optional noerase to subject in-reply-to cc replybuffer sendactions)
  "Like `mail' command, but display mail buffer in another frame."
  (interactive "P")
  (let ((pop-up-frames t)
	(temp-buffer nil))
    (unwind-protect
	(let ((temp-bufname " *mail-temp*"))
	  (setq temp-buffer (get-buffer-create temp-bufname))
	  (pop-to-buffer temp-bufname)
	  (mail noerase to subject in-reply-to cc replybuffer sendactions))
      (and temp-buffer
	   (kill-buffer temp-buffer)))))



;;; To kill multiple buffers matching regexp, such as all mail buffers

(defun kill-mail-buffers ()
  "Kill buffers that contain \"*mail\" in their name"
  (interactive)
  (kill-buffers-containing-name "\\*mail"))


(defun kill-matching-buffers ()
  "Accepts a regular expressiona and then kills all buffers with matching names"
  (interactive)
  (let ((regexp (read-string "Enter a REGEXP of buffers to kill: ")))
    (unless (string= regexp "")  
      ;; don't kill all buffers by mistake
      (kill-buffers-containing-name regexp))))

(defun kill-buffers-containing-name (name)
  "kill all buffers matching a regular expression"
  (dolist (buffer (buffer-list))
    (when (string-match name (buffer-name buffer))
      (kill-buffer buffer))))

