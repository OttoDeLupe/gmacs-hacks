;; View a file with "live" updates as the file grows on disk.
;; Copyright (C) 1991 Dave Gillespie

;; This file is part of GNU Emacs.

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

;; Usage:
;;   (autoload 'live-find-file "live" "View a file with \"tail -f\"" t)
;;   M-x live-find-file RET filename RET

(provide 'live)

(defvar live-file-notify t
  "*Non-nil to display a message in the echo area if data comes in on a
live file buffer which is not currently displayed.
If Non-nil and not \"t\", actually display the buffer if new data comes in.")

(defvar live-file-mask (concat "^\000\001\002\003\004\005\006\007\010\011"
			       "\013\015\016\017\020\021\022\023\024\025"
			       "\026\027\030\031\032\033\034\035\036\037\177")
  "*A string of characters to retain in a live file buffer;
all others are discarded.  If it begins with \"^\", it is a list of
characters which *should* be discarded.
Default is to discard all control characters except new-line and form-feed.")



(defun live-find-file (filename)
  "Read in and display a file with \"live\" updates as the file grows on disk.
This is accomplished using the Unix \"tail -f\" command.
The buffer will remain live until you kill it with \\[kill-buffer].
The buffer is made read-only, and should probably be left that way.
Inside the buffer, hit n to change notification mode, r to re-read, q to kill."
  (interactive "fLive find file: ")
  (setq filename (expand-file-name filename))
  (if (file-directory-p filename)
      (error "%s is a directory." filename))
  (let ((buf (create-file-buffer filename))
	size)
    (switch-to-buffer buf)
    (fundamental-mode)
    (setq major-mode 'live-find-file)
    (setq mode-name "Live-Find-File")
    (use-local-map (make-sparse-keymap))   ; should set up a real major mode!
;    (local-set-key "q" "\C-xk\r")
;gary modification
    (local-set-key "q" 'kill-this-buffer)
    (local-set-key "n" 'Live-file-notify)
    (local-set-key "r" 'Live-file-reread)
    (local-set-key " " 'scroll-up)
    (local-set-key "\177" 'scroll-down)
    (local-set-key "?" 'Live-file-help)
    (local-set-key "h" 'Live-file-help)
    (erase-buffer)
    (insert-file-contents filename t)
    (setq size (buffer-size))
    (live-file-clean-region (point-min) (point-max))
    (set-buffer-modified-p nil)
    (setq buffer-read-only t)
    (goto-char (point-max))
    (setq default-directory (file-name-directory filename))
    (let* ((process-connection-type nil)
	   (proc (start-process
		  "tail" (current-buffer) "tail"
		  (format "+%dcf" (1+ size))
		  filename)))
      (process-kill-without-query proc)
      (set-process-filter proc 'live-file-filter)))
)

; takes no args, which is what is needed
(defun kill-this-buffer ()
  (interactive)
  (kill-buffer nil))


(defun Live-file-help ()
  (interactive)
  (message "Live mode:  n=change notification method, q=quit, r=reread")
)

(defun Live-file-notify (flag)
  "Control whether to notify on input from live files not visible in a window.
This applies to files created by \\[live-find-file].
Cycles among three options:  Notify in echo area, notify by creating a new
window, or don't notify.
Notification mode applies if input arrives for a live file that is not
currently visible.  If input arrives for a live file that is visible but
that has been scrolled so that the end is not visible, new input always
notifies in the echo area."
  (interactive "P")
  (setq live-file-notify
	(cond ((null flag)
	       (or (not live-file-notify)
		   (and (eq live-file-notify t)
			'show)))
	      ((not (integerp flag)) 'show)
	      ((> flag 0) t)))
  (message (cond ((eq live-file-notify t) "Notifying in echo area.")
		 (live-file-notify "Notifying in a new window.")
		 (t "Not notifying.")))
)

(defun Live-file-reread ()
  "Re-read the contents of the current live file from disk.
Live-find-file only checks for new text being appended at the end.  This
command re-reads the whole of the file to see if any of it has changed."
  (interactive)
  (let ((name (buffer-file-name)))
    (or name
	(error "Buffer is not visiting a file."))
    (or (file-readable-p name)
	(error "File %s no longer exists." name))
    (message "Re-reading %s..." name)
    (kill-buffer (current-buffer))
    (live-find-file name)
    (message "Re-reading %s...done" name))
)

(defun live-file-filter (proc str)
  (let* ((oldbuf (current-buffer))
	 (buf (process-buffer proc))
	 (win (get-buffer-window buf))
	 (changed nil))
    (unwind-protect
	(progn
	  (set-buffer buf)
	  (clear-visited-file-modtime)
	  (let ((pt (point-max))
		(opt (and (< (point) (point-max)) (point)))
		(mod (buffer-modified-p)))
	    (goto-char pt)
	    (let ((buffer-read-only nil))
	      (insert str)
	      (live-file-clean-region pt (point-max)))
	    (setq changed (/= pt (point-max)))
	    (and changed
		 (or (not win)
		     (not (pos-visible-in-window-p pt win)))
		 (or win (eq live-file-notify t))
		 (= (minibuffer-depth) 0)
		 (message "From %s: %s"
			  (file-name-nondirectory
			   (buffer-file-name buf))
			  str))
	    (or mod
		(set-buffer-modified-p nil))
	    (or opt (setq opt (point-max)))
	    (goto-char opt)
	    (and win (set-window-point win opt))))
      (set-buffer oldbuf))
    (and changed
	 live-file-notify 
	 (not (eq live-file-notify t))
	 (not (get-buffer-window buf))
	 (set-window-point (display-buffer buf t)
			   (save-excursion (set-buffer buf)
					   (goto-char (point-max))
					   (point)))))
)

(defun live-file-clean-region (start end)
  (and live-file-mask
       (save-excursion
	 (goto-char end)
	 (while (progn
		  (skip-chars-backward live-file-mask start)
		  (> (point) start))
	   (delete-backward-char 1))))
)



;use find-file-from-buffer instead
;; new stuff added by gary for reading files whose names are at the point
;(defun edit-file (filename &optional other-window)
;  "edit a pointed to file.  If filename is a null string, the filename
;in the buffer around or before point is suggeded for use as the 
;filename in a prompt."
;  (interactive (list (get-pointed-filename "" 
;					   "Find File on file: " 
;					   (not current-prefix-arg))))
;  (let* ((expanded-filename
;	  (expand-file-name filename)))
;    (find-file filename)))



; make tail an alias
(define-function 'tail 'live-find-file)

;(defun tail (filename &optional other-window)
;  "tail of a file.  If filename is a null string, the filename
;in the buffer around or before point is suggeded for use as the 
;filename in a prompt."
;  (interactive (list (get-pointed-filename "" 
;					   "Tail of file: " 
;					   (not current-prefix-arg))))
;  (let ((expanded-filename
;	  (expand-file-name filename)))
;    (live-find-file expanded-filename)))


(defun get-pointed-filename (filename prompt-prefix interactivively)
  (when (equal filename "")
    (let ((default (snarf-filename-default)))
      (cond ((or (equal default "") interactivively)
	     (let (prompt input-file-name)
	       (setq prompt (concat prompt-prefix "(Default: \"" default "\") "))
	       (setq input-file-name (read-string prompt))
	       (setq filename
		     (if (equal input-file-name "")
			 default
			 input-file-name))))
	    (t (setq filename default)))))
  filename)


(defvar file-name-char-class "a-zA-Z0-9./_:\-~")

(defun snarf-filename-default ()
  (interactive)
  (let* ((start)
	 (end)
	 (filename ""))
    (save-excursion
      (if (not (looking-at file-name-char-class))
	  ;; Move backward for filename if not already on one
	  (re-search-backward (concat "[" file-name-char-class "]")
			      (point-min)
			      'stay)) 
      ;; Move to start of filename
      (re-search-backward (concat "[^" file-name-char-class "]")
			  (point-min)
			  'stay)
      ;; Find start and end of filename
      (when (re-search-forward (concat "\\([" file-name-char-class "]\\)+")
			       nil 
			       t)
	(setq start (match-beginning 0)
	      end (match-end 0)
	      filename (buffer-substring start end))
	(unless (equal (elt filename 0) ?\/)
	  (setq filename (concat default-directory filename)))
	)
      filename)))

(defun backward-filename ()
  (backward-sexp))

(defun forward-filename ()
  (forward-sexp))
