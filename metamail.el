;;; metamail.el --- Metamail interface for GNU Emacs

;; Copyright (C) 1993 Masanobu UMEDA

;; Author: Masanobu UMEDA <umerin@mse.kyutech.ac.jp>
;; Version: $Header: metamail.el,v 1.7 94/05/30 15:35:15 umerin Locked $
;; Keywords: mail, news, mime, multimedia

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; LCD Archive Entry:
;; metamail|Masanobu UMEDA|umerin@mse.kyutech.ac.jp|
;; Metamail interface for GNU Emacs|
;; $Date: 94/05/30 15:35:15 $|$Revision: 1.7 $|~/misc/metamail.el.Z|

;; Note: Metamail does not have all options which is compatible with
;; the environment variables.  For that reason, matamail.el have to
;; hack the environment variables.  In addition, there is no way to
;; display all header fields without extra informative body messages
;; which is suppressed by "-q" option.

;; The idea of using metamail to process MIME messages is from
;; gnus-mime.el by Spike <Spike@world.std.com>.

;;; Code:

(defvar metamail-program-name "metamail"
  "*Metamail program name.")

(defvar metamail-environment '("KEYHEADS=*" "MM_QUIET=1")
  "*Environment variables passed to `metamail'.
It must be a list of strings that have the format ENVVARNAME=VALUE.
It is not expected to be altered globally by `set' or `setq'.
Instead, change its value temporary using `let' or `let*' form.")

(defvar metamail-switches '("-m" "emacs" "-x" "-d" "-z")
  "*Switches for `metamail' program.
-z is required to remove zap file.
It is not expected to be altered globally by `set' or `setq'.
Instead, change its value temporary using `let' or `let*' form.")

;;;###autoload
(defun metamail-interpret-header ()
  "Interpret a header part of a MIME message in current buffer.
Its body part is not interpreted at all."
  (interactive)
  (save-excursion
    (let* ((buffer-read-only nil)
	   (metamail-switches		;Inhibit processing an empty body.
	    (append metamail-switches '("-c" "text/plain" "-E" "7bit")))
	   (end (progn
		  (goto-char (point-min))
		  (search-forward "\n\n" nil 'move)
		  ;; An extra newline is inserted by metamail if there
		  ;; is no body part.  So, insert a dummy body by
		  ;; itself.
		  (insert "\n")
		  (point))))
      (metamail-region (point-min) end nil 'nodisplay)
      ;; Remove an extra newline inserted by myself.
      (goto-char (point-min))
      (if (search-forward "\n\n\n" nil t)
	  (delete-char -1))
      )))

;;;###autoload
(defun metamail-interpret-body (&optional nodisplay)
  "Interpret a body part of a MIME message in current buffer.
Optional 1st argument NODISPLAY non-nil means buffer is not
redisplayed as output is inserted.
Its header part is not interpreted at all."
  (interactive)
  (save-excursion
    (let ((contype nil)
	  (encoding nil)
	  (end (progn
		 (goto-char (point-min))
		 (search-forward "\n\n" nil t)
		 (point))))
      ;; Find Content-Type and Content-Transfer-Encoding from the header.
      (save-restriction
	(narrow-to-region (point-min) end)
	(setq contype 
	      (or (mail-fetch-field "Content-Type") "text/plain"))
	(setq encoding 
	      (or (mail-fetch-field "Content-Transfer-Encoding") "7bit")))
      ;; Interpret the body part only.
      (let ((metamail-switches		;Process body part only.
	     (append metamail-switches
		     (list "-b" "-c" contype "-E" encoding))))
	(metamail-region end (point-max) nil nodisplay))
      )))

;;;###autoload
(defun metamail-buffer (&optional buffer nodisplay)
  "Process current buffer through `metamail'.
Optional 1st argument BUFFER specifies a buffer to be filled (nil
means current).
Optional 2nd argument NODISPLAY non-nil means buffer is not
redisplayed as output is inserted."
  (interactive)
  (metamail-region (point-min) (point-max) buffer nodisplay))

;;;###autoload
(defun metamail-region (beg end &optional buffer nodisplay)
  "Process current region through 'metamail'.
Optional 1st argument BUFFER specifies a buffer to be filled (nil
means current).
Optional 2nd argument NODISPLAY non-nil means buffer is not
redisplayed as output is inserted."
  (interactive "r")
  (let ((curbuf (current-buffer))
	(buffer-read-only nil)
	(metafile (make-temp-name "/tmp/metamail")))
    (save-excursion
      ;; Gee!  Metamail does not ouput to stdout if input comes from
      ;; stdin.
      (let ((selective-display nil)	;Disable ^M to nl translation.
	    (kanji-fileio-code 2)	;Write in JIS code when nemacs.
	    (file-coding-system		;Write in JUNET style when mule.
	     (if (featurep 'mule) *junet*)))
	(write-region beg end metafile nil 'nomessage))
      (if buffer
	  (set-buffer buffer))
      (setq buffer-read-only nil)
      ;; Clear destination buffer.
      (if (eq curbuf (current-buffer))
	  (delete-region beg end)
	(delete-region (point-min) (point-max)))
      ;; We have to pass the environment variable KEYHEADS to display
      ;; all header fields.  Metamail should have an optional argument
      ;; to pass such information directly.
      (let ((process-environment
	     (append process-environment metamail-environment)))
	;; Specify character coding system.
	(if (boundp 'NEMACS)
	    (define-program-kanji-code nil metamail-program-name 2)) ;JIS
	(if (featurep 'mule)
	    (define-program-coding-system nil metamail-program-name *junet*))
	(apply (function call-process)
	       metamail-program-name
	       nil
	       t			;Output to current buffer
	       (not nodisplay)		;Force redisplay
	       (append metamail-switches (list metafile))))
      ;; `metamail' may not delete the temporary file!
      (condition-case error
	  (delete-file metafile)
	(error nil))
      )))

;(defun metamail-region (beg end &optional buffer)
;  "Process current region through 'metamail'.
;Optional argument BUFFER specifies a buffer to be filled (nil means current)."
;  (interactive "r")
;  (let ((curbuf (current-buffer))
;	(buffer-read-only nil)
;	(metafile (make-temp-name "/tmp/metamail")))
;    (save-excursion
;      (write-region (point-min) (point-max) metafile nil 'nomessage)
;      (if (eq curbuf
;	      (if buffer (get-buffer buffer) (current-buffer)))
;	  (delete-region (point-min) (point-max)))
;      (apply (function call-process)
;	     metamail-program-name
;	     nil
;	     (or buffer t)		;BUFFER or current buffer
;	     nil			;don't redisplay
;	     (append metamail-switches (list metafile)))
;      )))

(provide 'metamail)

;;; metamail.el ends here
