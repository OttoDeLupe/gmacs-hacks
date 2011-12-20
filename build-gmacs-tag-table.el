;;; $Id: build-gmacs-tag-table.el,v 1.2 1989/03/30 17:53:27 salem Exp $

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


;;this is used to build the gmacs tags table.
;; -brewster

(defun build-gmacs-tag-table ()
  (interactive)
  (shell-command
    (format "cd /tmp
etags %s*.el
mv TAGS TAGS1
etags %s*.el
mv TAGS TAGS2
etags %s*.c
cat TAGS2 TAGS1 TAGS > %s
rm TAGS1 TAGS TAGS2
"
	    gmacs-utility-directory	; TAGS1
	    gmacs-lisp-directory	; TAGS2
	    gmacs-src-directory		; TAGS3
	    (concat gmacs-utility-directory ; destination
		    default-tags-file-name)
	    ))
  )

