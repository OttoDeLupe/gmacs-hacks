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
;;; Tue Apr 17 1990
;;;
;;; Simple function to force byte-compile every .el file in current directory.
;;;
;;; fad@Think.com
;;; $Id: byte-compile-directory.el,v 1.4 1990/04/17 11:24:43 fad Exp $

(defun byte-compile-if-el (file)
  "Byte-compile FILE if name ends in .el and if .elc file is not newer."
  (if (string-match ".*\.el$" file)
      (if (file-newer-than-file-p file (concat file "c"))
	  (byte-compile-file file)
	  (message (concat file " OK.")))))

(defun byte-compile-directory ()
"Byte-compile every .el file in current directory unless .elc file is newer."
  (interactive)
  (mapcar 'byte-compile-if-el (directory-files "."))
  (message (concat "Done byte-compiling directory " (pwd) " !")))
