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
;;; this file specifies SITE SPECIFIC information, in particular, the load paths.
;;; notice the setq load-path at the end.
;;;
;;; Jonathan@Think.com
;;; $Id: site.el,v 1.24 1993/07/14 21:42:09 barmar Exp $



;;the distribution sources were moved to /usr/local/src/utility/gmacs/dist so
;;that rdist would move these to the backup server as well.  This is part of the
;;new fault tolerance approach using NFS automounter with redundant
;;filesystems. 
;; -brewster and shn

(defun first-available-dir (&rest dirs)
  (if (or (null (cdr dirs))
	  (file-directory-p (car dirs)))
      (car dirs)
      (first-available-dir (cdr dirs))))

;; could use "emacs" as directory, but I prefer this approach
(defvar gmacs-version-directory (concat "-" (substring emacs-version 0 5)))
;(defvar gmacs-version-directory "")


(defvar gmacs-lisp-directory
    (first-available-dir (concat "/devel/tools/emacs-19.28" ;; was /usr/local/src/gnu/emacs"
				 gmacs-version-directory
				 "/lisp/"))
    "*GNU Emacs Lisp source directory.")

(defvar gmacs-src-directory
    (first-available-dir (concat "/devel/tools/emacs-19.28" ;; was /usr/local/src/gnu/emacs"
				 gmacs-version-directory
				 "/src/"))
    "*GNU Emacs C source directory.")

(defvar gmacs-utility-directory
  (first-available-dir "/devel/tools/gmacs-hacks/") ;; was /usr/local/lib/TMC-hacks-19/"
  "*Utility directory source directory.")


  
;; This MUST be p7 so that everyone refers to the same one, since directories
;; are now duplicated)

(defvar common-tmc-temp-directory "/tmp/"
  "*Directory where lisp compile-buffer's go.  Change this to /tmp if you
don't care about checking for others compiling the same files, or to work
temporarily if /usr/local/src/utility/tmp is unavailable.")
     
(defun add-path-item (item list endp)
  "Return list with ITEM added to LIST at end if ENDP, else at beginning, only if it is not already present (using 'equal')."
  (if (member-equal item list)
      list
      (cond (endp (append list (list item)))
	    (t    (cons item list)))
      ))

(setq load-path (add-path-item gmacs-lisp-directory load-path t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Local functions

(defvar *aliases-dir* "/etc/")

;; Function to easily edit the TMC mail aliases file
(defvar *aliases-files*  (list (concat *aliases-dir* "aliases"))
  "The old aliases file")

(defun aliases ()
  (interactive)
  (find-file (car *aliases-files*)))

