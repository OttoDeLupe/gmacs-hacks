;;; $Id: dfs.el,v 1.16 1990/03/08 10:08:08 fad Exp $

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
;;; 9/18/88
;;; 3/8/90 jgoldman -- fix dfs-load-n to load .elc files if no .el present.

;; This file might become part of GNU Emacs

;;; Written by Robert L. Krawitz <rlk@think.com> 7/8/88
;;; 
;;; change log:
;;;  9/18/88 brewster added load-path support
;;;  9/19/88 brewster fixed file-complete
;;;  9/21/88 brewster fixed where to not lose when errors occur
;;;  10/2/88 brewster added load-n
;;;  10/2/88 brewster rewrote dfs-load-n
;;;  10/2/88 brewster changed header to be public domain.
;;;   2/9/88 salem added dfs-file-set-source-files
;;;   2/9/88 salem added *dfs-source-file-extensions*
;;;   2/9/88 salem modified source-file-complete to ignore files with 
;;;           extensions in completion-ignored-extensions
;;;   6/16/89 modified require to use nice file convensions
;;;           I dont know if this is necessary, but the old one seemed to hang
;;;           if the source machine was down.
;;;   7/5/89  made truename be able to not probe a filesystem that has be demonstrated to be 
;;;           down recently.  see *cached-unavailability-time*

(require 'cl)
(require 'features)
(require 'dfs-local)
(require 'time-calc)
(provide 'dfs)

(defconst *dfs-source-file-extensions* '("el" "lisp" "c")
  "*A list of source file extensions.")

(defvar *dfs-file-sets* nil
  "Cached file sets, as a list of these forms:
(file-set
 (mod-time-hi mod-time-low) for the .dfs file
 (mod-time-hi mod-time-low) for the def-file-set file
 file-set-info)")

(defvar *dfs-loaded-files* nil
  "Cached loaded files, as a list of these forms:
(file (s-mod-time-hi s-mod-time-low) (b-mod-time-hi b-mod-time-low))")

;;; The following definition may be overridden in dfs-local.
(when (not (fboundp 'get-unix-pathname))
  (defun get-unix-pathname (path)
    "Return a valid Unix pathname from PATH, which may have local semantics."
    (copy-sequence path)))

(defun extract-host (pathname)
  "Returns the host portion of common-lisp style PATHNAME, if any."
  (let ((pos (string-match "[^\\]:" pathname)))
    (when pos (substring pathname 0 (1+ pos)))))

(defun symeq (sym1 sym2)
  "T if SYM1 has the same name, modulo packages, as SYM2.  Symbols may be
either symbols or strings."
  (string= (downcase (depackage (if (symbolp sym1) (symbol-name sym1) sym1)))
	   (downcase (depackage (if (symbolp sym2) (symbol-name sym2) sym2)))))

(defun file-complete (filename suffix)
  "If FILENAME does not end in a suffix, append .SUFFIX to it."
  (cond ((equal filename "")
         (concat filename "." suffix))
        ((string-match "\\." 
		    ;;dont look at first character
		    ;; because of unix convention -brewster
		    (substring (file-name-nondirectory filename) 1))
         filename)
        (t (concat filename "." suffix))))

(defun file-uncomplete (filename)
  "If FILENAME does ends in a .suffix, it strips it"
  (if (string-match 
	"\\." 
	(substring (file-name-nondirectory filename)
		   ;;dont look at first character
		   ;; because of unix convension -brewster
		   1 nil))
      (let ((answer nil)
	    (dot-char   (aref "." 0))
	    (length (length filename)))
	(dotimes (i (1- (length filename)))
	  (if (and (not answer)
		   (eq (aref filename (- length i 1))
		  dot-char))
	      (setq answer (- length i 1))))
	(if answer
	    (substring filename 0 answer)
	    (error "something is wrong in file-uncomplete of %s"
		   filename)))
      filename))

(defun test-file-uncomplete ()
  (if (not (equal (file-uncomplete "foo.el")
		  "foo"))
      (error "foo.el"))
  (if (not (equal (file-uncomplete "fooel.")
		   "fooel"))
      (error  "fooel."))
  (if (not (equal (file-uncomplete ".fooel")
		   ".fooel"))
      (error  ".fooel"))
  (if (not (equal (file-uncomplete "fooel")
		   "fooel"))
      (error  "fooel"))
  (if (not (equal (file-uncomplete "/foo.dir/fooel")
		   "/foo.dir/fooel"))
      (error  "/foo.dir/fooel"))
  
  (if (not (equal (file-uncomplete "/foo.dir./fooel")
		   "/foo.dir./fooel"))
      (error  "/foo.dir./fooel")))

(defun file-has-ignored-extension-p (filename)
  (catch 'end
    (let* ((extension-pos nil)
	   extension
	   (curpos  (1- (length filename)))
	   )
		       
      (while (> curpos 1)
	(if (= (aref filename curpos) ?.)
	    (setq extension-pos curpos
		  curpos 0))
	(setq curpos (1- curpos))
	)
      (when extension-pos
	(setq extension (substring filename extension-pos))
	(dolist (i completion-ignored-extensions)
	  (if (string-equal extension i)
	      (throw 'end t))
	  )))))
;;; tests
;; (file-has-ignored-extension-p "foo.vbin") --> t
;; (file-has-ignored-extension-p "foo.lisp") --> nil
;; (file-has-ignored-extension-p ".vbin")    --> nil
;; (file-has-ignored-extension-p "foo")      --> nil
;; (file-has-ignored-extension-p "")         --> nil
 
(defun source-file-complete (filename &optional existing-only)
  "Complete FILENAME for emacs lisp purposes.  It tries the extensions
listed in *dfs-source-file-extensions*.  Finally it returns a .lisp 
default extension"
  (catch 'found
    (when (and (file-exists-p filename)
	       (not (file-has-ignored-extension-p filename)))
      (throw 'found filename))
    (let ((filename-no-ext (file-uncomplete filename)))
      (dolist (ext *dfs-source-file-extensions*)
	(let ((fn (file-complete filename-no-ext ext)))
	  (when (file-exists-p fn)
	    (throw 'found fn)))))
    (if (not existing-only)
	(file-complete filename "lisp"))))

(defun file-time-older (file-time-1 file-time-2)
  "Return non-nil if FILE-TIME-1 represents an older time than FILE-TIME-2."
  (or (< (car file-time-1) (car file-time-2))
      (and (= (car file-time-1) (car file-time-2))
	   (< (car (cdr file-time-1)) (car (cdr file-time-2))))))

(defun file-older (file-1 file-2)
  "Return non-nil if FILE-1 is older than FILE-2"
  (file-time-older (nth 5 (file-attributes file-1))
		   (nth 5 (file-attributes file-2))))

(defun file-time-younger (file-time-1 file-time-2)
  "Return non-nil if FILE-TIME-1 represents a younger time than FILE-TIME-2."
  (or (> (car file-time-1) (car file-time-2))
      (and (= (car file-time-1) (car file-time-2))
	   (> (car (cdr file-time-1)) (car (cdr file-time-2))))))

(defun file-younger (file-1 file-2)
  "Return non-nil if FILE-1 is younger than FILE-2"
  (file-time-younger (nth 5 (file-attributes file-1))
		     (nth 5 (file-attributes file-2))))

(defun file-match (files1 files2)
  "Non-nil if any name in FILES1 matches any name in FILES2"
  (catch 'found
    (dolist (rfile files1)
      (dolist (cfile files2)
	(when (string-equal rfile cfile)
	  (throw 'found t))))))

;;; Various file set name manipulators

(defun dfs-base-name (file-set)
  "Returns the base portion of FILE-SET's name (without freeze numbers)."
  (let (where)
    (if (setq where (string-match "-f[0-9\.]+$" file-set))
	(substring file-set 0 where)
      (copy-sequence file-set))))

(defun dfs-version (file-set)
  "Returns the freeze number as an integer of FILE-SET."
  (let (where)
    (when (setq where (string-match "-f[0-9\.]+$" file-set))
      (string-to-int (substring file-set (+ 2 where))))))

(defun dfs-make-file-set-name (base version)
  "Returns a complete file set name from BASE and VERSION (freeze number)."
  (concat base "-f" (int-to-string version)))

;;; Stripping out unparsable common lisp/font trash helps

(defun delete-next-font-info()
  (let ((here (point-marker)))
    (when (search-forward "\^F" nil t)
      (cond ((looking-at "\^E")
	     (delete-region (1- (point))
			    (progn (if (zerop (marker-position here))
				       (progn
					 (search-forward
					   "\[Begin using 006 escapes\]"
					   nil t)
					 (when (looking-at "\^F")
					   (forward-char 1)))
				     (search-forward "\^F" nil t))
				   (goto-char (1- (point))))))
	    ((looking-at "(")
	     (delete-region (1- (point))
			    (progn (forward-sexp 1)
				   (point))))
	    ((looking-at "[0123456789]")
	     (delete-region (1- (point)) (1+ (point))))
	    (t (forward-char 1)))
      (goto-char (1+ here))
      (set-marker here nil)
      t)))

(defun delete-all-font-info()
  "Remove all font information from a buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (delete-next-font-info))))

(defun strip-flashy-comments ()
  "Read doesn't seem to like #||# comments.  This is terribly brain-dead."
  (save-excursion
    (goto-char (point-min))
    (let (start)
      (while (search-forward "\#\|" nil t)
	(setq start (- (point) 2))
	(search-forward "\|\#" nil 'move)
	(delete-region start (point))))))

(defun strip-all-junk ()
  "Removes comments, #| comments, and expands features in buffer."
  (condition-case ()
      (delete-all-font-info)
    (error nil))
  (condition-case ()
      (strip-flashy-comments)
    (error nil))
  (condition-case ()
      (strip-conditionals)
    (error nil)))

(defun depackage (symbol-name)
  "Extracts the package prefix of SYMBOL-NAME, which should be a string."
  (let ((pos (string-match "[^\\]:" symbol-name)))
    (if pos
	(substring symbol-name (+ 2 pos))
      (copy-sequence symbol-name))))

;;; Hunt down a file set.

(defun find-dfs (file-set &optional quiet)
  "Finds directory that FILE-SET is located in.  If optional QUIET non-nil,
don't print out status messages."
  (let* ((pathname (let ((load-path *dfs-home-directory*))
		     (truename (concat file-set ".dfs"))))
	 (old-info (assoc file-set *dfs-file-sets*))
	 (old-attributes (second old-info))
	 (attributes (file-attributes pathname)))
    (if (and (second (fourth old-info))
	     (or (and old-attributes (not attributes))
		 (not (file-time-older old-attributes (nth 5 attributes)))))
	(get-unix-pathname (second (fourth old-info)))
      (prog1
	  (let* ((default-major-mode 'fundamental-mode)
		 (tembuf (generate-new-buffer "dfs-temp")))
	    (when (not quiet)
	      (message "Looking for file set directory for %s . . ." file-set))
	    (unwind-protect
		(progn
		  (set-buffer tembuf)
		  (set-syntax-table (copy-syntax-table lisp-mode-syntax-table))
		  (modify-syntax-entry ?# "  14")
		  (modify-syntax-entry ?\| "  23")
		  (if (truename pathname ':exists)
		      (insert-file-contents (truename pathname))
		    (error "File set directory not found for %s" file-set))
		  (when (not quiet)
		    (message "Looking for file set directory for %s . . .done."
			     file-set))
		  (strip-all-junk)
		  (goto-char (point-min))
		  (catch 'found
		    (while (not (eobp))
		      (let ((form (read (current-buffer))))
			(and (listp form)
			     (>= (length form) 3)
			     (symbolp (first form))
			     (symeq (first form) 'def-file-set-directory)
			     (symbolp (second form))
			     (symeq (second form) file-set)
			     (stringp (third form))
			     (throw 'found
				    (source-file-complete
				     (get-unix-pathname (third form)))))))))
	      (kill-buffer tembuf)))
	(if old-info
	    (setcdr old-info (list (nth 5 attributes) nil nil))
	  (push (list (downcase file-set) (nth 5 attributes) nil nil)
		*dfs-file-sets*))))))

;;; Return information on a file set.

(defun dfs-info (file-set &optional quiet)
  "Return various information on FILE-SET.  If optional QUIET non-nil, don't
display status messages as various operations are performed.  Returns a list
of four values: the name of the file set as a symbol, the directory in
which the file set resides, translated into a Unix pathname,  auxiliary
information about the file set as defined by DFS, and a list of all files
with any auxiliary information, also defined by DFS."
  (let* ((where (find-dfs file-set quiet))
	 (default-major-mode 'fundamental-mode))
    (let* ((pathname (truename (get-unix-pathname where)))
	   (old-info (assoc file-set *dfs-file-sets*))
	   (old-attributes (third old-info))
	   (attributes (file-attributes pathname)))
      (if (and (fourth old-info)
	       (or (and old-attributes (not attributes))
		   (not (file-time-older old-attributes (nth 5 attributes)))))
	  (fourth old-info)
	(let ((info
		(let* ((default-major-mode 'fundamental-mode)
		       (tembuf (generate-new-buffer "dfs-temp")))
		  (when (not quiet)
		    (message "Looking for DEF-FILE-SET for %s . . ." file-set))
		  (unwind-protect
		      (progn
			(set-buffer tembuf)
			(set-syntax-table (copy-syntax-table
					    lisp-mode-syntax-table))
			(modify-syntax-entry ?# "  14")
			(modify-syntax-entry ?\| "  23")
			(insert-file-contents (truename
						(get-unix-pathname where)))
			(when (not quiet)
			  (message
			    "Looking for DEF-FILE-SET for %s . . . done."
			    file-set))
			(strip-all-junk)
			(goto-char (point-min))
			(catch 'found
			  (while (not (eobp))
			    (let ((form (read (current-buffer))))
			      (and (listp form)
				   (symbolp (first form))
				   (symeq (first form) 'def-file-set)
				   (listp (second form))
				   (symeq (car (second form)) file-set)
				   (throw 'found (list (car (second form))
						       where
						       (cdr (second form))
						       (cddr form))))))))
		    (kill-buffer tembuf)))))
	  (setcdr (cdr old-info) (list (nth 5 attributes) info))
	  info)))))

(defun dfs-file-set-source-files (file-set &optional existing-only-p
					   return-nondirectory ignore-no-load)
  "Returns a list of all the source files for file-set"
  (let* ((info (dfs-info file-set))
	 (dir (file-name-directory (get-unix-pathname (second info))))
	 (files nil))
    (dolist (desc (cons "def-file-set" (fourth info)))
      (catch 'noedit
	(let* ((filename (cond ((and (listp desc) (stringp (car desc)))
				(if (and ignore-no-load
					 (member ':no-load desc))
				    (throw 'noedit)
				    (car desc)))
				((stringp desc) desc)
				(t (throw 'noedit nil))))
	       (fullname (concat dir filename))
	       (existing-file
		(source-file-complete fullname existing-only-p))
	       )
	  (if existing-file
	      (push (if return-nondirectory
			(file-name-nondirectory existing-file)
			existing-file)
		    files))
	  )))
    (reverse files)))

(defun dfs-create-tags-file (file-set-list tags-file-name)
  "Creates tags files for each file set in FILE-SET-LIST, and creates a master
tags file in TAGS-FILE-NAME."
  (let ((buf (generate-new-buffer "dfs-tags"))
	(errbuf (generate-new-buffer "etags-errors")))
    (unwind-protect
	(progn
	  (dolist (fs file-set-list)
	    (condition-case ()
		(let ((info (dfs-info fs)))
		  (when info
		    (let ((dir (file-name-directory (get-unix-pathname
						     (second info))))
			  (files-to-tag (dfs-file-set-source-files fs t))
			  )
		      (when files-to-tag
			(message "Running tags on %s (%s) " (first info) dir)
			(let ((files (mapconcat 'identity files-to-tag " ")))
			  (call-process
			    "/bin/csh" nil errbuf nil
			    "-fvc"
			    (concat "cd " dir "; /usr/local/etags " files))))
		      (princ (concat "\^l\n" dir "TAGS,include\n") buf))))
	      (error nil)))
	  (set-buffer buf)
	  (write-region (point-min) (point-max) tags-file-name))
	(kill-buffer buf)
	(kill-buffer errbuf))))

(defun dfs-edit-file-set (file-set)
  "Edits each file in FILE-SET by placing it in an editor buffer."
  (interactive "sFile set: ")
  (dolist (file (dfs-file-set-source-files file-set))
    (let ((local-name (file-name-nondirectory file))
	  )
      (condition-case nil
	  (if (file-exists-p file)
	      (progn (message "Reading %s . . ." local-name)
		     (find-file-noselect file)
		     (message "Reading %s . . . done." local-name))
	      (error nil))
	(error (ding)
	       (message "Reading %s . . . not found!" local-name)))))
  (sit-for 0))

;;added by brewster
(defun load-n (file)
  (interactive "FLoad-n File: ")
  (or (dfs-load-n file)
      (message "done")))

(defun dfs-load-n (file &optional recompile reload show confirm just-warn)
  "Load and compile, if need be, FILE.  If optional RECOMPILE is t then always
recompile the file; if RECOMPILE is 'never then don't recompile the file.
If optional RELOAD is t then always reload the file; otherwise reload only
if not already loaded.  If SHOW is non-nil, don't actually do anything.  If
JUST-WARN is non-nil, then do not error if file cannot be loaded."
  (let ((real-file (truename
		     (file-complete (get-unix-pathname file) "el"))))
    (cond ((null real-file)
	   ;; let's check to see if there's an .elc
	   (let ((compiled-file (truename
				 (file-complete (get-unix-pathname file) "elc"))))
	     (if compiled-file
		 (load compiled-file)
		 (if just-warn 
		     (warn "Could not find file %s in the load-path" file)
		     (error "Could not find file %s in the load-path" file)))))
	  (t 
	    (let* ((source-file real-file)
		   (bin-file (file-complete (file-uncomplete real-file)
					    "elc"))
		   (source-file-attributes (file-attributes source-file))
		   (source-times (nth 5 source-file-attributes))
		   (bin-file-attributes (file-attributes bin-file))
		   (bin-times (nth 5 bin-file-attributes))
		   (loaded-info (assoc real-file *dfs-loaded-files*))
		   return)
	      
	      ;; in the case of never recompile, and there
	      ;; is no bin file then you lose.
	      (if (and (null bin-times) (eq recompile 'never))
		  (error "There is no bin file to load"))

	      ;;compile it if you need to:
	      (when (and source-file-attributes (not (eq recompile 'never)))
		(when (and (not (eq recompile 'never))
			   (or (eq recompile t)
			       (not bin-times)
			       (and source-times
				    (file-time-older 
				      (or bin-times '(0 0))
				      (or source-times '(0 0))))))
		  (when (not show)
		    (require 'byte-compile "bytecomp")
		    (when (or (not confirm) 
			      (y-or-n-p (concat "Compile " source-file)))
		      (byte-compile-file source-file)
		      (setq bin-times (nth 5 (file-attributes bin-file)))
		      ))
		  (setq return 'compile)))

	      ;;load it if you need to:
	      (let ((loaded-bin-times (car (cdr (cdr loaded-info)))))
		(when (or (eq reload t)
			  (null loaded-bin-times)
			  (null bin-times);;it was compiled
			  (file-time-younger 
			    bin-times loaded-bin-times))
		  (when (not show)
		    (when (or (not confirm) 
			      (y-or-n-p (concat "Load " source-file)))
		      (when (not (load bin-file t))
			(error "Bin file %s was not loaded for some reason. Bug in load-n" bin-file))
		      (setq return (or return 'load))
		      ))))
	      ;;record that we did it.
	      (when return ;;we did something
		(if loaded-info
		    (setcdr loaded-info
			    (list source-times bin-times))
		  (push (list real-file source-times bin-times)
			*dfs-loaded-files*)))
	      return)))))

(defun warn (format-string &rest args)
  (apply 'message format-string args)
  (ding)
  (sit-for 1))

(defun dfs-compile-load-file-set (file-set &optional recompile reload show
					   confirm)
  "Load and compile, if need be, FILE-SET.  If optional RECOMPILE is t then
always recompile the file set; if RECOMPILE is 'never then don't recompile
the file set.  If RELOAD is non-nil, then reload every file. 
If SHOW is non-nil, don't actually do anything."
  (let* ((info (dfs-info file-set))
	 (dir (file-name-directory (second info)))
	 (files (fourth info))
	 compiled-files loaded-files
	 (never-recompile (memq recompile '(never :never))))
    (dolist (file files)
      (catch 'noaction
	(cond ((stringp file)
	       (let* ((real-file (expand-file-name file dir))
		      (what-done (dfs-load-n real-file recompile reload show
					    confirm)))
		 (case what-done
		   (load (push file loaded-files))
		   (compile (push file compiled-files)
			    (push file loaded-files))
		   (t nil))))
	      (t			; Should be a list
		(let ((real-file (expand-file-name (car file) dir))
		      (actions (cdr file))
		      (forced-recompile nil)
		      (forced-reload nil))
		  (dolist (action actions)
		    (cond ((member action '(noload :noload))
			   (throw 'noaction nil))
			  ((listp action)
			   (case (car action)
			     ((compile-load :compile-load compile :compile)
			      (when (not (or forced-recompile
					     never-recompile))
				(setq forced-recompile
				      (or forced-recompile
					  (file-match (cdr action)
						      compiled-files)))))
			     ((load :load reload :reload)
			      (when (not forced-reload)
				(setq forced-reload
				      (or forced-reload
					  (file-match (cdr action)
						      loaded-files)))))
			     (t nil)))
			  (t nil)))
		  (let ((what-done (dfs-load-n
				     real-file
				     (or recompile forced-recompile)
				     (or reload forced-reload)
				     show confirm)))
		    (case what-done
		      (load (push (car file) loaded-files))
		      (compile (push (car file) compiled-files))
		      (t nil))))))))))

(defun dfs-load-file-set (file-set &optional reload show)
  "Load FILE-SET as needed, without any compilation.  If optional RELOAD is
non-nil, then reload every file.  If SHOW is non-nil, don't actually do
anything."
  (let* ((info (dfs-info file-set))
	 (dir (file-name-directory (second info)))
	 (files (fourth info))
	 loaded-files)
    (dolist (file files)
      (catch 'noaction
	(cond ((stringp file)
	       (let ((what-done (dfs-load-n file 'never reload show)))
		 (case what-done
		   (load (push file loaded-files))
		   (t nil))))
	      (t			; Should be a list
		(let ((real-file (car file))
		      (actions (cdr file))
		      (forced-recompile nil)
		      (forced-reload nil))
		  (dolist (action actions)
		    (cond ((member action '(noload :noload))
			   (throw 'noaction nil))
			  ((listp action)
			   (case (car action)
			     ((load :load reload :reload
				    compile-load :compile-load)
			      (when (not (or forced-reload never-load))
				(setq forced-reload
				      (or forced-reload
					  (file-match (cdr action)
						      loaded-files)))))
			     (t nil)))
			  (t nil)))
		  (let ((what-done (dfs-load-n
				     file 'never (or reload forced-reload)
				     show)))
		    (case what-done
		      (load (push file loaded-files))
		      (t nil))))))))))

(defun where (path &optional attribute just-the-first)
  "returns the list of filenames that contain the path
   if only the best is desired, see truename.
   The path can be a symbol or a string."
  (if (null attribute) (setq attribute ':readable))
  (let ((answer nil)
	;;allow both symbols and strings to be passed in.
	(path (if (stringp path) path (format "%s" path))))
    (dolist (trial-directory load-path)
      (if (and just-the-first answer)
	  nil;;avoid looking for more than just the first
	  (condition-case ()
	       (let ((new-path
		      (expand-file-name path 
					trial-directory)))
		 (ecase attribute 
		   (:readable 
		    (if (and (file-readable-p new-path)
			     (not (member-equal new-path answer)))
			(push new-path answer))) 
		   (:exists   
		    (when (and (file-exists-p new-path)
			       (not (member-equal new-path answer)))
		      (push new-path answer)
		      ))))
	     (error nil))))
    ;;put the current path last.  this is a guess at the right thing here.
    ;; -brewster
    (condition-case ()
	 (if (and (ecase attribute 
		    (:readable (file-readable-p path))
		    (:exists   (file-exists-p path)))
		  (not (member-equal (expand-file-name path)
				     answer)))
	     (push (expand-file-name path) answer))
       (error nil))
    (reverse answer)))

(defvar *shadow-load-path*
  nil "this holds a version of the load-path that has timeouts in it")
(defvar *cached-unavailability-time* 1
  "time in hours to not test a fileserver after it has been found to be unavailable.")

(defun truename (path &optional attribute)
  "this tries to return the expanded pathname.
   First it uses the default directory and then goes down the
   load-path to find the first one that :exists or :readable"
  (if (null attribute) (setq attribute ':readable))
  (update-shadow-load-path-if-necessary)
  (let ((answer nil)
	;;allow both symbols and strings to be passed in.
	(path (if (stringp path) path (format "%s" path)))
	(paths-tested 0))
    (dolist (trial-directory *shadow-load-path*)
      (cond (answer;;avoid looking for more than just the first.
	     ;;this is to fake a return
	     nil)
	    ((and (cadr trial-directory);;then we have a timeout
		  (< (- (tc-hours-since-1900) (cadr trial-directory))
		     *cached-unavailability-time*))
	     ;;dont try this trial directory because it has been found to be
	     ;;unavailable recently (as set by *cached-unavailability-time*)
	     nil)
	    (t
	     (setq paths-tested (1+ paths-tested))
	     (condition-case ()
		  (let ((new-path
			 (expand-file-name path 
					   (car trial-directory))))
		    (ecase attribute 
		      (:readable 
		       (if (and (file-readable-p new-path)
				(not (member-equal new-path answer)))
			   (setq answer new-path))) 
		      (:exists   
		       (when (and (file-exists-p new-path)
				  (not (member-equal new-path answer)))
			 (setq answer new-path)))))
		(error 
		 (progn
		  ;;set the unavailable slot of the trial-directory
		  (setf (car (cdr trial-directory)) (tc-hours-since-1900))
		  (setf (car (cdr (cdr trial-directory))) path)
		  nil))))))
    (cond ((and (= paths-tested 0) load-path)
	   ;;then we may have an error that cleared all the times
           ;;try it again.
	   (reset-shadow-load-path)
	   (truename path attribute))
	  (t answer))))

(defun update-shadow-load-path-if-necessary ()
  "This updates *shadow-load-path* if load-path has changed.
   This will invalidate all the timeouts if load-path has changed."
  (if (and (= (length *shadow-load-path*)
	      (length load-path))
	   (equal (mapcar 'car *shadow-load-path*) load-path))
      nil ;;do nothing.  the *shadow-load-path* is fine
      (setq *shadow-load-path*
	    (mapcar '(lambda (path) (list path nil nil)) load-path))))

(defun reset-shadow-load-path ()
  (setq *shadow-load-path*
	nil))

(defun member-equal (item list)
  "[cl] MEMBER ITEM LIST => Is ITEM in LIST?  Uses equal on the cars of LIST."
  (let ((ptr list)
        (done nil)
        (result '()))
    (while (not (or done (endp ptr)))
      (cond ((equal item (car ptr))
             (setq done t)
             (setq result ptr)))
      (setq ptr (cdr ptr)))
    result))

;;patch require to use dfs-load
; (defun require (symbol &optional path)
;  (when (not (featurep symbol))
;    (let ((string (if path 
;		      path
;		      (format "%s" symbol))))
;      (let ((filename (or (truename (format "%s.elc" string))
;			  (truename (format "%s.el" string)))))
;	(cond (filename
;		(load-file filename)
;		(if (not (featurep symbol))
;		    (error "Feature %s was not provided" symbol)))
;	      (t (error "could not find file %s" string)))))))

