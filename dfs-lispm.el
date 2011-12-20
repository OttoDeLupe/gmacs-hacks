;;; $Id: dfs-lispm.el,v 1.1 1988/12/27 17:06:10 fad Exp $

;; features.el -- provide Common Lisp (semi)compatible features
;; Copyright (C) 1988 Free Software Foundation, Inc. and Robert Krawitz

;; This file might become part of GNU Emacs

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

;;; Written by Robert Krawitz <rlk@think.com> 7/18/88

;;; Various Lisp Machine pathname hacks
;;; These functions can be used to translate Lisp Machine pathnames 
;;; into Unix style pathnames.

(provide 'dfs-lispm)

(defun translate-lispm-to-unix (pathname)
  "Translates a PATHNAME in lmfs syntax (without the host name) into Unix
syntax suitable for NFS access."
  (let (last-match
	 (return (copy-sequence pathname)))
    (while (setq last-match (string-match ">" return last-match))
      (setf (aref return last-match) ?/))
    return))

(defun extract-path (pathname)
  "Returns the local portion of lmfs PATHNAME, if any."
  (let ((pos (string-match "[^\\]:" pathname)))
    (if pos
	(substring pathname (+ 2 pos))
      (copy-sequence pathname))))

