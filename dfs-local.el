;;; $Id: dfs-local.el,v 1.3 1989/02/22 14:45:29 massar Exp $

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
;;; Site-specific DFS things.

(provide 'dfs-local)
(require 'dfs-lispm)

(defconst *host-name-mappings*
  '(("aquinas" . translate-aquinas-pathname)
    ("a" . translate-aquinas-pathname)
    (t . extract-path))
  "Translations of pathnames from various hosts.  Each element is a cons of
a hostname and a function of one argument that takes a pathname and returns
a suitable Unix pathname.  This constant is site-specific.")

(defconst *dfs-home-directory*
  '("/cm/dfs/site/"
    "/p1/dfs/site/")
  "Location of .dfs files at your site.")

;;; TMC local utilities

(defun translate-aquinas-pathname (pathname)
  (concat "/lispm/aquinas" (translate-lispm-to-unix (extract-path pathname))))

(defun get-unix-pathname (path)
  "Given a complete or partial lmfs PATH, returns an equivalent Unix pathname."
  (let ((host (extract-host path)))
    (if (not host)
	path
      (catch 'found
	(setq host (downcase host))
	(dolist (elt *host-name-mappings*)
	  (when (or (eq (car elt) t)
		    (string= (downcase (car elt)) host))
	    (throw 'found (apply (cdr elt) (list path)))))))))
