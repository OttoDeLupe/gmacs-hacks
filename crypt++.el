;;; crypt++.el -- code for handling all sorts of compressed and encrypted files

(defconst crypt-version (substring "$Revision: 2.37 $" 11 -2)
  "The revision number of crypt++.el -- code for handling all sorts of
compressed and encrypted files. To send a bug report type M-x
crypt-submit-report. Complete RCS identity is

   $Id: crypt++.el,v 2.37 1993/04/20 21:35:02 dodd Exp $

This file is available via anonymous ftp in:

   /roebling.poly.edu:/pub/crypt++.el

and

   /archive.cis.ohio-state.edu:/pub/gnu/emacs/elisp-archive/misc/crypt++.el.Z
")

;;; Copyright (C) 1993 Rod Whitby and Lawrence R. Dodd
;;; Copyright (C) 1988, 1989, 1990 Kyle E. Jones
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;; LCD Archive Entry:
;; crypt++|Rod Whitby and Lawrence R. Dodd|dodd@roebling.poly.edu|
;; Code for handling all sorts of compressed and encrypted files.|
;; $Date: 1993/04/20 21:35:02 $|$Revision: 2.37 $|~/misc/crypt++.el.Z|

;;; AVAILABLE: 
;;; 
;;; via anonymous ftp to roebling.poly.edu [128.238.5.31] in /pub/crypt++.el
;;; (ange-ftp TAG: /roebling.poly.edu:/pub/crypt++.el)
;;; 
;;; via anonymous ftp to archive.cis.ohio-state.edu [128.146.8.52] in 
;;; /pub/gnu/emacs/elisp-archive/misc/crypt++.el.Z

;;; BUG REPORTS: 
;;; 
;;; just type M-x crypt-submit-report to generate a bug report template

;;; please see notes on INSTALLATION and USAGE below

;;; RESPONSIBLE PARTIES:
;;;
;;;   /Northern Hemisphere/
;;;  
;;;     Lawrence R. Dodd <dodd@roebling.poly.edu>   -  _    ,__o
;;;     Department of Chemical Engineering         -  _   , \_<,
;;;     Polymer Research Institute                  -    (*)/'(*)
;;;     Polytechnic University                     ~~~~~~~~~~~~~~~~~~ 
;;;     Six Metrotech
;;;     Brooklyn, New York, 11201 USA                        
;;;
;;;   /Southern Hemisphere/ 
;;;                                                   _--_|\  
;;;     Rod Whitby <rwhitby@research.canon.oz.au>    /      \ 
;;;     Canon Information Systems Research Australia \_.--._/ 
;;;     1 Thomas Holt Drive                                v 
;;;     North Ryde, N.S.W., 2113 Australia
;;; 
;;;   World-wide coverage!!  Bugs fixed while you sleep!!

;;; VERSION:
;;;  
;;; $Id: crypt++.el,v 2.37 1993/04/20 21:35:02 dodd Exp $
;;; $Date: 1993/04/20 21:35:02 $
;;; $Revision: 2.37 $
;;;
;;; version log
;;; 1.1 - original version of crypt.el
;;; 1.2 -
;;;   jwz: works with tar-mode.el
;;;   jwz: applied patch from piet, merged with Lawrence Dodd's gzip version
;;; 1.3 -
;;;   lrd: fixed compress-magic-regexp 
;;; 1.4, 1.5, 1.6 -
;;;   lrd: write-file compresses or gzips based on file extension
;;; 2.1 -
;;;   lrd: merged with Rod Whitby's table-driven version (major upgrade)
;;; 2.2 -
;;;   rjw: Changed file name to crypt++.el, so archie and lispdir can find it.
;;; 2.3 -
;;;   rjw: Separated the hook additions and minor mode alist additions.
;;; 2.4 -
;;;   rjw: Fixed the interactive form for crypt-buffer.
;;; 2.5 - 
;;;   lrd: doc mods, changed GNU free software notice (was out of date), added 
;;;   anonymous ftp information
;;; 2.6 - 
;;;   lrd: added back in definition of `buffer' in defun crypt-buffer caused 
;;;   an error when trying to read encrypted file; modified check for minor 
;;;   mode alist addition; added gzip magic number warning
;;; 2.7 - [posted to gnu.emacs.sources]
;;;   lrd: added `TO DO' and `KNOW BUGS' section to header 
;;; 2.8 - 
;;;   lrd: added note about updating to v 1.24 of tar-mode.el
;;;   Thanks to Mark Borges <mdb@noaacrd.Colorado.EDU>
;;; 2.9 -
;;;   lrd: moved query about `crypt-freeze-vs-fortran' out of defvar for
;;;   `crypt-encoding-alist,' an erroneous value of `nil' was being stuck into
;;;   alist when user set `crypt-freeze-vs-fortran' was nil, doc mod.
;;;   Thanks to Mark Borges <mdb@noaacrd.Colorado.EDU>
;;; 2.10 -
;;;   rjw: moved query about `crypt-freeze-vs-fortran' back into defvar for
;;;   `crypt-encoding-alist,' - used append to ignore the erroneous `nil'.
;;; 2.11 -
;;;   rjw: fixed a bug in my fix :-(
;;; 2.12 -
;;;   rjw: Defvar crypt-magic-regexp and crypt-magic-regexp-inverse and
;;;   allow either a regexp or an elisp expression.
;;;   Suggested by Franc,ois Pinard <pinard@iro.umontreal.ca>.
;;; 2.13 - 
;;;   lrd: added in info on lispdir.el, doc mods and some puttering while 
;;;   looking over rjw's v 2.12 mods.
;;; 2.14 - 
;;;   lrd: doc mod - trivial huh? switched `compact' and  `gzip' in 
;;;   `crypt-encoding-alist' - want gzip near top
;;; 2.15 - 
;;;   lrd: added in LCD Archive Entry and modified comments on tar-mode.el 
;;;   since the version at the elisp-archive now works with crypt++.el
;;; 2.16 - 
;;;   lrd: provide `crypt' as well as `crypt++' allowing something like `ln -s 
;;;   crypt++.el crypt.el' to be meaningful 
;;;   Suggested (by|as) Per Abrahamsen <amanda@iesd.auc.dk>
;;; 2.17 -
;;;   lrd: clarified bug report procedure, added fancy pseudo-graphics, added 
;;;   to the `TO DO' list, put RCS tags in LCD Archive entry
;;; 2.18 - [posted to gnu.emacs.sources]
;;;   lrd: included pointer to elisp archive in crypt-version description,
;;;   changed "Decode buffer %s? " to "Decode %s? " in crypt-find-file-hook 
;;;   to be more general (mainly for crypt-insert-file)
;;; 2.19 -
;;;   rjw: Added the crypt-compact-vs-C++ switch to distinguish compacted and
;;;   C++ files.
;;; 2.20 -
;;;   lrd: (1) modified interactive form of crypt-buffer. (2) made search 
;;;   case-insensitive in crypt-submit-report. (3) modified encoded-mode and 
;;;   crypt-mode so that buffer-modified is not unconditionally set to `nil' 
;;;   when the mode is not changed. Thanks to Gerd Hillebrand 
;;;   <ggh@cs.brown.edu> for suggesting (2) and (3).
;;; 2.21 -
;;;   rjw: Added an entry to the TODO list about the hazards of using
;;;   call-process-region on a large region and not much room in /tmp
;;;   (David Carlisle <carlisle@computer-science.manchester.ac.uk>).
;;; 2.22 - 
;;;   lrd: allow write-file-hooks to contain functions as well as lists. 
;;;   Contributed by Ken Laprade <laprade@trantor.harris-atd.com>.
;;; 2.23 - 
;;;   lrd: made crypt-submit-report list values of more user-defined variables
;;; 2.24 - 
;;;   lrd: pass the -q switch to gzip to thwart the possibility of a --verbose
;;;   in the GZIP environment variable
;;; 2.25 -
;;;   lrd: added some more to the TO DO list, clarified some things, also 
;;;   untabified the entire file (I got tired of the control I's) 
;;; 2.26 - 
;;;   lrd: use the long-named options for GNU zip (self-documenting)
;;; 2.27 - 
;;;   lrd: included observation by Francois Pinard <pinard@iro.umontreal.ca> 
;;;   and worked on text in TO DO/KNOWN BUGS list
;;; 2.28 - 
;;;   lrd: added two new variables in (crypt-submit-report) to the list stuck
;;;   at the bottom of the mail message; changed the comments regarding the 
;;;   user-defined variables.  added in default values in user defined 
;;;   variables.  added to and removed stuff to the `TO DO' list.
;;;
;;;   (encoded-mode): 
;;;   added in code to remove any auto-save-files that may have been formed
;;;   before becoming an encoded buffer (for example a plain file saved to
;;;   disk encoded had orphan auto-save-files left behind).  turning off
;;;   auto-save-mode disables the creation of auto-save-files, but it also 
;;;   disables the possibility of these being removed when the buffer is 
;;;   saved.
;;; 
;;;   (crypt-region): 
;;;   now call the encrytion and decryption program directly instead of
;;;   through the shell.  this is more secure since the shell will expose the
;;;   password (key).  thanks to Jon Cargille <jcargill@cs.wisc.edu>.  defined
;;;   two new variables `crypt-decryption-args' and `crypt-encryption-args' to
;;;   take the arguments separately.  removed (let ((opoint)...)) construct 
;;;   this was a throw back to some old dead code and was not being used.
;;; 2.29 - 
;;;   lrd: added three new variables in (crypt-submit-report); added to the 
;;;   `TO DO' list.
;;;  
;;;   (encode-region,encode-buffer,encoded-mode): fixed interactive forms -
;;;   the conversion to table version had eliminated some of the interactive
;;;   features of these.  thanks to Kimball Collins <kpc@ptolemy.arc.nasa.gov>
;;;   for point this out.  new interactive form uses functions
;;;   `crypt-get-encoding-type' and `crypt-symbol-alist-to-table' and variable
;;;   `crypt-default-encoding' to generate completion list of encoding types.
;;; 
;;;   (crypt-write-file-hook): two new user-defined variables
;;;   `crypt-query-if-interactive' and `crypt-no-extension-implies-plain' and
;;;   the buffer-local variable `buffer-interactive-mode' are used to help
;;;   determined whether or not plain output is really desired for files
;;;   without a compression file-name extension.  the default behavior is the
;;;   same as before.
;;; 2.30 - 
;;;   lrd: added test for user-defined variable `crypt-never-ever-decrypt' 
;;;   when finding a file.  some users may never wish to decrypt files 
;;;   and like to edit binary files.  thanks to Nelson Minar 
;;;   <nelson@reed.edu>.  added to doc-strings of 
;;;   `crypt-magic-regexp[-inverse]' -- these can be set to `nil[t]' and 
;;;   accomplish the same thing as setting `crypt-never-ever-decrypt' to `t'
;;; 2.31 - 
;;;   rjw: Updated the comments in the encryption check section.
;;; 2.32 - [posted to gnu.emacs.sources]
;;;   lrd: added warning about `crypt-(de|en)cryption-program'; doc mod.
;;; 2.33 - 
;;;   lrd: if `crypt-(de|en)cryption-args' are `nil' then don't pass any
;;;   arguments to (de|en)cryption program, `nil' is the default instead of
;;;   "".  Thanks to Joe Ilacqua <spike@world.std.com>, David J. Schur
;;;   <djs@idm.com>, Peter Nuth <nuth@ai.mit.edu>, and Greg Larson 
;;;   <glarson@bnr.ca>.  `-q' exists in gzip 1.0.3 but not `--quiet' changed 
;;;   GZIP NOTE.  Thanks to Chris Moore <moore@src.bae.co.uk>.
;;; 2.34 - 
;;;   lrd: allow `crypt-(de|en)cryption-args' to be a list of strings -- more
;;;   robust.  query for password (key), if none is set, when writing out file
;;;   for which `buffer-save-encrypted' is `t.'  Thanks to John Interrante
;;;   <interran@uluru.Stanford.EDU>.  (crypt-write-file-hook): check filename
;;;   extension against regexp `crypt-encryption-file-extension' and query for
;;;   encryption, unless `crypt-auto-write-buffer-encrypted' is `t' (don't
;;;   bother doing reverse check, encrypted to plain, not a common request).
;;;   (crypt-mode): delete auto-save files (cf., encoded-mode), may exist now.
;;;   (read-string-no-echo): applied patch from Piet van Oostrum
;;;   <piet@cs.ruu.nl> -- set `cursor-in-echo-area' _after_ setting buffer
;;;   (this was screwing up gnews).
;;; 2.35 - 
;;;   lrd: doc mod
;;; 2.36 - 
;;;   lrd: fixed typo, added RMAIL note.
;;; 2.37 - 
;;;   lrd: 
;;;   (crypt-write-file-hook): search user-defined list
;;;   `crypt-ignored-filenames' for possible match with `buffer-filename'
;;;   before attempting conversion from compressed to plain format; useful for
;;;   compressed incoming mail files (e.g., RMAIL, INBOX).
;;;  
;;;   (crypt-mode): query for key if not set already; need to switch order of
;;;   recovering key and toggling crypt-mode in crypt-find-file-hook (thanks
;;;   to Piet van Oostrum <piet@cs.ruu.nl>).
;;;  
;;;   (crypt-buffer) and (encode-buffer): remove interactive form; use
;;;   (crypt-mode) and (encoded-mode) instead so encryption and compression
;;;   are done at the very end; leave interactive form in (crypt-region) and
;;;   (encode-region) may still be used.
;;;  
;;;   (set-encryption-key): remove from `command-history' if called
;;;   interactively - thanks to George M. Georgiou
;;;   <georgiou@silicon.csci.csusb.edu>.


;; INSTALLATION:
;;
;; To use this package, simply put it in a file called "crypt++.el" in a Lisp
;; directory known to Emacs, byte-compile it, and put the line:
;;
;;                      (require 'crypt++)
;;
;; in your ~/.emacs file or in the file default.el in the ../lisp directory of
;; the Emacs distribution.  Do not bother trying to autoload this file; this
;; package uses find-file and write-file hooks and thus should be loaded the
;; first time you visit any sort of file.  Any package loaded after crypt++.el
;; that appends something to `write-file-hooks' will not be executed because
;; crypt++.el writes out the file.  Other packages that append to
;; `write-file-hooks' should either be modified to prepend to that hook or be
;; loaded before crypt++.el (preferably the former).
;;
;; An alternative is to stick 
;; 
;;                      (require 'crypt) 
;; 
;; in your ~/.emacs, if it is not already there, and then make a (symbolic) 
;; link from crypt++.el to crypt.el
;; 
;;                      ln -s crypt++.el crypt.el
;; 
;; this will make crypt++ act like a drop-in replacement for the original
;; crypt since any package requiring crypt will find crypt++ instead.  Thanks
;; to Per Abrahamsen <amanda@iesd.auc.dk> for this suggestion.
;;


;; USAGE:
;; 
;; By default, intended to be transparent.  User-defined variables are:
;; 
;;        crypt-encoding-alist
;;        crypt-default-encoding
;;        crypt-encryption-program
;;        crypt-decryption-program
;;        crypt-encryption-args
;;        crypt-decryption-args
;;        crypt-encryption-file-extension
;;        crypt-auto-decode-buffer
;;        crypt-auto-write-buffer
;;        crypt-auto-write-buffer-encrypted
;;        crypt-ignored-filenames
;;        crypt-query-if-interactive
;;        crypt-no-extension-implies-plain
;;        crypt-never-ever-decrypt
;;        crypt-freeze-vs-fortran
;;        crypt-compact-vs-C++
;; 
;; to find out more about these variables, load this file, put your cursor at 
;; the end of any of the above lines, and hit C-h v [RET].
;;  
;; Users should be aware that `crypt-(de|en)cryption-program' can not contain
;; arguments (switches) since the encryption program is executed directly
;; instead of via `shell-file-name.'  Arguments must be defined in
;; `crypt-(de|en)cryption-args' as a string or a list of strings.
;;
;; although rarely needed, the following functions may be called interactively
;;
;;        (encoded-mode)
;;        (encode-region)
;;        (crypt-mode)
;;        (crypt-region)
;;        (set-encryption-key)
;;        (crypt-submit-report)
;;
;; to find out more about these functions, load this file, put your cursor
;; inside any of the `()' of the above lines, and hit C-h f [RET].


;; NOTES ON INTERFACES WITH OTHER PROGRAMS AND PACKAGES:
;;
;; GZIP NOTE: the magic number used by gzip (GNU zip) was changed by Jean-loup
;; Gailly in his Beta version 0.6. The magic regular expression used below in
;; `crypt-encoding-alist' reflects this change. If you are using a version of
;; gzip earlier than 0.6, then please upgrade.
;; 
;; The addition of the environment variable GZIP in version 1.0.2 of gzip
;; means that an error is possible with crypt++.  If the user uses GZIP to
;; define `--verbose' as one of the default options for gzip, then the
;; standard output messages will be appended to gzip'ed files by crypt++.
;; This corrupts the files.  The cleanest solution is for crypt++ always to
;; use the `-q' or `--quiet' switch, of gzip version 1.0.4 or higher, to
;; cancel the `--verbose' in the GZIP environment variable.  Bottom line: if
;; you are using a version of gzip earlier than 1.0.4, then please upgrade.
;; 
;; TAR-MODE NOTE: crypt++.el does not work properly with version 1.12 of 
;; tar-mode.el. Please obtain an updated version of tar-mode.el from 
;; roebling.poly.edu or archive.cis.ohio-state.edu.
;; 
;; LISPDIR NOTE: replace the "(require 'crypt)" that appear in lispdir.el (the
;; Lisp Code Directory formatter and apropos that uses ange-ftp and crypt for
;; transparent retrieval of LCD entries) with "(require 'crypt++)".  If the
;; symbolic link suggested above is used, then this is not needed.
;;
;; RMAIL NOTE: a patch is available in roebling.poly.edu:/pub/crypt++.patch
;; that uses "kill-fix.el" (or a built-in for Lucid Emacs) to preserve
;; crypt++'s buffer local variables.  These are normally wiped out when RMAIL
;; is run on an encrypted and/or compressed file because of a call to
;; `(kill-all-local-variables).' Since this patch requires "kill-fix.el" it is
;; not part of the standard distribution.  Thanks to Jeff Greif
;; <jmg@inference.com> for pointing out this error and suggestings this patch.


;; DESCRIPTION:
;;
;; The basic purpose of this package of Lisp functions is to recognize
;; automatically encrypted and encoded (i.e., compressed) files when they are
;; first visited or written.  The BUFFER corresponding to the file is decoded
;; and/or decrypted before it is presented to the user.  The file itself is
;; unchanged on the disk.  When the buffer is subsequently saved to disk, a
;; hook function re-encodes the buffer before the actual disk write takes
;; place.
;;
;; This package recognizes all sorts of compressed files by a magic number at
;; the beginning of these files but uses a heuristic to detect encrypted
;; files.  If you are asked for an encryption key for a file that is in fact
;; not encrypted, just hit RET and the file will be accepted as is, and the
;; crypt minor mode will not be entered.
;;
;; Other types of encoding programs may be added to crypt++ using the variable
;; `crypt-encoding-alist' which contains a table of encoding programs such as
;; compress, gzip (GNU zip), freeze, and compact.
;;
;; This new extended version of crypt now monitors the filename extensions of
;; buffers that are written out using write-file (C-x C-w).  If the filename
;; extension matches one of the extensions listed in `crypt-encoding-alist,'
;; then crypt++ will write the file out using the corresponding encoding
;; (compression) method. This is done whether or not the buffer originated
;; from a previously encoded (compressed) file.
;;
;; Thus, if the user is editing a file that may or may not have been encoded
;; originally (e.g., foobar.Z or foobar) and decides to write it to a
;; different file (e.g., barfoo or barfoo.z or barfoo.C).  Crypt++ will examine
;; the filename extension and write the buffer in plain format or an alternate
;; encoding (compression) format by searching through the entries in the table
;; of encoding methods `crypt-encoding-alist.'  This change in encoding state
;; is done automatically if the variable `crypt-auto-write-buffer' is t
;; otherwise the user is asked.


;; TO DO/KNOWN BUGS/HELP WANTED/APPLY WITHIN: 
;; 
;; All Users/hackers out there are strongly encouraged to pursue any of these
;; matters further (especially those that concern encryption and decryption!).
;; It is important to future programmers and modifiers of crypt++.el to know
;; about its perceived limitations.  Since necessity drives invention, users
;; who find any of the following features of crypt++.el annoying are asked to
;; make suggestions and send patches (again, especially those that concern
;; encryption and decryption!).
;; 
;; * currently crypt++ assumes that if a file is both encrypted and encoded
;;   (i.e., compressed) that the order in which it was done was encryption
;;   first _then_ compression.  As has been pointed by many people compression
;;   following encryption is useless since the encrypted file is basically
;;   random.  On the other hand, many agree that doing encryption _following_
;;   compression is better since it makes it harder to crack the encryption.
;;   We would like to make the ordering of these two user-configurable or if
;;   nothing else change the order.
;; 
;;   Having read the above however, Francois Pinard <pinard@iro.umontreal.ca> 
;;   writes that encryption following compression may not be harder to crack 
;;   since "the fact that the first few uncrypted bytes are expected (the 
;;   compress signature) facilitates a serious attempt at uncrypting." 
;; 
;; * get insert-file, write-region, and append-to-file to handle encoded and 
;;   encrypted files.  There is a test version of crypt++-insert.el available
;;   at roebling.poly.edu as well as an interesting low-level encoding package
;;   by Jay Adams <jka@ece.cmu.edu> called jka-compr.el which might address
;;   some of these issues.  crypt++-insert will ultimately be incorporated
;;   into crypt++.  We encourage hackers out there to come up with crypt++
;;   versions of write-region and append-to-file.  The difficulty is creating
;;   versions that mimic the originals as closely as possible.
;;
;; * instead of using call-process-region (which can fail badly if the region 
;;   is large and there's not much room in /tmp), write the region to a temp 
;;   file (with a customisable location) and use call-process directly.
;;
;; * users have mentioned trouble using crypt++ and hilit simultaneously since 
;;   the functions in write-file-hook for both write the file to disk and
;;   return `t'.  A possible solution is to have one of them write to a
;;   scratch buffer instead of to disk and return `nil' and then allow the
;;   other to do its work on the scratch buffer and write it to disk.  Thanks
;;   to Wayne Folta <folta@cs.UMD.EDU> and Amir J Katz <amir@matis.ingr.com>.
;;   It would be nice to have another way in emacs to have an
;;   after-write-file-hook and a before-write-file-hook of some sort.
;;  
;; * another possible source of trouble is with encryption (and encoding) 
;;   programs sticking verbose output into buffers prior to being written to
;;   disk.  This was definitely occurring with gzip because of --verbose in
;;   the GZIP environment variable and is solved/hidden with the --quiet
;;   switch.  However, I suspect that some encryption problems out there are
;;   capable of similar things so the user should be careful.
;; 
;; * integrating crypt++ with a backgrounding package such as Olin Shivers' 
;;   `background.el' might be useful too.  thanks to Mark Borges 
;;   <mdb@noaacrd.Colorado.EDU> for suggesting this.
;; 
;; * performing encode-buffer or crypt-buffer and then saving the file may 
;;   cause errors.  it is better to toggle encoded-mode (or crypt-mode) and
;;   simply save the file.  encode-buffer and crypt-buffer are no longer 
;;   interactive (as of 2.37).


;; user definable variables

(defvar crypt-encryption-program "crypt"
  "*Name of executable file to be used for encryption (see also the variable
`crypt-decryption-program').

DES users should set this to \"des\" and PGP users should set this to \"pgp\"

See also the variable `crypt-encryption-args'

Default: \"crypt\"")

(defvar crypt-decryption-program "crypt"
  "*Name of executable file to be used for decryption (see also the variable
`crypt-encryption-program').

DES users should set this to \"des\" and PGP users should set this to \"pgp\"

See also the variable `crypt-decryption-args'

Default: \"crypt\"")

(defvar crypt-encryption-args nil
  "*Arguments to be passed to the encryption command.  May be a string or a
list of strings or nil.

DES users should set this to the string \"-ek\" or to the list

        '\(\"-e\" \"-k\"\)

and set the variable `crypt-encryption-program' to \"des\".

PGP users should set this to the list

        '\(\"+batchmode\" \"+verbose=0\" \"-c\" \"-f\" \"-z\"\)

and set the variable `crypt-encryption-program' to \"pgp\".

Users of most versions of `crypt' will want this to be `nil'

Default: `nil'")

(defvar crypt-decryption-args nil
  "*Arguments to be passed to the decryption command.  May be a string or a
list of strings or nil.

DES users should set this to the string \"-dk\" or to the list

        '\(\"-d\" \"-k\"\)

and set the variable `crypt-decryption-program' to \"des\".

PGP users should set this to the list

        '\(\"+batchmode\" \"+verbose=0\" \"-f\" \"-z\"\)

and set the variable `crypt-decryption-program' to \"pgp\".

Users of most versions of `crypt' will want this to be `nil'

Default: `nil'") 

(defvar crypt-auto-decode-buffer t
  "*t value means that the buffers associated with encoded files will
be decoded automatically, without requesting confirmation from the user.
`nil' means to ask before doing the decoding.
Default: t")

(defvar crypt-auto-write-buffer nil
  "*t value means buffers to be written to files ending in extensions
matching those in `crypt-encoding-alist' will be encoded automatically,
without requesting confirmation from the user. `nil' means to ask before doing
this encoding. Likewise, buffers originating from encoded files to be written
to different files ending in extensions that do not match any of those in
`crypt-encoding-alist' will be written in plain format automatically, without
requesting confirmation from the user. `nil' means to ask before doing this
decoding.

Default: nil")

(defvar crypt-auto-write-buffer-encrypted nil
  "*t value enables automatic encryption of files whose filename extension is
matched by `crypt-encryption-file-extension,' a value of `nil' means there
will be a query.  See also `crypt-auto-write-buffer.'

Default: nil")

(defvar crypt-query-if-interactive t
  "*t value means that (if needed) the user will be queried when saving a
buffer in which `encoded-mode' was called interactively .  A value of `nil'
means that even if the filename extension is plain (i.e., is not one of the
file-extensions listed in `crypt-encoding-alist') the file will be written in
an encoded format without asking.

This variable is designed for users that edit a plain file (with plain
extension) and then toggle `encoded-mode' and do not wish to be queried every
time that they save the buffer.

NOTE: if encoded-mode was not called interactively (the usual scenario) then
the value of this variable has no effect on how the buffer is written to disk.
In such a case `crypt-no-extension-implies-plain' is then the relevant
variable.

Default: t")

(defvar crypt-no-extension-implies-plain t
  "*t value means that if file ends in an extension that does not match one of
the file-extensions listed in `crypt-encoding-alist' then crypt++ will assume
that writing of the file out in plain format _may_ be desired.  If in addition
`crypt-auto-write-buffer' is `t' then any file ending in a plain extension
will be automatically written in plain format, otherwise the user will be
queried.

A value of `nil' for this variable means that users who like to work with
encoded (compressed) files without file extensions may do so without being
queried each time they save the file.

NOTE: (1) this does not effect find-file (C-x C-f) since that works with a
magic regexp.  (2) there is no way for crypt++ to distinguish between
write-file and save-buffer so setting this to `nil' will mean that neither 
C-x C-w nor C-x C-s will query.

Default: t")

(defvar crypt-freeze-vs-fortran t
  "*t values means that the .F file extension denotes a frozen file
rather than a Fortran file.
Default: t")

(defvar crypt-compact-vs-C++ nil
  "*t values means that the .C file extension denotes a compacted file
rather than a C++ file.
Default: nil")

(defvar crypt-encoding-alist
  (append
   (list '(compress "\037\235" "\\(\\.Z\\)$"
                    "compress" "uncompress"
                    "Compress"))
   (list '(gzip "\037\213" "\\(\\.z\\)$"
                "gzip --quiet" "gzip --decompress --quiet"
                "Zip"))
   (and crypt-freeze-vs-fortran
        (list '(freeze "\037\236\\|\037\237" "\\(\\.F\\)$"
                       "freeze" "unfreeze"
                       "Freeze")))
   (and crypt-compact-vs-C++
        (list '(compact "\377\037" "\\(\\.C\\)$"
                        "compact" "uncompact"
                        "Compact")))
   ;; Add new elements here ...
   )
  "*A list of elements describing the encoding methods available, each of
which looks like

        \(ENCODING-TYPE MAGIC-REGEXP FILE-EXTENSION
                        ENCODE-PROGRAM DECODE-PROGRAM
                        MINOR-MODE
                        \)

ENCODING-TYPE is a symbol denoting the encoding type.  Currently known
encodings are (compress compact freeze gzip).

MAGIC-REGEXP is a regexp that matches the magic number at the
beginning of files encoded with ENCODING-TYPE.

FILE-EXTENSION is a string denoting the file extension usually
appended the filename of files encoded with ENCODING-TYPE.

ENCODE-PROGRAM is a string denoting the name of the program used to
encode files plus any arguments.

DECODE-PROGRAM is a string denoting the name of the program used to
decode files plus any arguments.

MINOR-MODE is a string denoting the name for the encoded minor mode as 
it will appear in the mode line.
")

(defvar crypt-ignored-filenames nil
  "*list of filenames, as regular expressions, for which conversion to plain
format from encoded (compressed) format should not be attempted.  That is, a
filename with a plain extension and `buffer-save-encoded' equal to `t' that is
matched by one of the elements in this list will be saved in encoded
(compressed) format without query.

This variable is provided for users that want to compress their incoming mail
for RMAIL and VM.  These programs look, respectively, for files `RMAIL' and
`INBOX' in order to store incoming mail.  Therefore, for example, the gzip
extension on `RMAIL.z' and `INBOX.z' needs to be removed.  This means that by
default crypt++ will query about saving in plain format every time exiting
RMAIL and VM.  This query can be eliminated for these special files by setting
this as follows

    \(setq crypt-ignored-filenames '\(\"INBOX$\" \"RMAIL$\"\)\)

Default: `nil'
")

(defvar crypt-default-encoding "gzip"
  "*string representing the default encoding type for use in an interactive
call of `encode-buffer.'  This must match one of the elements of the table
`crypt-encoding-alist.'  Will be modified by `crypt-get-encoding-type.'
Default: \"gzip\"")

(defvar crypt-never-ever-decrypt nil
  "*t means that crypt++ will never (ever) try to decrypt a buffer.  See also
`crypt-magic-regexp[-inverse].'

Default: nil")

(defvar crypt-encryption-file-extension nil
  "\
*Regular expression string denoting the file extension usually appended the
filename of files encrypted with `crypt-encryption-program.'  A value of `nil'
means no check of file extension is made when writing out files.

example: \"\\\\.encpt$\"

Default: `nil'
")

;; Encrypted files have no magic number, so we have to hack a way of
;; determining which new buffers start in crypt mode.  The current setup is
;; that we use only buffers that match crypt-magic-regexp very close to
;; beginning of buffer and that do NOT match crypt-magic-regexp-inverse.
;; Currently crypt-magic-regexp matches non-ASCII characters and
;; crypt-magic-regexp-inverse will match Sun OS, 4.x BSD, and Ultrix
;; executable magic numbers, so binaries can still be edited (heh)
;; without headaches.

(defvar crypt-magic-regexp "[\200-\377]"
  "\
Regexp that must match very close to the beginning of an encrypted
buffer.

This may also be some elisp expression to be evaluated at (point-min) that
will return `t' for an encrypted buffer.  If this is set to `nil' then crypt++
will never try to decrypt a buffer.  See also `crypt-never-ever-decrypt.'

See also `crypt-magic-regexp-inverse'.
")

(defvar crypt-magic-regexp-inverse
  "^\\(..\\)?\\([\007\010\013]\001\\|\001[\007\010\013]\\)"
  "\
Regexp that must _NOT_ match very close to the beginning of an encrypted
buffer.

This may also be some elisp expression to be evaluated at (point-min) that
will return `t' for a NON-encrypted buffer.  If this is set to `t' then
crypt++ will never try to decrypt a buffer. See also
`crypt-never-ever-decrypt.'

See also `crypt-magic-regexp'.
")


;; buffer locals

(defvar buffer-save-encrypted nil
  "*Non-nil means that when this buffer is saved it will be written out
encrypted, using the commands in variables `crypt-encryption-program' and
`crypt-decryption-program.'  Automatically local to all buffers.")
(make-variable-buffer-local 'buffer-save-encrypted)

(defvar buffer-encryption-key nil
  "*Key to use when encrypting the current buffer, prior to saving it.
Automatically local to all buffers.")
(make-variable-buffer-local 'buffer-encryption-key)

(defvar buffer-save-encoded nil
  "*Non-nil means that when this buffer is saved it will be written out
encoded with ENCODING-TYPE, as with the ENCODING-PROGRAM command.
Automatically local to all buffers.")
(make-variable-buffer-local 'buffer-save-encoded)

(defvar buffer-encoding-type nil
  "*Non-nil means that this buffer is encoded with ENCODING-TYPE.
Automatically local to all buffers.")
(make-variable-buffer-local 'buffer-encoding-type)

(defvar buffer-interactive-mode nil
  "t value means that `encoded-mode' was called interactively. otherwise,
`nil'.  Almost always this will be `nil'.  Automatically local to all
buffers.")
(make-variable-buffer-local 'buffer-interactive-mode)


;; defuns that work on the encoding-alist

(defun encoding-magic-regexp (encoding-type)
  "Returns a regexp that matches the magic number at the beginning of files
encoded with ENCODING-TYPE."
  (let ((elt (assoc encoding-type crypt-encoding-alist)))
    (and elt
         (nth 1 elt))))

(defun encoding-file-extension (encoding-type)
  "Returns a regexp that matches the file-extension typically associated with
files encoded with ENCODING-TYPE."
  (let ((elt (assoc encoding-type crypt-encoding-alist)))
    (and elt
         (nth 2 elt))))

(defun encoding-encode-program (encoding-type)
  "Returns a string denoting the name of the program used to encode files
encoded with ENCODING-TYPE."
  (let ((elt (assoc encoding-type crypt-encoding-alist)))
    (and elt
         (nth 3 elt))))

(defun encoding-decode-program (encoding-type)
  "Returns a string denoting the name of the program used to decode files
encoded with ENCODING-TYPE."
  (let ((elt (assoc encoding-type crypt-encoding-alist)))
    (and elt
         (nth 4 elt))))

(defun crypt-buffer-save-name (encoding-type)
  "Returns a variable name. t means that when the buffer is saved it will be
written out using its decoding program.  Automatically local to all buffers."
  (intern (concat "buffer-save-" (symbol-name encoding-type))))


;; Create a buffer-local variable for each type of encoding.
;; These variables are used to trigger the minor mode names.

(defvar crypt-minor-mode-encrypted
  '(buffer-save-encrypted " Crypt")
  "Minor mode alist entry for encrypted buffers.")

(defvar crypt-minor-mode-alist
  (append
   (list crypt-minor-mode-encrypted)
   (mapcar
    (function
     (lambda (element)
       (let ((variable (crypt-buffer-save-name (car element))))
         (make-variable-buffer-local variable)
         (list variable (concat " " (nth 5 element))))))
    crypt-encoding-alist))
"Alist derived from `crypt-encoding-alist' containing encoded minor modes.")


(defmacro save-point (&rest body)
  "Save value of point, evaluate FORMS, and restore value of point.
If the saved value of point is no longer valid go to (point-max).
This macro exists because, save-excursion loses track of point during
some types of deletions."
  (let ((var (make-symbol "saved-point")))
    (list 'let (list (list var '(point)))
          (list 'unwind-protect
                (cons 'progn body)
                (list 'goto-char var)))))


(defun crypt-find-file-hook ()
  "Hook run for decoding and/or decrypting the contents of a buffer. Part of
find-file-hooks"
  (let ((buffer-file-name buffer-file-name)
        (old-buffer-file-name buffer-file-name)
        (old-buffer-modified-p (buffer-modified-p))
        found
        elt regexp
        encrypted encoded
        case-fold-search buffer-read-only)

    (save-point
     (save-restriction
       (widen)
       (goto-char (point-min))

       ;; We can reasonably assume that either compaction or compression will
       ;; be used, or neither, but not both.

       ;; find the type of compression

       (let ((alist crypt-encoding-alist))
         (while (and alist (setq elt (car alist)) (not found))
           (if (looking-at (nth 1 elt))
               (progn (setq buffer-encoding-type (nth 0 elt))
                      (setq found t))
             ;; decrement 
             (setq alist (cdr alist)))))

       ;; do we have to decode? if not move on
       (if (and found 
                (or crypt-auto-decode-buffer
                    (y-or-n-p (format "Decode %s? "
                                      (buffer-name)))))
           (progn
             (message "Decoding %s..." (buffer-name))
             (encode-buffer (current-buffer) t)

             ;; We can not actually go into encoding mode yet because the
             ;; major mode may change later on and blow away all local
             ;; variables (and thus the minor modes).  So we make a note to go
             ;; into encoding mode later.

             (setq encoded buffer-encoding-type)

             ;; here we strip the encoded file's extension so that later we
             ;; can set the buffer's major mode based on this modified name
             ;; instead of the name with the extension.

             (if (string-match (encoding-file-extension buffer-encoding-type)
                               buffer-file-name)
                 (setq buffer-file-name
                       (substring buffer-file-name 0 (match-beginning 1))))

             (if (not (input-pending-p))
                 (message "Decoding %s... done" (buffer-name)))))

       ;; Now peek at the file and see if it still looks like a binary file.
       ;; If so, try the crypt-magic-regexp-inverse against it and if it FAILS
       ;; we assume that this is an encrypted buffer.

       (cond (

              (and (not (eobp))

                   ;; Check for existence of crypt-magic-regexp
                   (if (stringp crypt-magic-regexp)
                       (re-search-forward crypt-magic-regexp
                                          (min (point-max) 15) t)
                     (eval crypt-magic-regexp))

                   (goto-char (point-min))

                   ;; Check for absence of crypt-magic-regexp-inverse
                   (not (if (stringp crypt-magic-regexp-inverse)
                            (re-search-forward crypt-magic-regexp-inverse
                                               (min (point-max) 15) t)
                          (eval crypt-magic-regexp-inverse)))

                   ;; shut off crypt mode if `crypt-never-ever-decrypt' is `t'
                   ;; - thanks to Nelson Minar <nelson@reed.edu>. 
                   ;; this is equivalent to user setting `crypt-magic-regexp' 
                   ;; to `nil' and/or `crypt-magic-regexp-inverse' to `t'
                   (not crypt-never-ever-decrypt))
              
              (if (not buffer-encryption-key)
                  (call-interactively 'set-encryption-key))

              ;; if user did not enter a key, turn off crypt mode.  good for
              ;; binaries that crypt-magic-regexp-inverse doesn't recognize.
              ;; -- thanx to Paul Dworkin (paul@media-lab.media.mit.edu)
              ;; can now shut off entirely if `crypt-never-ever-decrypt' is 
              ;; `t' see above - lrd.

              (if (equal buffer-encryption-key "")

                  (message "No key given, buffer %s assumed normal."
                           (buffer-name))

                (progn
                  (message "Decrypting %s..." (buffer-name))
                  
                  (crypt-buffer buffer-encryption-key nil)
                  
                  ;; Tuck the key away for safe keeping since setting the major
                  ;; mode may well blow it away.  all buffer-locals are 
                  ;; destroyed.
                  
                  (setq encrypted buffer-encryption-key)
                  
                  (if (not (input-pending-p))
                      (message "Decrypting %s... done" (buffer-name)))))

              ))
       ))

    ;; OK, if any changes have been made to the buffer we need to rerun the
    ;; code the does automatic selection of major mode.

    (cond (

           (or encoded encrypted)
           (set-auto-mode)
           (hack-local-variables)

           ;; Now set our own minor modes.
           (if encoded
               ;; recover encoding type (may have been smashed by major mode)
               ;; and toggle encoded mode
               (progn (setq buffer-encoding-type encoded)
                      (encoded-mode 1)))

           (if encrypted
               ;; recover encryption key (may have been smashed by major mode)
               ;; and toggle crypt mode
               (progn (setq buffer-encryption-key encrypted)
                      (crypt-mode 1)))

           ;; Restore buffer file name now, so that lock file entry is removed
           ;; properly.

           (setq buffer-file-name old-buffer-file-name)

           ;; Restore buffer modified flag to its previous value.  This will
           ;; also remove the lock file entry for the buffer if the previous
           ;; value was nil; this is why buffer-file-name had to be manually
           ;; restored above.

           (set-buffer-modified-p old-buffer-modified-p)))))


;; This function should be called ONLY as a write-file hook.
;; Odd things will happen if it is called elsewhere.

(defun crypt-write-file-hook ()
  
  "Writes out file, if need be, in a non-plain format. Note this terminates
the calls in write-file-hooks so should definitely be at the end of that list."
  
  ;; We flag a buffer to be written out in encoded form if the file ends in
  ;; one of the file-extensions in crypt-encoding-alist. Conversely, we write
  ;; out a buffer as a plain file if it does _not_ end in one of these
  ;; file-extensions even if buffer-save-encoded is non-`nil'.
  
  (let ((alist crypt-encoding-alist)
        case-fold-search found elt)
    
    ;; search through the file name extensions for a match
    (while (and alist (setq elt (car alist)) (not found))
      (if (string-match (nth 2 elt) buffer-file-name)
          (setq found t)
        ;; decrement
        (setq alist (cdr alist))))
    
    ;; did we find a match? 
    (if found 
        
        ;; file name ends in a very provocative extension

        ;; check to see if we should write as an encoded file
        (if buffer-save-encoded

            ;; already encoded - do the methods of encoding match? - if not 
            ;; then change the method of encoding
            (if (and 
                 (not (eq (nth 0 elt) buffer-encoding-type))
                 (or crypt-auto-write-buffer
                     (y-or-n-p (concat "write file using " (nth 3 elt) "? "))))

                ;; case one 
                ;; turn off original encoding and turn on new encoding
                (progn (encoded-mode -1)
                       (setq buffer-encoding-type (nth 0 elt))
                       (encoded-mode 1)))
          
          ;; was a plain file
          (if (or crypt-auto-write-buffer
                  (y-or-n-p (concat "write file using " (nth 3 elt) "? ")))

              ;; case two
              ;; turn on encoding flags and _then_ the minor mode
              (progn (setq buffer-save-encoded t)
                     (setq buffer-encoding-type (nth 0 elt))
                     (encoded-mode 1))))
      
      ;; no match - a plain-jane file extension - but if the encoded flag is
      ;; non-`nil' then the user may really want it written out in plain
      ;; format so we must override this flag
      (if (and buffer-save-encoded
               
               ;; search the list of files to be ignored
               ;; if `crypt-ignored-filenames' is `nil' then this let form 
               ;; will return `t'.  if a match is found this form will return 
               ;; `nil'.  otherwise it will return `t'.
               (let ((tlist crypt-ignored-filenames)
                     case-fold-search found elt)

                 ;; search through the list of filenames for a match
                 (while (and tlist (setq elt (car tlist)) (not found))
                   (if (string-match elt buffer-file-name)
                       (setq found t)
                     ;; decrement
                     (setq tlist (cdr tlist))))
                 
                 ;; return `t' if we can _not_ find a match
                 (not found))

               ;; if `encoded-mode' was called interactively, then there is a
               ;; high probability that no matter what the file name extension
               ;; the user wishes to write the file out in some encoded format
               ;; thanks to Kimball Collins <kpc@ptolemy.arc.nasa.gov> for
               ;; pointing out the need for this.  unfortunately, still can 
               ;; not distinguish between write-file and save-buffer.  in the 
               ;; former the user may want to write in plain format (or indeed 
               ;; some other format).
               
               (if buffer-interactive-mode
                   ;; interactive
                   crypt-query-if-interactive 
                 ;; non-interactive but still may want encoded format
                 crypt-no-extension-implies-plain)

               (or crypt-auto-write-buffer
                   (y-or-n-p "write as a plain file? ")))

          ;; case three
          ;; turn off the minor mode and _then_ the flags
          (progn (encoded-mode -1)
                 (setq buffer-save-encoded nil)
                 (setq buffer-encoding-type nil)))))

  (if (and

       ;; maybe file ends with provocative extension w.r.t. encryption?
       (stringp crypt-encryption-file-extension) ; may be `nil'
       (string-match crypt-encryption-file-extension buffer-file-name)
       
       ;; match of filename extension - is file in plain format?
       (not buffer-save-encrypted)
       
       ;; query?
       (or crypt-auto-write-buffer-encrypted
           (y-or-n-p "write as an encrypted file? ")))

      (progn
        ;; set password and toggle crypt-mode
        (call-interactively 'set-encryption-key)
        (crypt-mode 1)))

  ;; Now decide whether or not we need to continue with this defun. Does the
  ;; buffer need to be saved in a non-plain form?  If not then writing is not
  ;; done here but later in the write-file-hooks (probably at the end).

  (if (or buffer-save-encoded buffer-save-encrypted)
      
      (save-excursion
        (save-restriction
          (let 
              
              ;; BINDINGS
              ((copy-buffer (get-buffer-create " *crypt copy buffer*"))
               (selective-display selective-display)
               (buffer-read-only))
            
            ;; FORMS
            (copy-to-buffer copy-buffer 1 (1+ (buffer-size)))
            (narrow-to-region (point) (point))
            
            (unwind-protect
                
                (progn
                  (insert-buffer-substring copy-buffer)
                  (kill-buffer copy-buffer)
                  
                  ;; selective-display non-`nil' means we must convert
                  ;; carriage returns to newlines now, and set
                  ;; selective-display temporarily to nil.
                  
                  (cond (selective-display
                         (goto-char (point-min))
                         (subst-char-in-region (point-min) (point-max) ?\r ?\n)
                         (setq selective-display nil)))
                  
                  (cond
                   (buffer-save-encrypted
                    ;; check for password
                    (if (not buffer-encryption-key)
                        (call-interactively 'set-encryption-key))
                    (if (null buffer-encryption-key)
                        (error "No encryption key set for buffer %s"
                               (buffer-name)))
                    (if (not (stringp buffer-encryption-key))
                        (error "Encryption key is not a string"))
                    (message "Encrypting %s..." (buffer-name))
                    (crypt-buffer buffer-encryption-key t)))
                  
                  (cond
                   (buffer-save-encoded
                    (message "Encoding %s..." (buffer-name))
                    (encode-buffer)))
                  
                  ;; write buffer/region to disk
                  (write-region (point-min) (point-max) buffer-file-name nil t)
                  (delete-region (point-min) (point-max))
                  (set-buffer-modified-p nil)
                  
                  ;; return t so that basic-save-buffer will
                  ;; know that the save has already been done.
                  
                  ;; NOTE: this TERMINATES write-file-hooks so any hooks
                  ;; following crypt-write-file-hook will not be executed
                  
                  t )
              ;; unwind...sit back...take a load off...have a beer
              ;; If the crypted stuff has already been removed
              ;; then this is a no-op.
              (delete-region (point-min) (point-max))))))))

              
;;;; Defuns that do the actual decoding-encoding and decryption-encryption

;;; ENCRYPTING

(defun crypt-region (start end encrypt key)

  "Encrypt/decrypt the text in the region. From a program, this function takes
four args: START, END, ENCRYPT and KEY. When called interactively START and
END default to point and mark \(START being the lesser of the two\), KEY is
prompted for. If ENCRYPT is t encryption is done otherwise decrypt is done
using contents of variables `crypt-encryption-program' and
`crypt-decryption-program.'"

  (interactive
   (progn
     (barf-if-buffer-read-only)
     (list (region-beginning) (region-end)
      (y-or-n-p "Encrypt? ") 
      (read-string-no-echo "Crypt region using key: "))))

  (save-point

   ;; we define the PROGRAM as the variables `crypt-(en|de)cryption-program'
   ;; these should be just the name of the executable and should _not_ contain
   ;; any arguments.  `call-process-region' would be confused if we tried to
   ;; pass the arguments as part of the PROGRAM.  the arguments are passed
   ;; through `crypt-(en|de)cryption-args'

   ;; thanks to Joe Ilacqua <spike@world.std.com> and others for pointing out
   ;; an error that occurs with some encryption programs (e.g., the crypt from
   ;; Sun Microsystems, HPUX-8, and BSD) if crypt-(en|de)cryption-args is
   ;; `"".'  allow `nil' values for argument.

   (if encrypt
       ;; encrypt region
       (if (or (not crypt-encryption-args) (equal "" crypt-encryption-args))
           ;; nil or "" args - don't pass
           (call-process-region start end crypt-encryption-program t t nil key)
         ;; check whether the args are in the form of a list
         (if (listp crypt-encryption-args)
             ;; list of args - must use apply
             (apply 'call-process-region
                    (append (list start end crypt-encryption-program t t nil)
                            crypt-encryption-args (list key)))
           (call-process-region start end crypt-encryption-program t t nil
                                crypt-encryption-args key)))

     ;; decrypt region
     (if (or (not crypt-decryption-args) (equal "" crypt-decryption-args))
         ;; nil or "" args - don't pass
         (call-process-region start end crypt-decryption-program t t nil key)
       ;; check whether the args are in the form of a list
       (if (listp crypt-decryption-args)
           ;; list of args - must use apply
           (apply 'call-process-region
                  (append (list start end crypt-decryption-program t t nil)
                          crypt-decryption-args (list key)))
         (call-process-region start end crypt-decryption-program t t nil
                              crypt-decryption-args key))))))
     
(defun crypt-buffer (key encrypt &optional buffer)

  "Using KEY, if prefix arg (or ENCRYPT non-nil from a program), then encrypt
BUFFER \(defaults to the current buffer\), otherwise decrypt.

Use crypt-mode to encrypt an entire buffer interactively."

  (or buffer (setq buffer (current-buffer)))
  (save-excursion (set-buffer buffer)
                  (crypt-region (point-min) (point-max) encrypt key)))


;;; ENCODING

(defun encode-region (start end &optional undo)

  "Encode the text in the region. From a program, this function takes three
args: START, END and UNDO. When called interactively START and END default to
point and mark \(START being the lesser of the two\).  Prefix arg \(or
optional second arg non-nil\) UNDO means decode."

  (interactive "*r\nP")

  ;; if called interactively then we may need to determine the encoding type
  (if (and (interactive-p) (not buffer-encoding-type))
      (crypt-get-encoding-type))

  (save-point

   ;; we define the PROGRAM as `shell-file-name' since the variables
   ;; `encoding-(en|de)code-program' contain arguments in addition to the name
   ;; of the executable.  call-process-region would be confused if we tried to 
   ;; define the PROGRAM as `encoding-(en|de)code-program' because of these 
   ;; arguments.
   (call-process-region
    start end shell-file-name t t nil "-c"
    (if undo (encoding-decode-program buffer-encoding-type)
      (encoding-encode-program buffer-encoding-type)))

   (cond ((not undo)
          (goto-char start)
          (let (case-fold-search)
            (if (not (looking-at (encoding-magic-regexp buffer-encoding-type)))
                (error "%s failed!" (if undo "Decoding" "Encoding"))))))))

(defun encode-buffer (&optional buffer undo)

  "Encode BUFFER \(defaults to the current buffer\). Prefix arg \(or second
arg non-nil from a program) UNDO means decode.

Use encoded-mode to encode an entire buffer interactively."

  (or buffer (setq buffer (current-buffer)))
  (save-excursion (set-buffer buffer)
                  (encode-region (point-min) (point-max) undo)))



;;;; MODES

(defun crypt-mode (&optional arg)

  "Toggle crypt mode. With arg, turn crypt mode on iff arg is positive,
otherwise turn it off. In crypt mode, buffers are automatically encrypted
before being written.  If crypt mode is toggled and a key has been set for the
current buffer, then the current buffer is marked modified, since it needs to
be rewritten with \(or without\) encryption.

Entering crypt mode causes auto-saving to be turned off in the current buffer,
as there is no way \(in Emacs Lisp\) to force auto save files to be
encrypted."

  (interactive "P")
  (let ((oldval buffer-save-encrypted))
    (setq buffer-save-encrypted
          (if arg (> arg 0) (not buffer-save-encrypted)))

    (if buffer-save-encrypted
        ;; we are going to save as encrypted, we will turn off auto-saving.
        (progn
          ;; if an auto-save file already exists, then delete it
          (if (and (stringp buffer-auto-save-file-name)
                   (file-exists-p buffer-auto-save-file-name))
              (delete-file buffer-auto-save-file-name))
          ;; if the key is not set then ask for it.
          (if (not buffer-encryption-key)
              (call-interactively 'set-encryption-key))
          ;; turn-off auto-saving
          (auto-save-mode 0))

      ;; we are not going to save as encrypted, we will turn on auto-saving
      ;; but only if we are editing a file and the default says we should.
      (auto-save-mode (if (and auto-save-default buffer-file-name) 1 0)))

    (if buffer-encryption-key
        ;; set buffer-modified flag to `t' only if the mode has been changed, 
        ;; old code set unconditionally to `nil' if mode was not changed 
        ;; modification suggested by: Gerd Hillebrand <ggh@cs.brown.edu>
        (if (not (eq oldval buffer-save-encrypted))
            (set-buffer-modified-p t)))))


;;; originally `tek-symbol-alist-to-table' from tek-highlight.el
(defun crypt-symbol-alist-to-table (list)
  "Converts an alist of symbols to a table suitable for `completing-read.'
Called by `crypt-get-encoding-type'."
  (mapcar (function (lambda (x) (list (symbol-name (car x)))))
          list))

(defun crypt-get-encoding-type ()

  "Function called by encoded-mode and encode-region that will query the user
for the `buffer-encoding-type' using the contents of `crypt-encoding-alist'
and `crypt-default-encoding.'"
 
  ;; use poor man's gmhist (i.e., we could have used gmhist's
  ;; `completing-read-with-history-in' instead)
  (let (
        ;; find the encoding type desired by user
        (type
         (completing-read
          (concat "encoding type (? for list): [" crypt-default-encoding "] ")
          (crypt-symbol-alist-to-table crypt-encoding-alist))))
    
    ;; test length of object returned by `completing-read'
    (if (zerop (length type))
        
        ;; nothing there, i.e., user hit return -- use default
        (setq buffer-encoding-type (intern crypt-default-encoding))
      
      ;; use the value from mini-buffer and update the default value
      (setq buffer-encoding-type (intern type)
            crypt-default-encoding type))))

(defun encoded-mode (&optional arg)

  "Toggle encoded mode. With arg, turn encoded mode on iff arg is positive,
otherwise turn it off. In encoded mode, buffers are automatically encoded
before being written. If encoded mode is toggled, the current buffer is
marked modified, since it needs to be written with (or without) encoding.

Entering encoded mode causes auto-saving to be turned off in the current
buffer, as there is no way (in Emacs Lisp) to force auto save files to be
encoded."

  (interactive "P")

  ;; set flag indicating whether or not `encoded-mode' was called 
  ;; interactively
  (setq buffer-interactive-mode (interactive-p))

  ;; if called interactively then need to determine encoding type
  (if (and buffer-interactive-mode (not buffer-encoding-type))
      (crypt-get-encoding-type))

  ;; save old value of `buffer-save-encoded'
  (let ((oldval buffer-save-encoded))

    ;; set the variable `buffer-save-encoded' to `t' if the argument is 
    ;; positive, otherwise toggle its current value.
    (setq buffer-save-encoded
          (if arg (> arg 0) (not buffer-save-encoded)))

    ;; set the variable generated by `crypt-buffer-save-name' to the value
    ;; store in `buffer-save-encoded.'
    (set-variable (crypt-buffer-save-name buffer-encoding-type)
                  buffer-save-encoded)

    (if buffer-save-encoded
        ;; we are going to save as encoded, we will turn off auto-saving.
        (progn
          ;; if an auto-save file already exists, then delete it
          (if (and (stringp buffer-auto-save-file-name)
                   (file-exists-p buffer-auto-save-file-name))
              (delete-file buffer-auto-save-file-name))
          ;; turn-off auto-saving
          (auto-save-mode 0))

      ;; we are not going to save as encoded, we will turn on auto-saving but
      ;; only if we are editing a file and the default says we should.
      (auto-save-mode (if (and auto-save-default buffer-file-name) 1 0)))

    ;; have we toggled the mode? 

    ;; if yes, then mark buffer as modified.  if not, then leave
    ;; buffer-modified flag alone.

    ;; the old code previously set the variable `set-buffer-modified-p' to a
    ;; value of `t' if there was a mode change and (unconditionally) to `nil'
    ;; if there was not a mode change.

    ;; modification suggested by: Gerd Hillebrand <ggh@cs.brown.edu>

    (if (not (eq oldval buffer-save-encoded))
        (set-buffer-modified-p t))))


;;;; Additional crypt defuns 

(defun read-string-no-echo (prompt &optional confirm)
  
  "Read a string from the minibuffer, prompting with PROMPT. Optional second
argument CONFIRM non-nil means that the user will be asked to type the string
a second time for confirmation and if there is a mismatch, the process is
repeated.

           Line editing keys are --
             C-h, DEL      rubout
             C-u, C-x      line kill
             C-q, C-v      literal next"
  
  (catch 'return-value
    (save-excursion
      (let ((input-buffer (get-buffer-create " *password*"))
            char string help-form done kill-ring)
        (set-buffer input-buffer)
        (let ((cursor-in-echo-area t)
              (echo-keystrokes 0))
          (unwind-protect
              (while t
                (erase-buffer)
                (message prompt)
                (while (not (memq (setq char (read-char)) '(?\C-m ?\C-j)))
                  (if (setq form
                            (cdr
                             (assq char
                                   '((?\C-h . (delete-char -1))
                                     (?\C-? . (delete-char -1))
                                     (?\C-u . (delete-region 1 (point)))
                                     (?\C-x . (delete-region 1 (point)))
                                     (?\C-q . (quoted-insert 1))
                                     (?\C-v . (quoted-insert 1))))))
                      (condition-case error-data
                          (eval form)
                        (error t))
                    (insert char))
                  (message prompt))
                (cond ((and confirm string)
                       (cond ((not (string= string (buffer-string)))
                              (message
                               (concat prompt "[Mismatch... try again.]"))
                              (ding)
                              (sit-for 2)
                              (setq string nil))
                             (t (throw 'return-value string))))
                      (confirm
                       (setq string (buffer-string))
                       (message (concat prompt "[Retype to confirm...]"))
                       (sit-for 2))
                      (t (throw 'return-value (buffer-string)))))
            (set-buffer-modified-p nil)
            (kill-buffer input-buffer)))))))

(defun set-encryption-key (key &optional buffer)

  "Set the encryption KEY for BUFFER. KEY should be a string. BUFFER should be
a buffer or the name of one; it defaults to the current buffer.  If BUFFER is
in crypt mode, then it is also marked as modified, since it needs to be saved
with the new key."

  (interactive
   (progn
     (barf-if-buffer-read-only)
     (list
      (read-string-no-echo
       (format "Set encryption key for buffer %s: " (buffer-name))))))

  ;; for security reasons we remove `(set-encryption-key "password")' from the 
  ;; `command-history' list if called interactively.
  (if (interactive-p)
      (setq command-history (cdr command-history)))

  (or buffer (setq buffer (current-buffer)))

  (save-excursion
    (set-buffer buffer)
    (if (equal key buffer-encryption-key)
        (message "Key is identical to original, no change.")
      (setq buffer-encryption-key key)
      ;; don't touch the modify flag unless we're in crypt-mode.
      (if buffer-save-encrypted
          (set-buffer-modified-p t)))))


;; Install the hooks, then add the mode indicators to the minor mode alist.

;; Check that the hooks are not already installed.

;; Contributed by Ken Laprade <laprade@trantor.harris-atd.com>
;; Really should use some sort of add-hook - 16 Feb 93 - KCL
(or (and (listp write-file-hooks) (not (eq (car write-file-hooks) 'lambda)))
    (setq write-file-hooks (list write-file-hooks)))

(cond
 ((not (memq 'crypt-write-file-hook write-file-hooks))
  ;; make this hook last on purpose
  (setq write-file-hooks (append write-file-hooks
                                 (list 'crypt-write-file-hook))
        find-file-hooks (cons 'crypt-find-file-hook find-file-hooks)
        find-file-not-found-hooks (cons 'crypt-find-file-hook
                                        find-file-not-found-hooks))))

;; Check that the mode indicators are not already installed.

(cond
 ((not (memq crypt-minor-mode-encrypted minor-mode-alist))
  ;; add the mode indicators
  (setq minor-mode-alist (append crypt-minor-mode-alist
                                 minor-mode-alist))))


;;;; BUG REPORTS

;;; this section is provided for reports.
;;; adopted from Barry A. Warsaw's c++-mode.el

(defvar crypt-mailer 'mail
  "*Mail package to use to generate report mail buffer.")

(defconst crypt-help-address
  "dodd@roebling.poly.edu, rwhitby@research.canon.oz.au"
  "Address(es) accepting submission of reports on crypt++.el.")

(defconst crypt-maintainer "Larry and Rod"
  "First name(s) of people accepting submission of reports on crypt++.el.")

(defconst crypt-file "crypt++.el"
  "Name of file containing emacs lisp code.")

(defun crypt-submit-report ()
  "Submit via mail a report using the mailer in crypt-mailer, filename in
crypt-file, to address in crypt-help-address."
  (interactive)
  (funcall crypt-mailer)
  (insert crypt-help-address)
  (let ((case-fold-search t))
    (if (re-search-forward "^subject:[ \t]+" (point-max) 'move)
        (insert "Report on " crypt-file " version " crypt-version))
    (if (not (re-search-forward mail-header-separator (point-max) 'move))
        (progn (goto-char (point-max))
               (insert "\n" mail-header-separator "\n")
               (goto-char (point-max)))
      (forward-line 1)))
  (set-mark (point)) ;user should see mark change
  (insert "\n\n---------\n")
  (insert (emacs-version) "\n")
  (insert "code: " crypt-file ",v " crypt-version)
  ;;
  (insert "\n\n")
  (insert "current values of variables -- \n\n") 
  ;;
  (insert "  crypt-encryption-program: ")
  (insert (prin1-to-string crypt-encryption-program) "\n")
  ;;
  (insert "  crypt-decryption-program: ")
  (insert (prin1-to-string crypt-decryption-program) "\n")
  ;;
  (insert "  crypt-encryption-args: ")
  (insert (prin1-to-string crypt-encryption-args) "\n")
  ;;
  (insert "  crypt-decryption-args: ")
  (insert (prin1-to-string crypt-decryption-args) "\n")
  ;;
  (insert "  crypt-encryption-file-extension: ")
  (insert (prin1-to-string crypt-encryption-file-extension) "\n")
  ;;
  (insert "  crypt-auto-decode-buffer: ")
  (insert (prin1-to-string crypt-auto-decode-buffer) "\n")
  ;;
  (insert "  crypt-auto-write-buffer: ")
  (insert (prin1-to-string crypt-auto-write-buffer) "\n")
  ;;
  (insert "  crypt-auto-write-buffer-encrypted: ")
  (insert (prin1-to-string crypt-auto-write-buffer-encrypted) "\n")
  ;;
  (insert "  crypt-query-if-interactive: ")
  (insert (prin1-to-string crypt-query-if-interactive) "\n")
  ;;
  (insert "  crypt-no-extension-implies-plain: ")
  (insert (prin1-to-string crypt-no-extension-implies-plain) "\n")
  ;;
  (insert "  crypt-ignored-filenames: ")
  (insert (prin1-to-string crypt-ignored-filenames) "\n")
  ;;
  (insert "  crypt-never-ever-decrypt: ")
  (insert (prin1-to-string crypt-never-ever-decrypt) "\n")
  ;;
  (insert "  crypt-freeze-vs-fortran: ")
  (insert (prin1-to-string crypt-freeze-vs-fortran) "\n")
  ;;
  (insert "  crypt-compact-vs-C++: ")
  (insert (prin1-to-string crypt-compact-vs-C++) "\n")
  ;;
  (insert "  crypt-default-encoding: ")
  (insert crypt-default-encoding "\n")
  ;;
  (insert "\n  crypt-encoding-alist: ")
  (insert (prin1-to-string crypt-encoding-alist))
  (fill-region (save-excursion (beginning-of-line) (point)) (point))
  ;;
  (exchange-point-and-mark)
  (insert "\n" crypt-maintainer ",\n\n  ")
  (message "%s, please write the message, use C-c C-c to send" (user-login-name)))

;; provide this package as crypt++ as well as crypt
(provide 'crypt++)
(provide 'crypt)

