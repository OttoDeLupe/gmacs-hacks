(provide 'load-tmc-hacks)

;;----------------
;; This is version 19.25.03 of this file.
;; Changes since last time: 
;    1.  Fixed rmail-show-message-hook bug in rmail-autokill.el
;;   2.  jmincy@avid.com:  More hooks for comint-previous-similar-input.
;;   3.  jonny@Synopsys: fix to comint-previous-similar-input for 19.26
;;   4.  jonny@Synopsys: fix to rmail-autokill-internal for long message-id header
;;   5.  jmincy@avid.com:  fix automodes for c++ vs c
;;   6.  fixed .q automode for S
;;   Other misc cleanups.


;;----------------
;; This is a version of the TMC emacs hacks, updated for
;; emacs-19.25 by Gary Sabot (gary@sabot.com).

;; Dont forget to customize site.el and site-hacks.el for your site.

;; I updated this so that I could use it, but since it seems to 
;; be in decent shape, I'm distributing it to interested people.
;; It probably is buggy.  Don't complain to me.  If you like, send me 
;; fixes or accolades.

;; Things to do:

;; Replace TMC advise with new GNU advice, which can give advice to functions
;; before they are loaded, so you can eliminate some of the calls to require

;; Put a reasonable interface in for calling comint-write-input-ring 
;; and comint-read-input-ring interactively, and for naming the default
;; history files for lisp buffers, etc.

;; Maybe put old LISPM keybindings in for lisp mode.  
;; TMC people can add notes about what old functions have been replaced 
;; with (i.e. vc- family replaces old RCS stuff like co-current buffer)

;; Warn if this file is being loaded when .elc file is older than .el file
;; (load-library is a stupid function).


;; Sample .emacs file:

;;   ;; need to have your OWN login name here for v19 to inhibit message:
;;   (setq inhibit-startup-echo-area-message "gary")
;;   (setq debug-on-error t)
;;   
;;   (setq load-path (cons "/usr/local/lib/TMC-hacks-19/" load-path))
;;   
;;   
;;   ;; set variables that affect loading of tmc-hacks
;;   (setq *load-completion* t)
;;   (setq check-for-extended-command-bindings t)
;;   (setq *zmacs-compatibility* t)
;;   (setq *load-icomplete* t)
;;   (setq *mime-stuff* t)
;;   (setq *load-multiple-mail-buffers* t)
;;   
;;   ;; load hack libraries
;;   (load-library "load-tmc-hacks")
;;   
;;   (setq debug-on-error nil)
;;   (message "Done loading .emacs")







(require 'cl)		      ;common-lisp compatibility
(require 'dfs)		      ;this is to get dfs-load-n


;; TMC hacks use advice, eventually switch to newer advice module
(require 'advise)

;; load site specific variables saying where to find things.
(load-library "site")


;;;**************************************************
;;;  File compression
;;;**************************************************
;;set this to nil in your .emacs if you use APL, Hebrew, etc.
(defvar *load-crypt* t "*are files containing meta characters assumed to be encrypted")

(if *load-crypt*	   
    (require 'crypt++)
  (setq crypt-freeze-vs-fortran nil)
)


;;;**************************************************
;;;  Completion
;;;**************************************************
(defvar *load-completion* t "*load completion?")

(when *load-completion*
  ;; load the new completions and init them, as described in completion.el
  (load "completion")
  (initialize-completions)
)


;;;**************************************************
;;;  Byte compilation
;;;**************************************************

;;; Function to byte-compile every .el file in the current directory -- to
;;; be used to install the hacks.  fad Tue Apr 17 1990
(autoload 'byte-compile-directory "byte-compile-directory"
	  "Compile every .el file in current directory." t)



;;;**************************************************
;;;  tail stuff  --gary sabot Wed Nov 27 1991
;;;**************************************************

(autoload 'live-find-file "live" "View a file with \"tail -f\"" t)
;;   M-x live-find-file RET filename RET
(autoload 'tail "live" "Run life-filed-file on filename near cursor" t)



;;;**************************************************
;;; rolo-mode hacks -- fad Wed Aug  2 1989
;;;**************************************************
(autoload 'rolo "rolodex" "Edit rolodex file using rolo-mode" t)
(autoload 'rolo-mode "rolodex" "Put buffer into Rolo mode for editing rolodex files." t)
(autoload 'rolo-expand-name-to-email "rolodex" "Expand name before point
		to rolodex email entry." t)
(autoload 'rolo-find "rolodex" "Find an entry in your rolodex file." t)
;;; keys set in keymap.el



;;;**************************************************
;;; Keyboard bindings reminder
;;;**************************************************

;; Loading this next file make meta-x remind you of the keyboard
;; bindings for functions whose name you are 
;; laboriously M-x typing-out.  It normally prints its notifications in
;; the mode line, and they go away after a few seconds.
;; You can call M-x display-binding-statistics to get a summary
;; of all reminders displayed so far
;; To turn it on, you must include this in your .emacs
;;  (setq check-for-extended-command-bindings t)
;;  --gary
(when (and (boundp 'check-for-extended-command-bindings)
	   check-for-extended-command-bindings)
  (dfs-load-n "bindings-notifier"))



(defvar *load-mime-stuff* nil "*If this is T, then MIME is loaded")
(when *load-mime-stuff*
  ;emacs-mime-tools-is-moved-to-FTP.KYUTECH.AC.JP
  ; or anubis.ac.hmc.edu /pub/emacs/packages/metamail
  (autoload 'mime-mode "mime" "Minor mode for editing MIME message." t)
  (autoload 'rmail-show-mime		"rmailmime" "Show MIME messages."  t)
  (autoload 'rmail-convert-mime-header	"rmailmime" "Convert MIME header." nil)
  (setq rmail-message-filter 'rmail-convert-mime-header)
  (add-hook 'rmail-mode-hook '(lambda () 
			       (local-set-key "!" 'rmail-show-mime)))
  (add-hook 'mail-mode-hook '(lambda () (mime-mode)))

  (when window-system  ;ascii terminals cant fontify
    (setq mime-mode-hook
	  (list
	   (function
	    (lambda ()
	      ; why is this commented out?
	      ; (font-lock-mode 1)
	      (setq font-lock-keywords (list mime-tag-regexp))))))
    ))




;;;**************************************************
;;;  icomplete is neat
;;;**************************************************


(defvar *load-icomplete* nil "*If this is T, then icomplete is loaded")
(if *load-icomplete*
    (require 'icomplete))


(defvar *tmc-key-bindings* t "*If this is T, then the TMC hacks
  will include some key bindings.  Set to nil before loading this file
  if you don't want the TMC bindings.")

(defvar *use-S-bindings-and-mode* nil "*If this is T, then the .s
  files will be treated as program files for the S language, loading
  S-mode.  Also, *tmc-keybindings* will be set up for S-mode.  Otherwise,
  .s files are assembly code and assembler mode is used.")



;;;**************************************************
;;;  Subset of ZMACS behavior
;;;**************************************************

(defvar *zmacs-compatibility* nil "*If this is T, then the TMC hacks
  will try to approximate a lisp machine.  If it is nil, 
  then it will not try so hard.  Please set this before loading this file.
  (since it is a defvar, it will not override a previous value)")


(autoload 'find-unbalanced-parentheses "zmacs-stuff"
  "Check the buffer for unbalanced parentheses.  Stops at any that are 
unbalanced." 
  t)




(autoload 'switch-to-other-buffer "zmacs-stuff"
  "Switches to the other buffer, as determined by (other-buffer).
   With a non-null ARG (numeric prefix), switches to the nth buffer
   (1-origin) in the history.  As with ZMACS, 0 and 1 are special cases:
   0 means display a buffer list, while 1 means bury current buffer."
  t)


;if you like this:
;(global-set-key "\C-x2" 'split-window-vertically-other-buffer)
(autoload 'split-window-vertically-other-buffer "zmacs-stuff"
  "Split current window into two windows, one above the other.
This window becomes the uppermost of the two, and gets
ARG lines.  No arg means split equally.
This selects the (other-buffer) in the other window."
  t)


;;;**************************************************
;;;  Key bindings
;;;**************************************************

(when *zmacs-compatibility*
  (global-set-key "" 'electric-buffer-list)
  (global-set-key "\M-\C-l" 'switch-to-other-buffer)

  ;; I tried to fix this by putting it on comint-mode-hook,
  ;; but if you call some derivative mode like shell mode, it
  ;; copies the comint-mode-map and then calls its own hook.
  ;; Since I can't name all such derivative modes, do this:
  (require 'comint)
  (define-key comint-mode-map "\e\C-l" 'switch-to-other-buffer)
  (require 'rmail)
  (define-key rmail-mode-map "\e\C-l" 'switch-to-other-buffer)
  )


(when *tmc-key-bindings*

  ;; You may or may not want to do this globally:
  ;;(global-set-key "\C-z" 'comint-previous-similar-input)
  ;; Its nicer to do it mode by mode, below, but that will not handle other 
  ;; (or new) modes that build on comint, unless they are specifically
  ;; added
  (add-hook 'shell-mode-hook 
	    '(lambda () (local-set-key "\C-z" 'comint-previous-similar-input)))
  (add-hook 'inferior-lisp-mode-hook 
	    '(lambda () (local-set-key "\C-z" 'comint-previous-similar-input)))

  (when *use-S-bindings-and-mode*
    (add-hook 'inferior-S-mode-hook 
	      '(lambda () (local-set-key "\C-z" 'comint-previous-similar-input))))

  (add-hook 'telnet-mode-hook 
	    '(lambda () (local-set-key "\C-z" 'comint-previous-similar-input)))
  (add-hook 'inferior-scheme-mode-hook 
	    '(lambda () (local-set-key "\C-z" 'comint-previous-similar-input)))
  (add-hook 'inferior-prolog-mode-hook
	    '(lambda () (local-set-key "\C-z" 'comint-previous-similar-input)))
)


;;;**************************************************
;;;  Various variables
;;;**************************************************


(setq completion-ignored-extensions
      (append '(".vbin"  ".vbin2" ".v2bin" ".vsbin" ".2bin" 
		".sbin" ".sbin3" ".sbin4" ".vo" ".2o" ".4o" "..c" ".ibin"
		".ibin8" ".bin8") 
	      completion-ignored-extensions
	      ))

(put 'eval-expression 'disabled nil)
(setq default-major-mode 'text-mode)
(setq inhibit-startup-message t)
;; delete-old-versions is for emacs 19.27 and later
;; trim-versions-without-asking is for emacs 19.25 and earlier
(setq version-control t
      trim-versions-without-asking t
      delete-old-versions t
      kept-new-versions 4
      kept-old-versions 0)


(setq search-slow-speed 2400)
(setq search-slow-window-lines 5)
(setq scroll-step 4)


(setq c-tab-always-indent nil)
(setq require-final-newline t)

;;;;*********************************************** 
;;;; Auto-Fill On By Default
;;;;***********************************************


(add-hook 'text-mode-hook '(lambda () (auto-fill-mode 1)))
(add-hook 'mail-mode-hook '(lambda () (auto-fill-mode 1)))

(setq default-fill-column 70)


;;;;*********************************************** 
;;;; Patches to mail
;;;;***********************************************

(setq rmail-dont-reply-to-names "info-\\|thunko\\|bboard\\|staff\\|staff-ext")


(defvar *load-multiple-mail-buffers* nil
  "*If this is T, then use multiple mail buffers.")

(if *load-multiple-mail-buffers*
    (require 'sendmail-multiple-buffers))

(setq mail-aliases t) ;; Enable mail aliases from .mailrc



;;;;***********************************************
;;;; Patches to RMAIL
;;;;***********************************************

;;; RMAIL used to be in the dumped image.  Load it now for the
;;; benefit of users whose init files depend on it being there already.
;;; and so the advise below will work.
(require 'rmail)


;;; If you want RMAIL replies to automatically include the original,
;;; set this to t in your init file --gary
(defvar rmail-include-original-in-reply nil)

;; include original in reply
(advise rmail-reply :after (when rmail-include-original-in-reply
			     (mail-yank-original 3)))


(advise rmail :around
	(require 'rmail-autokill) ;;gary 5/6/91
	:do-it
			      ;set rmail-keep-backup to t in your .emacs if you
			      ;want to keep backups of rmail files -- not
			      ;recommended, as they get very big!  fad 1/11/89
	(defvar rmail-keep-backup nil "*If t, rmail buffers have same \
version-control value as user's other buffers.  
If nil, version-control is \"never\"")
	(if rmail-keep-backup
	    (kill-local-variable version-control))
	)


;;;if you want rmail to put your incoming mail some other place than ~/RMAIL,
;;;then put something of this form in your .emacs file:
;;;(setq rmail-file-name (expand-file-name "my-in-file"))

;;;if you want rmail to suggest a default file to save messages into
;;;other than ~/XMAIL, then put something of this form in your .emacs file:
;;;(setq rmail-default-rmail-file (expand-file-name "my-out-file"))




;;;**************************************************
;;;  Comint, shell, and friends
;;;**************************************************

(require 'comint)
;; can't autoload comint since it adds a hook, replace stuff below with
;; advice eventually
(dfs-load-n "comint-extra")
;(dfs-load-n "comint-isearch")


;; S mode autoloads, not destructive so always do them
(autoload 'S	   "S" "Start up an Splus interaction buffer" t)
(autoload 'S-mode  "S" "Go into S mode" t)

(when *use-S-bindings-and-mode*
  ;;override assembler mode for .s
  (setq auto-mode-alist (nconc '(("\\.s$" . S-mode) ("\\.S$" . S-mode) ("\\.q$" . S-mode))
			       auto-mode-alist)))


;; add other modes as well
(setq auto-mode-alist (nconc '(("\\.c\\+\\+$" . c++-mode))
			     auto-mode-alist))




;;;**************************************************
;;;  TAGS
;;;**************************************************


(defvar default-tags-file-name (concat "TAGS-" (substring emacs-version 0 5)))

(unless tags-file-name
  (setq tags-file-name (truename default-tags-file-name)))

;;this allows the TAGS tables to be fault tolerant, but doesn't
;; seem to work the first time find-tag is called; hence setting 
;; tags-file-name above.
(advise visit-tags-table :around
	(unless tags-file-name
	  (setq tags-file-name default-tags-file-name))
	(let ((tags-file-name tags-file-name))
	  (setq tags-file-name (truename tags-file-name))
	  :do-it))

(autoload 'build-gmacs-tag-table "build-gmacs-tag-table" "" t)


	  
;;;;*********************************************** 
;;;; Random hacks
;;;;***********************************************

(autoload 'font-big 	"font" "Set font to largest size if running X." t)
(autoload 'font-medium 	"font" "Set font to medium size if running X." t)
(autoload 'font-small 	"font" "Set font to small size if running X." t)
(autoload 'font-tiny 	"font" "Set font to smallest size if running X." t)
(autoload 'font-apl 	"font" "Set font to APL if running X." t)


(switch-to-buffer "*scratch*")


;;;---------------------------------------------------------------------------
;;; Site specific
;;;---------------------------------------------------------------------------

(dfs-load-n "site-hacks")


;;; Local Variables:
;;; comment-column:30
;;; comment-start: ";"
;;; End:
