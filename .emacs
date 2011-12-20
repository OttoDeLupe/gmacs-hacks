(put 'eval-expression 'disabled nil)

(setq blink-matching-paren t)
(setq truncate-partial-width-windows nil)

(global-font-lock-mode t)
(global-set-key "\C-x\C-b" 'electric-buffer-list)

;(fmakunbound 'c-mode)
;(makunbound 'c-mode-map)
;(fmakunbound 'c++-mode)
;(makunbound 'c++-mode-map)
;(makunbound 'c-style-alist)

(setq load-path (cons "c:/usr/emacs-91.31/tmc-hacks/" load-path))

(load "cc-mode")
;;(load "c:/usr/emacs-19.31/tmc-hacks/zmacs-stuff.elc")

(global-set-key "\M-\C-l" 'switch-to-other-buffer)

(setq font-lock-maximum-decoration '((c-mode . 3) (c++-mode . 3) (emacs-lisp-mode . 3)))


;;;
;;; Define all these fns and add them to the appropriate add-hook routines
;;; as the mode maps aren't created until they're used. 
;;;

(defun my-turn-on-font-lock ()
  (interactive "")
  (set-face-foreground 'bold "blue")
  (set-face-foreground 'bold-italic "red")
  (set-face-foreground 'italic "purple1")
  (set-face-foreground 'underline "darkgreen")
  (set-face-foreground 'default "black")
  (set-face-background 'region "lightsteelblue")
  (set-face-background 'highlight "turquoise")
  (set-face-underline-p 'bold t)
  (set-face-underline-p 'underline t)
  (set-face-foreground 'font-lock-comment-face "purple")
  (set-face-foreground 'font-lock-function-name-face "red")
  (set-face-foreground 'font-lock-keyword-face "blue")
  (set-face-foreground 'font-lock-reference-face "blue")
  (set-face-foreground 'font-lock-string-face "forestgreen")
  (set-face-foreground 'font-lock-type-face "black")
;;  (set-face-foreground 'font-lock-variable-name "red")
  (transient-mark-mode t)
  (line-number-mode 1)) 


(defun c-mode-setup ()
  (define-key c-mode-map "\C-m" 'newline-and-indent)
  (define-key c-mode-map "\M-\C-g" 'goto-line)
  (define-key c-mode-map "\M-\C-q" 'font-lock-fontify-buffer)
;  (c-set-offset 'statement-cont 0)
  (my-turn-on-font-lock))

(defun java-mode-setup ()
  (define-key java-mode-map "\C-m" 'newline-and-indent)
  (define-key java-mode-map "\M-\C-g" 'goto-line)
  (define-key java-mode-map "\M-\C-q" 'font-lock-fontify-buffer)
;  (c-set-offset 'statement-cont 0)
  (my-turn-on-font-lock))

(defun emacs-lisp-mode-setup ()
  (define-key emacs-lisp-mode-map "\C-m" 'newline-and-indent)
  (define-key emacs-lisp-mode-map "\M-\C-g" 'goto-line)
  (define-key emacs-lisp-mode-map "\M-\C-q" 'font-lock-fontify-buffer)
  (my-turn-on-font-lock))

(defun indented-text-mode-setup ()
  (define-key indented-text-mode-map "\C-m" 'newline-and-indent)
  (auto-fill-mode 1))

(defun perl-mode-setup ()
  (my-turn-on-font-lock))

(defun mail-mode-setup ()
  (my-turn-on-font-lock))


;; Set the default font and frame size for all new frames.
(setq default-frame-alist
      '((top . 200)
	(left . 50)
	(width . 96)
	(height . 60)
	(font .  "-*-Lucida Console-normal-r-*-*-11-82-*-*-c-*-*-ansi"))
      )

;; Set the default font and frame size for the initial
;; frame. Xdefaults overrides these values for the initial frame
(setq initial-frame-alist
      '((top . 100)
	(left . 400)
	(width . 96)
	(height . 60)
	(font .  "-*-Lucida Console-normal-r-*-*-11-82-*-*-c-*-*-ansi"))
      )

(defun my-window-setup-hook ()
  (set-default-font "-*-Lucida Console-normal-r-*-*-11-82-*-*-c-*-*-ansi-")
  (set-foreground-color "black")
  (set-background-color "ivory")
  (set-face-foreground 'modeline "yellow")
  (set-face-background 'modeline "blue")
  (set-mouse-color "orchid")
  (set-cursor-color "blue"))

(add-hook 'window-setup-hook 'my-window-setup-hook)

(add-hook 'c-mode-hook 'c-mode-setup)
(add-hook 'emacs-lisp-mode-hook 'emacs-lisp-mode-setup)

(message "Done loading .emacs")
