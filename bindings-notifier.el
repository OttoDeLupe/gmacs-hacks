;; A function for reminding you about functions that are available 
;; as keyboard bindings.
; $Id: bindings-notifier.el,v 1.9 1992/08/31 14:27:12 gary Exp $

;; Written February 1989 by Gary Sabot (gary@THINK.COM).


;; This file implements a keyboard binding
;; notifier/teacher.  It lets you know
;; when a function that you are calling by name
;; is available by a series of keystrokes.   

;; You can get a complete summary of the functions that the notifier
;; has notified you about so far by calling DISPLAY-BINDING-STATISTICS

;; Known bugs: CAL says it might break keyboard macros that use M-x
;; It displays menu bindings even if you used them! 

(require 'cl)

;; USER SETTABLE VARIABLES

(defvar check-for-extended-command-bindings t
  "* If t, every time you execute an extended command that is
be bound to a keystroke, you will be notified about it the keystroke.
If nil, you never get notified (the bindings-notifier is turned off).")


(defvar display-bindings-in-mode-line t
  "* If t, display binding information in place of the mode line
for a short period of time, or until there is keyboard input. 
If nil, the binding infomration is displayed in a pop up window.")

(defvar display-bindings-sit-time 10
  "* maximum (if user doesn't interupt by typing) time in seconds to 
display the binding information in the mode line.")



;; RECORD KEEPING
(defvar saved-names nil
  "* Saves names of functions that have been called by name 
even though they have key bindings.  Used by display-binding-statistics.")



;; Bind entry point to meta-x 
(global-set-key "\M-x" 'execute-extended-command-and-check-for-bindings)



;; MAIN ENTRY POINT
(defun execute-extended-command-and-check-for-bindings (&optional arg)
  "Replacement for execute-extended-command.  This function notifies you
if the function that you type in has a keyboard binding.  It
prints out those bindings and updates statistics in the variable
SAVED-NAMES.  Output goes in a typeout window if 
DISPLAY-BINDINGS-IN-MODE-LINE is nil, otherwise it replaces
the mode line for DISPLAY-BINDINGS-SIT-TIME seconds, or until 
you type something"
  (interactive "P")
  (execute-extended-command arg)
  (when check-for-extended-command-bindings
    (let ((name (first-atomic-command command-history)))
      (let ((output (where-is-internal name (current-local-map))))
	(when output
	  (update-binding-statistics name)
	  (display-key-binding-output 
	    (describe-location name output)))))))

(defun first-atomic-command (list)
  "Some commands, like dired, insert a lambda expression
onto the command history after their name.  This skips over
the lambda to find the real command name."
  (if list
      (let ((result (caar list)))
	(if (listp result)
	    (first-atomic-command (cdr list))
	    result))
      nil))

(defun describe-location (function-name list-of-bindings)
  "Given a function and a list of bindings, returns a string
(just like the where-is function) describing the function and
its bindings."
  (format "%s is on %s"
	  function-name
	  (mapconcat 'key-description list-of-bindings ", ")))

(defun update-binding-statistics (name)
  "Update the statistics for functions called by name instead of binding."
  (let ((n (assoc name saved-names)))
    (if n
	(setcdr n (+ 1 (cdr n)))
	(setq saved-names (cons (cons name 1) saved-names)))))

(defun display-binding-statistics ()
  "Display a sorted description of all the functions that have been
       called by name instead of by keystroke."
  (interactive)
  (setq saved-names (sort saved-names '(lambda (x y) (> (cdr x) (cdr y)))))
  (display-in-typeout-window 
    (format "%s"
	    (mapconcat 'describe-name-usage saved-names "
"))))

(defun describe-name-usage (name-count)
  "Takes a cons of a name and a count, and returns a string
describing them that also includes the keyboard binding of the name."
  (format "%s, used %s %s (on %s)" 
	  (car name-count) 
	  (cdr name-count) 
	  (if (> (cdr name-count) 1)
	      "times"
	      "time")
	  (mapconcat 'key-description 
		     (where-is-internal (car name-count) (current-local-map))
		     ", ")))


(defun display-key-binding-output (text)
  "display output of binding notifier"
  (if display-bindings-in-mode-line
      (display-in-mode-line text)
      (display-in-typeout-window text)))

(defun display-in-mode-line (text)
  "display output of binding notifier in the mode line"
  (let ((mode-line-format mode-line-format))
    (setq mode-line-format
	    (list (concat "    " text)))
    (force-time-redisplay)
    (sit-for display-bindings-sit-time))
  (force-time-redisplay))

;(defun display-in-display-time-string (text)
;  "display output of binding notifier in the mode line"
;  (let ((display-time-string display-time-string))
;    (setq display-time-string (concat " *** " text " *** "))
;    (force-time-redisplay)
;    (sit-for display-bindings-sit-time))
;  (force-time-redisplay))

(defun force-time-redisplay ()
  (save-excursion (set-buffer (other-buffer)))
  (set-buffer-modified-p (buffer-modified-p))
  (sit-for 0))
    
;; this clobbers the output at the bottom
;(defun display-as-message (text)
;  (message text))

;; this one opens up a typeout window
(defun display-in-typeout-window (text)
; view mode has changed in v19, get rid of save-excursion and bury for now
; window replaces old rather than being a typeout window
;  (save-window-excursion
;    (split-window nil nil)
    (let ((old-buffer (current-buffer))
	  (typeout-window-buffer (get-buffer-create "*key-binding-output-window*")))
      (switch-to-buffer typeout-window-buffer)
      (let ((buffer-read-only nil))
	(delete-region (point-min) (point-max))
	(insert text)
	(shrink-window-if-larger-than-buffer (selected-window)))
      (setq buffer-read-only t)
      (view-mode old-buffer 'kill-buffer)
      ))





