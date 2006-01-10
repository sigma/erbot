;;; erbc5.el --- continuation of erbc.el
;; Time-stamp: <2006-01-10 10:28:14 deego>
;; Copyright (C) 2003 D. Goel
;; Emacs Lisp Archive entry
;; Filename: erbc5.el
;; Package: erbc5
;; Author: D. Goel <deego@gnufans.org>
;; Keywords:  
;; Version:  
;; URL:  http://www.emacswiki.org/cgi-bin/wiki.pl?ErBot
;; For latest version: 

(defconst erbc5-home-page
  "http://gnufans.net/~deego")


 
;; This file is NOT (yet) part of GNU Emacs.
 
;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
 
;; This is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
 

;; See also:


(defconst erbc5-version "0.0dev")
;;==========================================
;;; Requires:
(eval-when-compile (require 'cl))

;;; Code:

(defgroup erbc5 nil 
  "The group erbc5."
  :group 'applications)
(defcustom erbc5-before-load-hook nil 
  "Hook to run before loading erbc5."
  :group 'erbc5)
(defcustom erbc5-after-load-hook nil 
  "Hook to run after loading erbc5."
  :group 'erbc5)
(run-hooks 'erbc5-before-load-hook)

(defcustom erbc5-verbosity 0
  "How verbose to be.  
Once you are experienced with this lib, 0 is the recommended
value.  Values between -90 to +90 are \"sane\".  The
rest are for debugging."
  :type 'integer
  :group 'erbc5)
(defcustom erbc5-interactivity 0
  "How interactive to be.  
Once you are experienced with this lib, 0 is the recommended
value.  Values between -90 and +90 are \"sane\".  The rest are for
debugging."
  :type 'integer
  :group 'erbc5)
(defcustom erbc5-y-or-n-p-function 'erbc5-y-or-n-p
  "Function to use for interactivity-dependent  `y-or-n-p'.
Format same as that of `erbc5-y-or-n-p'."
  :type 'function
  :group 'erbc5)
(defcustom erbc5-n-or-y-p-function 'erbc5-y-or-n-p
  "Function to use for interactivity-dependent `n-or-y-p'.
Format same as that of `erbc5-n-or-y-p'."
  :type 'function
  :group 'erbc5)
(defun erbc5-message (points &rest args)
  "Signal message, depending on POINTS anderbc5-verbosity.
ARGS are passed to `message'."
  (unless (minusp (+ points erbc5-verbosity))
    (apply #'message args)))
(defun erbc5-y-or-n-p (add prompt)
  "Query or assume t, based on `erbc5-interactivity'.
ADD is added to `erbc5-interactivity' to decide whether
to query using PROMPT, or just return t."
  (if (minusp (+ add erbc5-interactivity))
        t
      (funcall 'y-or-n-p prompt)))
(defun erbc5-n-or-y-p (add prompt)
  "Query or assume t, based on `erbc5-interactivity'.
ADD is added to `erbc5-interactivity' to decide whether
to query using PROMPT, or just return t."
  (if (minusp (+ add erbc5-interactivity))
        nil
      (funcall 'y-or-n-p prompt)))

;;; Real Code:

(defalias 'fsi-listp-proper 'erbutils-listp-proper)
(erbutils-defalias-i '(upcase downcase capitalize upcase-initials))



(ignore-errors (require 'calc))

(defvar erbn-calc-time 3)
(defcustom erbn-calc-p nil
  "Enable this variable at your own risk.
Enabling this means that fsbot will do calc operations, but those have
no timeout build in... leading to DOS attacks. ")


(defun fsi-calc-eval (&optional str)
  "
Note that  even though this function has a with-timeout built into it,
that doesn't save us from a DOS attack..since emacs polls only when
waiting for user input.. 

which is why turned off by default.

"
  (unless erbn-calc-p 
    (error "Sorry, but i am a bot! not a calc!"))
  (unless str (error "Eval what?"))
  (unless (stringp str)
    (setq str (format "%s" str)))
  (with-timeout 
      (erbn-calc-time "That's WAY too much math for me!")
    (calc-eval str)))
    
(defalias 'fs-calc 'fs-calc-eval)

(erbutils-defalias '(process-list))
(defalias 'fs-list-processes 'fs-process-list)

(defcustom erbn-sregex-p nil
  "Nil by default for safety. Enable to permit fs-sregex.
I think it is safe, but not 100% sure, so disabled by default. --DG"
  )


(defun fsi-sreg (&rest args)
  (format "%S" 
	  (apply 'fs-sregex args)))


(defun fsi-sregex (&rest args)
  (cond
   (erbn-sregex-p
    (apply 'sregex args))
   (t
    (error "sregexp is disabled in this bot. "))))



(defmacro fsi-ignore-errors-else-string (&rest body)
  "Like ignore-errors, but tells and returns the erros.
\(Improved for me by Kalle on 7/3/01:)"
  (let ((err (gensym)))
    `(condition-case ,err (progn ,@body)
       (error
	(let 
	    ((str 	
	      (message "IGNORED ERROR: %s" (error-message-string ,err))))
	  (ding t)
	  (ding t)
	  (ding t)
  	  (sit-for 1)
	  str)))))


;; more math functions
(erbutils-defalias-i '(mod))
;; these from cl-extra
(erbutils-defalias-i '(isqrt floor* ceiling* round* mod* rem* signum
			     random*))


(erbutils-defalias-i '(symbol-name))



(provide 'erbc5)
(run-hooks 'erbc5-after-load-hook)



;;; erbc5.el ends here
