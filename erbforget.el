;;; erbforget.el --- Help make the bots forget some TERMS. 
;; Time-stamp: <2005-01-01 20:53:24 deego>
;; Copyright (C) 2003 D. Goel
;; Emacs Lisp Archive entry
;; Filename: erbforget.el
;; Package: erbforget
;; Author: D. Goel <deego@gnufans.org>
;; Keywords:
;; Version:
;; URL:  http://www.emacswiki.org/cgi-bin/wiki.pl?ErBot


(defconst erbforget-home-page
  "http://www.emacswiki.org/cgi-bin/wiki.pl?ErBot")


 
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


;; Quick start:
(defconst erbforget-quick-start
  "Help..."
)

(defun erbforget-quick-start ()
  "Provides electric help from variable `erbforget-quick-start'."
  (interactive)
  (with-electric-help
   '(lambda () (insert erbforget-quick-start) nil) "*doc*"))

(defconst erbforget-version "0.0-DUMMY")
(defun erbforget-version (&optional arg)
   "Display erbforget's version string.
With prefix ARG, insert version string into current buffer at point."
  (interactive "P")
  (if arg
      (insert (message "erbforget version %s" erbforget-version))
    (message "erbforget version %s" erbforget-version)))

;;==========================================
;;; Requires:
(eval-when-compile (require 'cl))

;;; Code:

(defgroup erbforget nil
  "The group erbforget."
  :group 'applications)
(defcustom erbforget-before-load-hooks nil
  "Hooks to run before loading erbforget."
  :group 'erbforget)
(defcustom erbforget-after-load-hooks nil
  "Hooks to run after loading erbforget."
  :group 'erbforget)
(run-hooks 'erbforget-before-load-hooks)

(defcustom erbforget-verbosity 0
  "How verbose to be.
Once you are experienced with this lib, 0 is the recommended
value.  Values between -90 to +90 are \"sane\".  The
rest are for debugging."
  :type 'integer
  :group 'erbforget)
(defcustom erbforget-interactivity 0
  "How interactive to be.
Once you are experienced with this lib, 0 is the recommended
value.  Values between -90 and +90 are \"sane\".  The rest are for
debugging."
  :type 'integer
  :group 'erbforget)
(defcustom erbforget-y-or-n-p-function 'erbforget-y-or-n-p
  "Function to use for interactivity-dependent  `y-or-n-p'.
Format same as that of `erbforget-y-or-n-p'."
  :type 'function
  :group 'erbforget)
(defcustom erbforget-n-or-y-p-function 'erbforget-y-or-n-p
  "Function to use for interactivity-dependent `n-or-y-p'.
Format same as that of `erbforget-n-or-y-p'."
  :type 'function
  :group 'erbforget)
(defun erbforget-message (points &rest args)
  "Signal message, depending on POINTS anderbforget-verbosity.
ARGS are passed to `message'."
  (unless (minusp (+ points erbforget-verbosity))
    (apply #'message args)))
(defun erbforget-y-or-n-p (add prompt)
  "Query or assume t, based on `erbforget-interactivity'.
ADD is added to `erbforget-interactivity' to decide whether
to query using PROMPT, or just return t."
  (if (minusp (+ add erbforget-interactivity))
        t
      (funcall 'y-or-n-p prompt)))
(defun erbforget-n-or-y-p (add prompt)
  "Query or assume t, based on `erbforget-interactivity'.
ADD is added to `erbforget-interactivity' to decide whether
to query using PROMPT, or just return t."
  (if (minusp (+ add erbforget-interactivity))
        nil
      (funcall 'y-or-n-p prompt)))

;;; Real Code:


(defun erbforget-sw (reg &optional prevent-reg)
  "RUN THIS WHEN AS MYBOT WHEN SU-ED TO THE BOT. 

Forget all terms containing occurrence of regexp REG. 

REMINDER: DO NOT FORGET TO exclude terms like fsbot hbot erbot deego
Deepak (author) <and of course, terms like emacs> in prevent-reg
when using this command.
\\(bot\\|emacs\\|deego\\|goel\\|deepak\\|alex\\|bpt\\|oddmuse\\|iam\\)
.. for example..
"
  (interactive "sRegex to forget: ")
  (let* 
      ((lenterms
	(fs-search-basic reg nil nil 'describe))
       (len (first lenterms))
       (terms (second lenterms)))
    (cond
     ((= len 0 ) (message "No such terms. "))
     (t 
      (when (y-or-n-p (format "Forget %S terms? " len))
	(erbforget-slowly terms prevent-reg))))))


(defun erbforget-slowly (terms &optional prevent-reg)
  (let 
      ((len (length terms))
       (ctr 0)
       thisterm 
       skipp
       notes
       )
    (while terms
      (setq thisterm (car terms) terms (cdr terms))
      (setq ctr (+ ctr 1))
      (setq skipp 
	    (and prevent-reg
		 (progn
		   (setq notes (mapconcat 'identity 
					  (fs-notes thisterm)
					  " "))
		   (string-match prevent-reg notes))))
      (cond
       
       (skipp
	(message "NOT FORGETTING term %S of %S: %S" ctr len thisterm)
	(sleep-for 1)
	)
       (t
	(message "Forgetting term %S of %S: %S" ctr len thisterm)
	(sleep-for 0.1)
	(fs-forget thisterm "all")
	(message "Forgetting term %S of %S: %S.. done" ctr len thisterm)
	(sleep-for 0.1)
	)


	))))
	





(provide 'erbforget)
(run-hooks 'erbforget-after-load-hooks)



;;; erbforget.el ends here
