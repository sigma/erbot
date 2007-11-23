;;; erbedit.el --- quicker operator editing of bots' bbdb
;; Time-stamp: <2007-11-23 11:30:12 deego>
;; Copyright (C) 2003 D. Goel
;; Emacs Lisp Archive entry
;; Filename: erbedit.el
;; Package: erbedit
;; Author: D. Goel <deego@gnufans.org>
;; Keywords:  
;; Version:  
;; URL:  http://www.emacswiki.org/cgi-bin/wiki.pl?ErBot
;; For latest version: 

(defconst erbedit-home-page
  "http://gnufans.net/~deego")


 
;; This file is NOT (yet) part of GNU Emacs.
 
;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
 
;; This is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
 


(defconst erbedit-version "0.0dev")

;;==========================================
;;; Requires:
(eval-when-compile (require 'cl))

;;; Code:

(defgroup erbedit nil 
  "The group erbedit."
  :group 'applications)
(defcustom erbedit-before-load-hook nil 
  "Hook to run before loading erbedit."
  :group 'erbedit)
(defcustom erbedit-after-load-hook nil 
  "Hook to run after loading erbedit."
  :group 'erbedit)
(run-hooks 'erbedit-before-load-hook)

(defcustom erbedit-verbosity 0
  "How verbose to be.  
Once you are experienced with this lib, 0 is the recommended
value.  Values between -90 to +90 are \"sane\".  The
rest are for debugging."
  :type 'integer
  :group 'erbedit)
(defcustom erbedit-interactivity 0
  "How interactive to be.  
Once you are experienced with this lib, 0 is the recommended
value.  Values between -90 and +90 are \"sane\".  The rest are for
debugging."
  :type 'integer
  :group 'erbedit)
(defcustom erbedit-y-or-n-p-function 'erbedit-y-or-n-p
  "Function to use for interactivity-dependent  `y-or-n-p'.
Format same as that of `erbedit-y-or-n-p'."
  :type 'function
  :group 'erbedit)
(defcustom erbedit-n-or-y-p-function 'erbedit-y-or-n-p
  "Function to use for interactivity-dependent `n-or-y-p'.
Format same as that of `erbedit-n-or-y-p'."
  :type 'function
  :group 'erbedit)
(defun erbedit-message (points &rest args)
  "Signal message, depending on POINTS anderbedit-verbosity.
ARGS are passed to `message'."
  (unless (minusp (+ points erbedit-verbosity))
    (apply #'message args)))
(defun erbedit-y-or-n-p (add prompt)
  "Query or assume t, based on `erbedit-interactivity'.
ADD is added to `erbedit-interactivity' to decide whether
to query using PROMPT, or just return t."
  (if (minusp (+ add erbedit-interactivity))
        t
      (funcall 'y-or-n-p prompt)))
(defun erbedit-n-or-y-p (add prompt)
  "Query or assume t, based on `erbedit-interactivity'.
ADD is added to `erbedit-interactivity' to decide whether
to query using PROMPT, or just return t."
  (if (minusp (+ add erbedit-interactivity))
        nil
      (funcall 'y-or-n-p prompt)))

;;; Real Code:



(provide 'erbedit)
(run-hooks 'erbedit-after-load-hook)


(defun erbedit-replace-string (from to)
  "Like fs-replace-string, but acts across the entire bbdb"
  "Forget all terms containing occurrence of regexp REG. 

REMINDER: DO NOT FORGET TO exclude terms like fsbot hbot erbot deego
Deepak (author) <and of courser, terms like emacs> in prevent-reg
when using this command.
\\(bot\\|emacs\\|deego\\|goel\\|deepak\\|alex\\|bpt\\|oddmuse\\|iam\\)
.. for example..
"
  (let* 
      ((lenterms
	(fs-search-basic (regexp-quote from)
			 nil nil 'describe))
       (len (first lenterms))
       (terms (second lenterms)))
    (cond
     ((= len 0 ) (message "No terms. "))
     (t 
      (when (y-or-n-p (format "Act on these %S terms? " len))
	(erbedit-replace-string-slowly terms from to))))))

(defun erbedit-replace-string-slowly (terms from to)
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
      (message "Acting on term %S of %S: %S" ctr len thisterm)
      (sleep-for 0.1)
      (fs-replace-string from to thisterm "all")
      (message "Acting on term %S of %S: %S ... done" ctr len thisterm)
      (sleep-for 0.1)
      )))



;;; erbedit.el ends here
