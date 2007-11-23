;;; erbforget.el --- Help make the bots forget some TERMS. 
;; Time-stamp: <2007-11-23 11:30:10 deego>
;; Copyright (C) 2003 D. Goel
;; Emacs Lisp Archive entry
;; Filename: erbforget.el
;; Package: erbforget
;; Author: D. Goel <deego@gnufans.org>
;; Keywords:
;; Version:
;; URL:  http://www.emacswiki.org/cgi-bin/wiki.pl?ErBot


 
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

;;; Real Code:


(defun erbforget-sw (reg &optional prevent-reg matchingonly)
  "RUN THIS AS MYBOT WHEN SU-ED TO THE BOT. 

Forget all terms containing occurrence of regexp REG. 

REMINDER: DO NOT FORGET TO exclude terms like fsbot hbot erbot deego
Deepak (author) <and of course, terms like emacs> in prevent-reg
when using this command.
\\(bot\\|emacs\\|deego\\|goel\\|deepak\\|alex\\|bpt\\|oddmuse\\|iam\\)
.. for example..
Return len, which may (or may not) correspond to the number of items
removed. 
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
      (when (erbforget-y-or-n-p 40 (format "Forget %S terms? " len))
	(erbforget-slowly terms prevent-reg matchingonly reg))))
    len))


(defun erbforget-slowly (terms &optional prevent-reg matchingonly reg)
  "When matchingonly is t, we forget only the particular entry in the
NOTES that matches the regexp REG, if any..."
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
      (setq notes (fs-notes thisterm))
      (setq skipp 
	    (and prevent-reg
		 (string-match prevent-reg 
			       (mapconcat 'identity notes " "))))
      (cond
       
       (skipp
	(message "NOT FORGETTING term %S of %S: %S" ctr len thisterm)
	(sleep-for 1)
	)
       (matchingonly
	(let ((num -1) (donep nil))
	  (while (not donep)
	    (incf num 1)
	    (cond
	     ((>= num (length notes))
	      (setq donep t))
	     ((string-match reg (nth num notes))
	      (setq donep t)
	      (message "Forgetting term %S of %S: %S" ctr len thisterm)
	      (sleep-for 0.1)
	      (fs-forget thisterm num))
	     (t nil)))))

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
