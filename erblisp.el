;;; erblisp.el --- 
;; Time-stamp: <2003-06-20 17:56:04 deego>
;; Copyright (C) 2002 D. Goel
;; Emacs Lisp Archive entry
;; Filename: erblisp.el
;; Package: erblisp
;; Author: D. Goel <deego@gnufans.org>
;; Version: 99.99
;; URL:  http://www.emacswiki.org/cgi-bin/wiki.pl?ErBot
 

(defvar erblisp-home-page
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
(defvar erblisp-quick-start
  "Help..."
)

(defun erblisp-quick-start ()
  "Provides electric help regarding variable `erblisp-quick-start'."
  (interactive)
  (with-electric-help
   '(lambda () (insert erblisp-quick-start) nil) "*doc*"))



(defvar erblisp-version "99.99")

;;==========================================
;;; Code:

(defgroup erblisp nil 
  "The group erblisp"
   :group 'applications)
(defcustom erblisp-before-load-hooks nil "" :group 'erblisp)
(defcustom erblisp-after-load-hooks nil "" :group 'erblisp)
(run-hooks 'erblisp-before-load-hooks)


(defun erblisp-process-msg (msg &optional proc nick tgt)
  "MSG is either a string or a tree.. If it is a tree, it looks
something like 
   '(foo bar (bar foo))

This command sandboxes the message and then processes it.."

  (if (stringp msg)
      (setq msg (read msg)))
  (format "%s" (eval (erblisp-sandbox-fuzzy msg))))

(defun erblisp-sandbox-quoted-maybe (expr)
  "sandboxes the whole expression even if it starts with a quote."
  (cond
   ((and (listp expr)
	 (equal (first expr) 'quote))
    (cons 'quote
	  (mapcar 'erblisp-sandbox (cdr expr))))
   (t (erblisp-sandbox expr))))


(defun erblisp-sandbox-quoted (expr)
  "Assumes that the expression will result in a quoted thingy and
tries to make sure that we sandbox that whole quoted thing.. "
  (cond
   ((and (listp expr)
	 (equal (first expr) 'quote))
    (cons 'quote
	  (mapcar 'erblisp-sandbox (cdr expr))))
   ((listp expr)
    (list 'fs-sandbox-quoted (erblisp-sandbox expr)))
   ;; just an atom 
   (t (erblisp-sandbox expr))))


(defvar erblisp-allowed-words
  '(nil t 
	;; Also consider:
	;; &rest
	;; &optional
	
	)
  "You should add &rest and &optional to this list. 
We WON'T do this by default since this could lead to exploits if you
*happen* to have bound these keywords to weird stuff like 
(setq &rest (shell-command \"rm -rf /\")) in your .emacs."
)

(defun erblisp-sandbox (expr)
  ""
  (cond 
   ;; first condition
   ((null expr) nil)
   ;; second condition
   ((listp expr) 
    (let ((fir (first expr)))
      (cond
       ((listp fir)
	(cons (erblisp-sandbox fir)
	      (mapcar 'erblisp-sandbox (cdr expr))))
       ((equal (format "%S" fir) "quote")
	;; if quoted, it is fine...
	expr)
       (t (cons 
	   (if (or (equal 0 (string-match "fs-" (format "%S" fir)))
		   (member fir erblisp-allowed-words))
	       fir
	     (intern (concat "fs-" (format "%S" fir))))
	   (mapcar 'erblisp-sandbox (cdr expr)))))))
   

   ;; final condition.. --> when the expr is an atom..  It should be a
   ;; a constant..  or an allowed atom.. allowed == prefixed with fs-
   (t (cond
       ((and (symbolp expr) 
	     (equal 0 (string-match "fs-" (format "%s" expr))))
	expr)
       ((equal expr t) expr)
       ((member expr erblisp-allowed-words) expr)
       ((symbolp expr)
	;;(boundp (intern (concat "fs-" (format "%S" expr)))))
	(intern (concat "fs-" (format "%s" expr))))
       ;; other symbol
       ;;((symbolp expr) (list 'quote expr))
       ;; a number or string now..
       ;; this actually happens when they feed byte-compiled code to
       ;; the bot, like:
       ;;, (funcall #[nil "\300\207" [1] 1])    
       ((not (or (symbolp expr) (numberp expr) (stringp expr)))
	(error "%s %s" "Should not reach here.  Quantum Tunnelling! "
	       "What are you trying to feed me? Byte-compiled code??"
	       ))
       (t expr)))
   ))
	 

(defun erblisp-sandbox-fuzzy (expr)
  "Sandboxes a message.. Ensures that the functions are all fs-
and the arguments are NOT variable-names... This one sandboxes
preferably by quoting unless fs-symbol is bound.."
  (cond 

   ;; first condition
   ((null expr) nil)
   
   ;; second condition
   ((listp expr) 
    (let ((fir (first expr)))
      (cond
       ((listp fir)
	(cons (erblisp-sandbox-fuzzy fir))
	(mapcar 'erblisp-sandbox-fuzzy (cdr expr)))
       ((equal (format "%S" fir) "quote")
	;; if quoted, it is fine...
	expr)
       (t (cons 
	   (if (equal 0 (string-match "fs-" (format "%S" fir)))
	       fir
	     (intern (concat "fs-" (format "%S" fir))))
	   (mapcar 'erblisp-sandbox-fuzzy (cdr expr)))))))
   

   ;; final condition.. --> when the expr is an atom..  It should be a
   ;; a constant..  or an allowed atom.. allowed == prefixed with fs-
   (t (cond
       ((and (symbolp expr) 
	     (equal 0 (string-match "fs-" (format "%s" expr))))
	expr)
       ((and (symbolp expr)
	     (or
	      (boundp (intern (concat "fs-" (format "%S" expr))))
	      (fboundp (intern (concat "fs-" (format "%S" expr))))
	     ))
	(intern (concat "fs-" (format "%s" expr))))
       ;; other symbol
       ((symbolp expr) (list 'quote expr))
       ;; a number or string now..

       ((not (or (symbolp expr) (numberp expr) (stringp expr)))
	(error "Should not reach here.  Fuzzy tunnels!"))
       (t expr)))
   ))




(defun erblisp-sandbox-full(expr &optional midstream)
  "
This will ensure that anything rigt after parens is sandboxed by a
fs- prefix.  And anything else is either a symbol , or a string,
but not a variable...  viz: quoted ...else converted into one. 

midstream is in internal variable..."
  (cond
   ((null expr) nil)
   ((listp expr)
    (let* ((fir (first expr)))
      (if (eql fir 'quote)
	  expr
	(cons (erblisp-sandbox-full fir)
	      (mapcar '(lambda (arg)
			 (erblisp-sandbox-full arg t))
		      (cdr expr))))))  
   ;; now we know that expr is a non-nil atom...
   (midstream
    (if (stringp expr) expr
      (list 'quote expr)))



   ;; midstream is untrue... expr is thus an atom at the beginning..
   (t
    (if (equal 0 (string-match "fs-" (format "%s" expr)))
	expr (intern (concat "fs-" (format "%s" expr)))))))

(provide 'erblisp)
(run-hooks 'erblisp-after-load-hooks)



;;; erblisp.el ends here
