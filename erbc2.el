;;; erbc2.el --- mostly: special functions for erbc.el
;; Time-stamp: <2003-08-04 17:51:52 deego>
;; Copyright (C) 2003 D. Goel
;; Emacs Lisp Archive entry
;; Filename: erbc2.el
;; Package: erbc2
;; Author: D. Goel <deego@gnufans.org>
;; Keywords:
;; Version:
;; URL:  http://gnufans.net/~deego



 
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
 


;; this gile contains yet more functions for fs-.  The functions
;; here shall tend to be "specially defined" ones.


(defconst erbc2-version "NA")
;;==========================================
;;; Requires:
(eval-when-compile (require 'cl))

;;; Code:

(defcustom erbc2-before-load-hooks nil
  "Hooks to run before loading erbc2."
  :group 'erbc2)
(defcustom erbc2-after-load-hooks nil
  "Hooks to run after loading erbc2."
  :group 'erbc2)
(run-hooks 'erbc2-before-load-hooks)


;;; Real Code:

(defvar erbnoc-while-max 10000)
(defvar erbnoc-while-ctr 0)
(defmacro fs-while (cond &rest body)
  `(let
       ((erbnoc-while-ctr 0))
     (while
	 ,cond
       ;; this should enable the with-timeout checks..
       (sleep-for 0.01)
       (if (> erbnoc-while-ctr erbnoc-while-max)
	   (error "Max while iterations exceeded: %S"
		  erbnoc-while-ctr))
       (incf erbnoc-while-ctr)
       nil
       ,@body)))
       


(defmacro fs-dotimes (spec &rest body)
  `(dotimes 
       ,spec
     (sleep-for 0.01)
     nil
     ,@body))
       



(defun fs-set-difference (a b)
  (set-difference a b))


(defun fs-pp (&optional foo &rest bar)
  (pp foo))


(defun fs-mapcar (sym seq)
  "only symbols allowed at this time. "
  (unless (symbolp sym)
    (error "Function argument to mapcar for this bot can only be a symbol."))
  (setq sym (erblisp-sandbox-quoted sym))
  ;; everything should already be boxquoted.. cool
  (mapcar sym seq))

(defun fs-mapc (sym seq)
  "only symbols allowed at this time. "
  (unless (symbolp sym)
    (error "Function argument to mapcar for this bot can only be a symbol."))
  (setq sym (erblisp-sandbox-quoted sym))
  ;; everything should already be boxquoted.. cool
  (mapc sym seq))


(defvar erbnoc-tmp-avar nil)
(defvar erbnoc-tmp-newargs nil)

(defun erbnoc-apply-sandbox-args-old (args)
  (cond
   ((= (length args) 0) nil)
   ((= (length args) 1) 
    (if (equal (caar args) 'quote) args
      (mapcar 'erblisp-sandbox-quoted args)))
   (t
    (cons (erblisp-sandbox-quoted (car args))
	  (erbnoc-apply-sandbox-args (cdr args))))))
(defun erbnoc-apply-sandbox-args (args)
  (cond
   ((not (listp args))
    (erblisp-sandbox args))
   ((= (length args) 0) nil)
   (t
    (mapcar 'erblisp-sandbox args))))

(defvar erbnoc-apptmpa)
(defvar erbnoc-apptmpb)
(defvar erbnoc-apptmpc)
(defvar erbnoc-apptmpd)
(defvar erbnoc-tmpsymbolp)


(defmacro fs-apply (fcnsym &rest args)
  ""
  (unless fcnsym (error "No function to fs-apply!"))
  (let (erbnoc-tmpargs
	(erbnoc-tmplen (length args))
	erbnoc-tmpfirstargs
	erbnoc-lastargs
	erbnoc-tmpspecialp ;; denotes: NIL: no arguments at all.
	erbnoc-tmpnoinitialp ;; denotes the case when the len args =1..
	)
    (cond
     ((= (length args) 0)
      (setq erbnoc-tmpspecialp t))
     ((= (length args) 1)
      (setq erbnoc-tmpnoinitialp t)))
    (cond
     ((null args)
      (setq erbnoc-tmpargs nil)
      (setq erbnoc-tmplastargs nil)
      (setq erbnoc-tmpspecialp nil))
     (t
      (setq erbnoc-tmpargs
	    (append (subseq args 0 (- erbnoc-tmplen 1))))
      (setq erbnoc-tmplastargs
	    (first (last args)))))
    (setq erbnoc-tmpargs (erbnoc-apply-sandbox-args erbnoc-tmpargs))
    (setq erbnoc-tmplastargs 
	  (if (and (listp erbnoc-tmplastargs)
	       (equal (car erbnoc-tmplastargs) 'quote))
	      erbnoc-tmplastargs
	    (erbnoc-apply-sandbox-args erbnoc-tmplastargs)))
    (cond
     ((listp fcnsym)
      (setq fcnsym (erblisp-sandbox-quoted fcnsym)))
     ((symbolp fcnsym)
      (setq fcnsym (erblisp-sandbox-quoted fcnsym)))
     (t (error "No clue how to apply that. ")))
    (cond
     (erbnoc-tmpspecialp
      `(apply ,fcnsym nil))
     (erbnoc-tmpnoinitialp
      `(apply ,fcnsym ,erbnoc-tmplastargs))
     (t
      `(apply ,fcnsym ,@erbnoc-tmpargs ,erbnoc-tmplastargs)))))


(defmacro fs-apply-old (fcnsym &rest args)
  (unless fcnsym (error "No function to fs-apply!"))
  (let (erbnoc-tmpargs
	(erbnoc-tmplen (length args))
	erbnoc-tmpnewargs
	)
    (cond
     ((null args)
      (setq erbnoc-tmpargs nil))
     (t
      (setq erbnoc-tmpargs
	    (append (subseq args 0 (- erbnoc-tmplen 1))
		    (last args)))))
    
    (let* (
	   (erbnoc-tmp-newargs (erbnoc-apply-sandbox-args erbnoc-tmpargs))
	   (erbnoc-tmp-newlen (length erbnoc-tmp-newargs)))
    (cond
     ((listp fcnsym)
      (setq fcnsym (erblisp-sandbox-quoted fcnsym)))
     ((symbolp fcnsym)
      (setq fcnsym (erblisp-sandbox-quoted fcnsym)))
     (t (error "No clue how to apply that. ")))
    `(let ((erbnoc-tmp-avar ,fcnsym))
       (cond
	((symbolp erbnoc-tmp-avar)
	 (setq erbnoc-tmp-avar
	       (erblisp-sandbox-quoted erbnoc-tmp-avar)))
	(t "nada"))
       ,(if (= erbnoc-tmp-newlen 0)
	    `(apply erbnoc-tmp-avar nil)
	  `(apply erbnoc-tmp-avar ,@erbnoc-tmp-newargs nil))))))

(defmacro fs-funcall (symbol &rest args)
  `(fs-apply ,symbol ,@args nil))


(defalias 'fs-function 'identity)

(defvar erbnoc-read-mode nil)
(defvar erbnoc-read-input nil)

(defvar fs-internal-botread-prompt "Enter: ")

(defun fs-botread (&optional prompt)
  (unless prompt (setq prompt fs-internal-botread-prompt))
  (ignore-errors
    (erbot-reply (concat prompt "") proc nick tgt msg nil))
  (setq fs-internal-botread-prompt "Enter: ")
  (setq erbnoc-read-mode t)
  (while 
      (not erbnoc-read-input)
    (sleep-for 0.1)
    (sit-for 0.1))
  (let ((input erbnoc-read-input))
    (setq erbnoc-read-input nil)
    (setq erbnoc-read-mode nil)
    input))

(defun fs-dun-mprinc (str)
  (ignore-errors
    (erbot-reply str proc nick tgt msg nil))
  (setq fs-internal-botread-prompt str))  
    
(defun fs-botread-feed-internal (str)
  (setq erbnoc-read-input str)
  (format 
   "Thanks for feeding the read-line.  Msg obtained: %s"
   str)
  (setq erbnoc-read-mode nil)
  str)



;; i love this thing.. just no time to finish this yet..

;;; (defvar erbnoc-calsmart-tmp-expr nil)
;;; (defvar erbnoc-calsmart-tmp-exprb nil)
;;; (defvar erbnoc-calsmart-tmp-exprc nil)
;;; (defvar erbnoc-calsmart-tmp-error nil)

;;; (defmacro fs-calsmart (&rest exprs)
;; "This will insert parenthesis appropriately, so you can type stuff
;; like , c + 2 3 4 - 3 4 * 3 4 5 (- 2 3) 
;; and fsbot will try parenthesis at appropriate places until the
;; resulting expression makes sense .. "
;;;   (require 'choose)
;;;   (case (length exprs)
;;;     ((1) `(car ,exprs))
;;;     (t
;;;      `(choose-with 
;;;        (let* (
;;; 	      (erbnoc-calsmart-tmp-expr expr)
;;; 	      (erbnoc-calsmart-tmp-exprb 
;;; 	       (erbnoc-calsmart-break-expr erbnoc-calsmart-tmp-expr))
;;; 	      (erbnoc-calsmart-tmp-exprc 
;;; 	       (choose (list erbnoc-calsmart-expr 
;;; 			     erbnoc-calsmart-tmp-exprb)))
;;; 	      )
;;; 	 (cond
;;; 	  (erbnoc-calsmart-tmp-exprb
;;; 	   (condition-case erbnoc-calsmart-tmp-error
;;; 	       (eval erbnoc-calsmart-tmp-exprc)
;;; 	     (error (choose-fail))))
;;; 	  ;; couldn't break.. just do the normal thing. 
;;; 	  (t (eval erbnoc-calsmart-tmp-expr))))))))
    

;;; (defun erbnoc-calsmart-break-expr (expr)
;;;   "Expr is a list, which we intend to break.  WE prefer breaking such
;;; that the broken function gets 2 arguments.
;;; We want to rewrap everything by erbnoc-calsmart, so things get broken
;;; further..  
  
	   

(defun fs-bash-specific-quote (&optional number &rest ignored)
  "NUMBER need not be jsut NUMBER.  Any argument to
bash-specific-quotes, like random, should work."
  (require 'bash-quotes)
  (let (aa bb  bashstr)
    (unless number 
      (setq number "random"))
    (bash-specific-quote (format "%s" number))
    (sit-for 5)
    ;;     (let (aa bb)
    ;;       (set-buffer "*bash*")
    ;;       (goto-char (point-min))
    ;;       (setq aa (search-forward "--------" nil t))
    ;;       (forward-line 1)
    ;;       (setq aa (search-forward "--------" nil t))
    ;;       (forward-line 1)
    ;;       (setq aa (point))
    ;;       (setq bb (search-forward "--------" nil t))
    ;;       (forward-line -1)
    ;;       (setq bb (point))
    ;;       (when (and aa bb)
    ;; 	(buffer-substring-no-properties aa bb)))
    (set-buffer "*bash*")
    (setq bashstr (buffer-string))
    (with-temp-buffer 
      (insert bashstr)
      (goto-char (point-min))
      (setq aa (search-forward-regexp "^--------" nil t))
      (forward-line 1)
      (setq aa (search-forward-regexp "^--------" nil t))
      (forward-line 1)
      (beginning-of-line)
      (setq aa (point))
      (setq bb (search-forward-regexp "^--------" nil t))
      (forward-line -1)
      (end-of-line)
      (setq bb (point))
      (if (and aa bb)
	  (buffer-substring-no-properties aa bb)
	"No result"))))

(defalias 'fs-bsc 'fs-bash-specific-quote)
(defalias 'fs-bash-quote 'fs-bash-specific-quote)
(defalias 'fs-bash.org 'fs-bash-specific-quote)
;;(defalias 'fs-bash 'fs-bash-specific-quote)



(defalias 'fs-lexical-let 'lexical-let)
(provide 'erbc2)
(run-hooks 'erbc2-after-load-hooks)



;;; erbc2.el ends here
