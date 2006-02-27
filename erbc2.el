;;; erbc2.el --- mostly: special functions for erbc.el
;; Time-stamp: <2006-02-27 16:14:49 deego>
;; Copyright (C) 2003 D. Goel
;; Emacs Lisp Archive entry
;; Filename: erbc2.el
;; Package: erbc2
;; Author: D. Goel <deego@gnufans.org>
;; Keywords:
;; Version:
;; URL:  http://www.emacswiki.org/cgi-bin/wiki.pl?ErBot




 
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


(defconst erbc2-version "0.0dev")
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

(defvar erbn-while-max 10000)
(defvar erbn-while-ctr 0)
(defmacro fs-while (cond &rest body)
  `(let
       ((erbn-while-ctr 0))
     (while
	 ,cond
       ;; this should enable the with-timeout checks..
       (sleep-for 0.01)
       (if (> erbn-while-ctr erbn-while-max)
	   (error "Max while iterations exceeded: %S"
		  erbn-while-ctr))
       (incf erbn-while-ctr)
       nil
       ,@body)))
       


(defmacro fs-dotimes (spec &rest body)
  `(dotimes 
       ,spec
     (sleep-for 0.01)
     nil
     ,@body))
       



(defun fsi-set-difference (a b)
  (set-difference a b))


(defun fsi-pp (&optional foo &rest bar)
  (pp foo))






(defvar erbn-tmp-avar nil)
(defvar erbn-tmp-newargs nil)

(defun erbn-apply-sandbox-args-old (args)
  (cond
   ((= (length args) 0) nil)
   ((= (length args) 1) 
    (if (equal (caar args) 'quote) args
      (mapcar 'erblisp-sandbox-quoted args)))
   (t
    (cons (erblisp-sandbox-quoted (car args))
	  (erbn-apply-sandbox-args (cdr args))))))
(defun erbn-apply-sandbox-args (args)
  (cond
   ((not (listp args))
    (erblisp-sandbox args))
   ((= (length args) 0) nil)
   (t
    (mapcar 'erblisp-sandbox args))))

(defvar erbn-apptmpa)
(defvar erbn-apptmpb)
(defvar erbn-apptmpc)
(defvar erbn-apptmpd)
(defvar erbn-tmpsymbolp)


(defmacro fs-apply (fcnsym &rest args)
  ""
  (when erbot-paranoid-p 
    (error "This function is disabled: erbot-paranoid-p"))
  (unless fcnsym (error "No function to fs-apply!"))
  (let (erbn-tmpargs
	(erbn-tmplen (length args))
	erbn-tmpfirstargs
	erbn-lastargs
	erbn-tmpspecialp ;; denotes: NIL: no arguments at all.
	erbn-tmpnoinitialp ;; denotes the case when the len args =1..
	)
    (cond
     ((= (length args) 0)
      (setq erbn-tmpspecialp t))
     ((= (length args) 1)
      (setq erbn-tmpnoinitialp t)))
    (cond
     ((null args)
      (setq erbn-tmpargs nil)
      (setq erbn-tmplastargs nil)
      (setq erbn-tmpspecialp nil))
     (t
      (setq erbn-tmpargs
	    (append (subseq args 0 (- erbn-tmplen 1))))
      (setq erbn-tmplastargs
	    (first (last args)))))
    (setq erbn-tmpargs (erbn-apply-sandbox-args erbn-tmpargs))
    (setq erbn-tmplastargs 
	  (if (and (listp erbn-tmplastargs)
	       (equal (car erbn-tmplastargs) 'quote))
	      erbn-tmplastargs
	    (erbn-apply-sandbox-args erbn-tmplastargs)))
    (cond
     ((listp fcnsym)
      (setq fcnsym (erblisp-sandbox-quoted fcnsym)))
     ((symbolp fcnsym)
      (setq fcnsym (erblisp-sandbox-quoted fcnsym)))
     (t (error "No clue how to apply that. ")))
    (cond
     (erbn-tmpspecialp
      `(apply (erblisp-sandbox-quoted ,fcnsym) nil))
     (erbn-tmpnoinitialp
      `(apply (erblisp-sandbox-quoted ,fcnsym) ,erbn-tmplastargs))
     (t
      `(apply (erblisp-sandbox-quoted ,fcnsym) ,@erbn-tmpargs ,erbn-tmplastargs)))))


;; (defmacro fs-apply-old (fcnsym &rest args)
;;   (error "This function is old.")
;;   (unless fcnsym (error "No function to fs-apply!"))
;;   (let (erbn-tmpargs
;; 	(erbn-tmplen (length args))
;; 	erbn-tmpnewargs
;; 	)
;;     (cond
;;      ((null args)
;;       (setq erbn-tmpargs nil))
;;      (t
;;       (setq erbn-tmpargs
;; 	    (append (subseq args 0 (- erbn-tmplen 1))
;; 		    (last args)))))
    
;;     (let* (
;; 	   (erbn-tmp-newargs (erbn-apply-sandbox-args erbn-tmpargs))
;; 	   (erbn-tmp-newlen (length erbn-tmp-newargs)))
;;     (cond
;;      ((listp fcnsym)
;;       (setq fcnsym (erblisp-sandbox-quoted fcnsym)))
;;      ((symbolp fcnsym)
;;       (setq fcnsym (erblisp-sandbox-quoted fcnsym)))
;;      (t (error "No clue how to apply that. ")))
;;     `(let ((erbn-tmp-avar ,fcnsym))
;;        (cond
;; 	((symbolp erbn-tmp-avar)
;; 	 (setq erbn-tmp-avar
;; 	       (erblisp-sandbox-quoted erbn-tmp-avar)))
;; 	(t "nada"))
;;        ,(if (= erbn-tmp-newlen 0)
;; 	    `(apply erbn-tmp-avar nil)
;; 	  `(apply erbn-tmp-avar ,@erbn-tmp-newargs nil))))))


(defmacro fs-funcall (symbol &rest args)
  `(fs-apply ,symbol ,@args nil))



;; hm, what is this?  Was it me?  silly me.. Why did I do this?? 
(defalias 'fs-function 'identity)

(defvar erbn-read-mode nil)
(defvar erbn-read-input nil)

(defvar fs-internal-botread-prompt "Enter: ")

(defun fsi-botread (&optional prompt)
  (unless prompt (setq prompt fs-internal-botread-prompt))
  (ignore-errors
    (erbot-reply (concat prompt "") proc nick tgt msg nil))
  (setq fs-internal-botread-prompt "Enter: ")
  (setq erbn-read-mode t)
  (while 
      (not erbn-read-input)
    (sleep-for 0.1)
    (sit-for 0.1))
  (let ((input erbn-read-input))
    (setq erbn-read-input nil)
    (setq erbn-read-mode nil)
    input))

(defun fsi-dun-mprinc (str)
  (ignore-errors
    (erbot-reply str proc nick tgt msg nil))
  (setq fs-internal-botread-prompt str))  
    
(defun fsi-botread-feed-internal (str)
  (setq erbn-read-input str)
  (format 
   "Thanks for feeding the read-line.  Msg obtained: %s"
   str)
  (setq erbn-read-mode nil)
  str)



;; i love this thing.. just no time to finish this yet..

;;; (defvar erbn-calsmart-tmp-expr nil)
;;; (defvar erbn-calsmart-tmp-exprb nil)
;;; (defvar erbn-calsmart-tmp-exprc nil)
;;; (defvar erbn-calsmart-tmp-error nil)

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
;;; 	      (erbn-calsmart-tmp-expr expr)
;;; 	      (erbn-calsmart-tmp-exprb 
;;; 	       (erbn-calsmart-break-expr erbn-calsmart-tmp-expr))
;;; 	      (erbn-calsmart-tmp-exprc 
;;; 	       (choose (list erbn-calsmart-expr 
;;; 			     erbn-calsmart-tmp-exprb)))
;;; 	      )
;;; 	 (cond
;;; 	  (erbn-calsmart-tmp-exprb
;;; 	   (condition-case erbn-calsmart-tmp-error
;;; 	       (eval erbn-calsmart-tmp-exprc)
;;; 	     (error (choose-fail))))
;;; 	  ;; couldn't break.. just do the normal thing. 
;;; 	  (t (eval erbn-calsmart-tmp-expr))))))))
    

;;; (defun erbn-calsmart-break-expr (expr)
;;;   "Expr is a list, which we intend to break.  WE prefer breaking such
;;; that the broken function gets 2 arguments.
;;; We want to rewrap everything by erbn-calsmart, so things get broken
;;; further..  
  
	   

(defun fsi-bash-specific-quote (&optional number &rest ignored)
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
    (setq bashstr (erbutils-buffer-string))
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

(defalias 'fsi-bsc 'fs-bash-specific-quote)
(defalias 'fs-bash-quote 'fs-bash-specific-quote)
(defalias 'fs-bash.org 'fs-bash-specific-quote)
;;(defalias 'fs-bash 'fs-bash-specific-quote)






(defalias 'fsi-lexical-let 'lexical-let)
(provide 'erbc2)
(run-hooks 'erbc2-after-load-hooks)



;;; erbc2.el ends here
