;;; erbc3.el ---erbot lisp stuff which should be PERSISTENT ACROSS SESSIONS.
;; Time-stamp: <2005-08-11 20:30:32 deego>
;; Copyright (C) 2003 D. Goel
;; Emacs Lisp Archive entry
;; Filename: erbc3.el
;; Package: erbc3
;; Author: D. Goel <deego@gnufans.org>
;; Keywords:
;; Version:
;; URL:  http://www.emacswiki.org/cgi-bin/wiki.pl?ErBot
;; For latest version:

(defconst erbc3-home-page
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
 

(defconst erbc3-version "0.dev")
(defun erbc3-version (&optional arg)
   "Display erbc3's version string.
With prefix ARG, insert version string into current buffer at point."
  (interactive "P")
  (if arg
      (insert (message "erbc3 version %s" erbc3-version))
    (message "erbc3 version %s" erbc3-version)))

;;==========================================
;;; Requires:
(eval-when-compile (require 'cl))

;;; Code:

(defgroup erbc3 nil
  "The group erbc3."
  :group 'applications)
(defcustom erbc3-before-load-hook nil
  "Hook to run before loading erbc3."
  :group 'erbc3)
(defcustom erbc3-after-load-hook nil
  "Hook to run after loading erbc3."
  :group 'erbc3)
(run-hooks 'erbc3-before-load-hook)


;;; Real Code:
;; pf stands for persistent functions.
;; pv stands for persistent variables.

(defvar erbn-pf-file "~/public_html/data/userfunctions.el")
(defvar erbn-pv-file "~/public_html/data/uservariables.el")

(defun fsi-pfpv-load ()
  (fsi-pf-load)
  (fsi-pv-load))

(defun fsi-pf-load ()
  (if (file-exists-p erbn-pf-file)
      (fsi-ignore-errors-else-string (load erbn-pf-file))
    (message "File does not exist: %s" erbn-pf-file)))



(defun fsi-pv-load ()
  (when (file-exists-p erbn-pv-file)
    (ignore-errors (load erbn-pv-file))))

	 

(defun fsi-user-function-p (fcn)
  (member 
   fcn 
   (erbutils-functions-in-file erbn-pf-file)))


(defun erbn-create-defun-new (sexps body)
  (cons body sexps))

(defun erbn-create-defun-overwrite (sexps body fcn)
  (cons body
	(remove
	 (first (member-if
		 (lambda (arg) (equal (second arg) fcn))
		 sexps))
	 sexps)))

			 

(defun erbn-write-sexps-to-file (file sexps &optional backup-rarity)
  (unless backup-rarity (setq backup-rarity 1))
  (when (zerop (random backup-rarity)) (erbutils-mkback-maybe file))

  (find-file file)
  (widen)
  (delete-region (point-min) (point-max))
  (insert "\n\n\n")
  (insert
   (mapconcat
    (lambda (arg) (pp-to-string arg)) sexps "\n\n\n"))
  (insert "\n\n\n")
  (save-buffer))

(defvar erbn-tmp-sexps)
(defvar erbn-tmp-newbody)






	  


(defun fsi-pv-get-variables-values ()
  (let 
      ((vars 
	(apropos-internal "^fs-" 'boundp)))
    (mapcar
     (lambda (v)
       `(ignore-errors 
	  (defvar ,v
	    (quote ,(eval v)))))
     vars)))


(defcustom fs-pv-save-rarity 100000
  "if this is 1000, then file is saved one in a  thousand times... ")

;;;###autoload
(defun fsi-pv-save ()
  (interactive)
  (erbn-write-sexps-to-file 
   erbn-pv-file 
   (fs-pv-get-variables-values) 1000))
   ;; this should lead to a few saves every day... not too many, one hopes..
;;1000))


   
(defun erbn-readonly-check (sym)
  (if (get sym 'readonly)
      (error "The symbol %S can't be redefined! It is read-only!"
	     sym)))




(defmacro fsi-defun (fcn args &rest body)
  
  ;; the given fcn icould be a number or string, in which
   ;; case sandboxing won't touch it, so we need to override that case.
  (let ((docp nil))
    (unless 
	(and (listp body)
	     (> (length body) 0))
      (error "Function body should have a length of 1 or more"))
    (unless (and (symbolp fcn) (not (fsi-constant-object-p fcn)))
      (error "Defun symbols only! :P"))
    ;; doc string exists, and is followed by more stuff..
    (when (and (> (length body) 1)
	       (stringp (first body)))
      (setq docp t))
    (erbn-readonly-check fcn)
    
    (erbn-write-sexps-to-file
     erbn-pf-file
     (erbn-create-defun-overwrite
      (erbutils-file-sexps erbn-pf-file)
      (if docp
	  
	  (cons 'defun 
		(cons fcn 
		      (cons args 
			    (cons 
			     (first body)
			     (cons
			      `(erblisp-check-args ,@args)
			      (cons
			       '(sit-for 0)
			       (cdr body)))))))

	  (cons 'defun 
		(cons fcn 
		      (cons args 
			    (cons
			     `(erblisp-check-args ,@args)
			     (cons 
			      '(sit-for 0)
			      body))))))
      
      fcn))
    (fsi-pf-load)
    `(quote ,fcn)))



     

(defun fsi-defalias (sym1 sym2)
  (eval `(fsi-defun 
	  ,(erblisp-sandbox-quoted sym1) (&rest fs-bar)
	  (fs-apply (quote ,(erblisp-sandbox-quoted sym2)) fs-bar))))










(defun fsi-makunbound (&optional sym)
  (unless sym (error "Syntax: , (makunbound 'symbol)"))
  (setq sym
	(erblisp-sandbox sym))
  (makunbound sym))


(defun fsi-fmakunbound (&optional sym)
  (unless sym (error "Syntax: , (fmakunbound 'symbol)"))

  (setq sym
	(erblisp-sandbox sym))

  (erbn-readonly-check sym)

  (let 
      ;; this is to be returned..
      ((result (fmakunbound sym))
       (sexps       (erbutils-file-sexps erbn-pf-file)))
	
    ;; now we want to remove any definition of sym from the user
    ;; file: 
    
    (erbn-write-sexps-to-file
     erbn-pf-file
     (remove 
      (first 
       (member-if
	(lambda (arg) (equal (second arg) sym))
	sexps))
      sexps))
    (fsi-pf-load)
    result))


(defvar erbn-tmpsetq nil)

(defmacro fsi-setq (&rest args)
  `(let ((erbn-tmpsetq
	  (setq ,@args)))
     (fs-pv-save)
     erbn-tmpsetq))



(defun fsi-constant-object-p (object)
  "If the object is a symbol like nil or t, a symbol that cannot be
redefunned, return true. "
  (or (member object (list nil t))
      (keywordp object)))



(erbutils-defalias-i '(type-of))

(provide 'erbc3)
(run-hooks 'erbc3-after-load-hook)



;;; erbc3.el ends here
