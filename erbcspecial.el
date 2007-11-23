;;; erbcspecial.el --- Special/dangerous implementation functions.
;; Many fs-functions can simply be defined in terms of other
;; fs-functions (and always should be!, for security.)
;; This file is for the remaining few, that can't be.
;; Thus, CODE IN THIS FILE SHOULD BE CONSTRUCTED VERY CAREFULLY.
1
;; Time-stamp: <2007-11-23 11:30:12 deego>
;; Copyright (C) 2004 D. Goel
;; Emacs Lisp Archive entry
;; Filename: erbcspecial.el
;; Package: erbcspecial
;; Author: D. Goel <deego@glue.umd.edu>
;; Keywords:
;; Version:
;; URL:  http://www.emacswiki.org/cgi-bin/wiki.pl?ErBot
;; For latest version:


 
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
 

;; See also:

(defconst erbcspecial-version "0.0dev")

;;==========================================
;;; Requires:
(eval-when-compile (require 'cl))


;;; Code:

(defun erbn-special-quote-function (fcn)
  (cond
   ((symbolp fcn)
    (erblisp-sandbox-quoted fcn))
   ((and (listp fcn)
	 (equal (first fcn) 'lambda)
	 fcn))
   ;; notice the recursion below:
   ((listp fcn) (erbn-special-quote-function (fs-eval fcn)))
   (t (error "Cannot apply this as a function!"))))


;; (defun fs-mapcar-old (sym seq)
;;   "only symbols allowed at this time. "
;;   (unless (symbolp sym)
;;     (error "Function argument to mapcar for this bot can only be a symbol."))
;;   (setq sym (erblisp-sandbox-quoted sym))
;;   ;; everything should already be boxquoted.. cool
;;   (mapcar sym seq))

(defun fsi-mapcar (fcn ls)
  (apply 'mapcar 
	 (erbn-special-quote-function fcn)
	 ls nil))




;; (defun fs-mapc (sym seq)
;;   "only symbols allowed at this time. "
;;   (unless (symbolp sym)
;;     (error "Function argument to mapcar for this bot can only be a symbol."))
;;   (setq sym (erblisp-sandbox-quoted-ensure-symbol sym))
;;   ;; everything should already be boxquoted.. cool
;;   (mapc sym seq))




(defun fsi-mapc (fcn ls)
  (apply 'mapc
	 (erbn-special-quote-function fcn)
	 ls nil))



(defun fsi-mapconcat (fcn ls sep)
  (apply 'mapconcat
	 (erbn-special-quote-function fcn)
	 ls sep nil))







(defun fsi-maplist (fcn ls &rest args)
  (require 'cl)
  (apply 'maplist
	 (erbn-special-quote-function fcn)
	 ls args))



(defun fsi-mapl (fcn ls &rest args)
  (require 'cl)
  (apply 'mapl
	 (erbn-special-quote-function fcn)
	 ls args))

(defun fsi-mapcar* (fcn ls &rest args)
  (require 'cl)
  (apply 'mapcar*
	 (erbn-special-quote-function fcn)
	 ls args))



(defun fsi-mapcon (fcn ls &rest args)
  (require 'cl)
  (apply 'mapcon
	 (erbn-special-quote-function fcn)
	 ls args))






;;; Real Code:



(provide 'erbcspecial)
(run-hooks 'erbcspecial-after-load-hook)



;;; erbcspecial.el ends here
