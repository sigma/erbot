;;; erbcspecial.el --- Special/dangerous implementation functions.
;; Many fs-functions can simply be defined in terms of other
;; fs-functions (and always should be!, for security.)
;; This file is for the remaining few, that can't be.
;; Thus, CODE IN THIS FILE SHOULD BE CONSTRUCTED VERY CAREFULLY.
1
;; Time-stamp: <2004-04-22 22:26:33 deego>
;; Copyright (C) 2004 D. Goel
;; Emacs Lisp Archive entry
;; Filename: erbcspecial.el
;; Package: erbcspecial
;; Author: D. Goel <deego@glue.umd.edu>
;; Keywords:
;; Version:
;; URL: http://gnufans.net/~deego
;; For latest version:

(defconst erbcspecial-home-page
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

(defconst erbcspecial-version "0.0-DUMMY")
(defun erbcspecial-version (&optional arg)
   "Display erbcspecial's version string.
With prefix ARG, insert version string into current buffer at point."
  (interactive "P")
  (if arg
      (insert (message "erbcspecial version %s" erbcspecial-version))
    (message "erbcspecial version %s" erbcspecial-version)))

;;==========================================
;;; Requires:
(eval-when-compile (require 'cl))

;;; Code:


;; (defun fs-mapcar-old (sym seq)
;;   "only symbols allowed at this time. "
;;   (unless (symbolp sym)
;;     (error "Function argument to mapcar for this bot can only be a symbol."))
;;   (setq sym (erblisp-sandbox-quoted sym))
;;   ;; everything should already be boxquoted.. cool
;;   (mapcar sym seq))

(defun fs-mapcar (fcn ls)
  (apply 'mapcar 
	 (erblisp-sandbox-quoted fcn)
	 ls nil))




;; (defun fs-mapc (sym seq)
;;   "only symbols allowed at this time. "
;;   (unless (symbolp sym)
;;     (error "Function argument to mapcar for this bot can only be a symbol."))
;;   (setq sym (erblisp-sandbox-quoted sym))
;;   ;; everything should already be boxquoted.. cool
;;   (mapc sym seq))

(defun fs-mapc (fcn ls)
  (apply 'mapc
	 (erblisp-sandbox-quoted fcn)
	 ls nil))



(defun fs-mapconcat (fcn ls)
  (apply 'mapconcat
	 (erblisp-sandbox-quoted fcn)
	 ls nil))





(defun fs-maplist (fcn ls &rest args)
  (apply 'maplist
	 (erblisp-sandbox-quoted fcn)
	 ls args))



(defun fs-mapl (fcn ls &rest args)
  (apply 'mapl
	 (erblisp-sandbox-quoted fcn)
	 ls args))

(defun fs-mapcar* (fcn ls &rest args)
  (apply 'mapcar*
	 (erblisp-sandbox-quoted fcn)
	 ls args))



(defun fs-mapcon (fcn ls &rest args)
  (require 'cl)
  (apply 'mapcon
	 (erblisp-sandbox-quoted fcn)
	 ls args))






;;; Real Code:



(provide 'erbcspecial)
(run-hooks 'erbcspecial-after-load-hook)



;;; erbcspecial.el ends here
