;;; erbunlisp.el --- Help Simplify functions for nonlisp channels. 
;; Time-stamp: <2007-11-23 11:29:47 deego>
;; Copyright (C) 2003 D. Goel
;; Emacs Lisp Archive entry
;; Filename: erbunlisp.el
;; Package: erbunlisp
;; Author: D. Goel <deego@gnufans.org>
;; Keywords:
;; Version:
;; URL:  http://www.emacswiki.org/cgi-bin/wiki.pl?ErBot


(defconst erbunlisp-home-page
  "http://www.emacswiki.org/cgi-bin/wiki.pl?ErBot")


 
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
 

(defconst erbunlisp-version "0.0dev")

;;==========================================
;;; Requires:
(eval-when-compile (require 'cl))

;;; Code:

(defgroup erbunlisp nil
  "The group erbunlisp."
  :group 'applications)
(defcustom erbunlisp-before-load-hooks nil
  "Hooks to run before loading erbunlisp."
  :group 'erbunlisp)
(defcustom erbunlisp-after-load-hooks nil
  "Hooks to run after loading erbunlisp."
  :group 'erbunlisp)
(run-hooks 'erbunlisp-before-load-hooks)



;;; Real Code:

(defcustom erbunlisp-list
  '((fs-remove fs-forget remove))
  "When you type erbunlisp-install, the first entries will get aliased
to second one. 
When you type erbunlisp-uninstall, the first entries will get aliased
to the third one. "
  :group 'erbunlisp)



(defun erbunlisp-install ()
  (interactive)
  (mapcar 
   (lambda (arg)
     (defalias (first arg)
       (second arg)))
   erbunlisp-list))

(defun erbunlisp-uninstall ()
  (interactive)
  (mapcar 
   (lambda (arg)
     (defalias (first arg) (third arg)))
   erbunlisp-list))


(provide 'erbunlisp)
(run-hooks 'erbunlisp-after-load-hooks)



;;; erbunlisp.el ends here
