;;; erbc6.el --- fsbot functions contributed by freenode users, esp. #emacsers.
;; Time-stamp: <2003-08-16 15:19:25 deego>
;; Copyright (C) 2003 D. Goel
;; Emacs Lisp Archive entry
;; Filename: erbc6.el
;; Package: erbc6
;; Author: D. Goel <deego@glue.umd.edu> and #emacsers
;; Keywords:
;; Version:
;; URL: http://gnufans.net/~deego
;; For latest version:

(defconst erbc6-home-page
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
 

;;; Real Code:




(defun fs-C-h (sym &rest thing)
  "
;;; 2003-08-16 T15:19:00-0400 (Saturday)    D. Goel
Coded by bojohann on #emacs."
  (cond
   ((eq sym 'f)
    (apply 'df thing))
   ((eq sym 'k)
    (apply 'dk thing)) 
    ((eq sym 'c)
     more
     (apply 'describe-key-briefly thing))
    ((eq sym 'w)
     (apply 'dw thing))
    ((eq sym 'v)
     (apply 'dv thing))))

(provide 'erbc6)
(run-hooks 'erbc6-after-load-hook)



;;; erbc6.el ends here
