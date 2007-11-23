;;; erblog.el --- 
;; Time-stamp: <2007-11-23 11:30:08 deego>
;; Copyright (C) 2002 D. Goel
;; Emacs Lisp Archive entry
;; Filename: erblog.el
;; Package: erblog
;; Author: D. Goel <deego@gnufans.org>
;; Version: 0.0DEV
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
 

;; See also:

(defvar erblog-version "0.0dev")

;;==========================================
;;; Code:

(defgroup erblog nil 
  "The group erblog"
   :group 'applications)
(defcustom erblog-before-load-hooks nil "" :group 'erblog)
(defcustom erblog-after-load-hooks nil "" :group 'erblog)
(run-hooks 'erblog-before-load-hooks)


(defvar erblog-active-targets nil
  "This stores the list of targets that have had some activity...

The idea is that the operator sets this to nil (see commands
below).. goes away, comes back and examined this variables to find
out which channels have had activity...
")

(defun erblog-log-target (target &rest stuff)
  (unless (member (format "%s" target)
		  erblog-active-targets)
    (progn
      (add-to-list 'erblog-active-targets
		   (format "%s" target))
      (erblog-show-targets))))
    
;; operator bind to C-c s
(defun erblog-show-targets ()
  (interactive)
  (message "%s" erblog-active-targets))

;; bind to C-c r
(defun erblog-reset-targets ()
  (interactive)
  (message "Nulling.. was %s" erblog-active-targets)
  (setq erblog-active-targets nil))

(provide 'erblog)
(run-hooks 'erblog-after-load-hooks)



;;; erblog.el ends here
