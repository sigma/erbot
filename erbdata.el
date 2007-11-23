;;; erbdata.el --- 
;; Time-stamp: <2007-11-23 11:30:12 deego>
;; Copyright (C) 2002 D. Goel
;; Emacs Lisp Archive entry
;; Filename: erbdata.el
;; Package: erbdata
;; Author: D. Goel <deego@gnufans.org>
;; Version: 0.0DEV
;; URL:  http://www.emacswiki.org/cgi-bin/wiki.pl?ErBot
 

(defvar erbdata-home-page
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
 

;; See also:

(defvar erbdata-version "0.0dev")

;;==========================================
;;; Code:

(defgroup erbdata nil 
  "The group erbdata"
   :group 'applications)
(defcustom erbdata-before-load-hooks nil "" :group 'erbdata)
(defcustom erbdata-after-load-hooks nil "" :group 'erbdata)
(run-hooks 'erbdata-before-load-hooks)

(defvar erbdata-flames
  '(
    "%s: Are you smoking crack?"
    "%s: Is it larger than a breadbox?"
    "What are you smoking, %s?"
    "You are confused, but this is your normal state. "
    ))





(provide 'erbdata)
(run-hooks 'erbdata-after-load-hooks)



;;; erbdata.el ends here
