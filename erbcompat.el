;;; erbcompat.el --- Erbot GNU Emacs/XEmacs compatibility issues
;; Time-stamp: <2004-03-28 12:19:00 deego>
;; Copyright (C) 2004 S. Freundt
;; Emacs Lisp Archive entry
;; Filename: erbcompat.el
;; Package: erbot
;; Author:  Sebastian Freundt <freundt@math.TU-Berlin.DE>
;; Version: NA
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


(defvar erbot-on-xemacs-p nil
	"Whether erbot is run on xemacs.")

(setq erbot-on-xemacs-p
      (and (string-match "xemacs" emacs-version) t))


(or (and erbot-on-xemacs-p
	 (defun erbcompat-local-variable-p (variable &optional buffer)
	   "Just in compatibilty to GNU Emacs"
	   (local-variable-p variable (or buffer (current-buffer)))))
    (defalias 'erbcompat-local-variable-p 'local-variable-p))

(provide 'erbcompat)

;; erbcompat.el ends here
