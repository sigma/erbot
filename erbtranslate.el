;;; erbtranslate.el --- Natural Language translation functions.  CURRENTLY INSECURE.
;; Time-stamp: <2006-04-24 12:37:46 deego>
;; Copyright (C) 2002 Alejandro Benitez
;; Emacs Lisp Archive entry
;; Filename: erbc
;; Package: erbot
;; Authors: Alejandro Benitez <benitezalejandrogm@gmail.com>, 
;;         Deepak Goel <deego@gnufans.org>
;; Version: 0.0DEV
;; URL:  http://www.emacswiki.org/cgi-bin/wiki.pl?ErBot

;;; THIS FILE IS WORK IN PROGRESS.  DO NOT USE IT IN YOUR BOT YET.  IT
;;; WORKS, BUT HAS SECURITY LOOPHOLES.


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

;; You need to install libtranslate for this to work.  The binary,
;; translate-bin, is provided, for example in Ubuntu Dapper:
;; http://packages.ubuntu.com/dapper/libs/libtranslate-bin
;; See also:

;; This file needs shs.el
;;(require 'shs)


(defvar erbtranslate-version "0.0dev")

(defun erbtranslate-enabled-check ()
  (erbutils-enabled-check erbn-translate-p))



(defalias 'fsi-t8 'fsi-translate)

(defcustom erbn-translate-p nil 
 "Enabling this should be completely safe.  We do use call-process
here whenever passing any arguments to external commands.")



(defun fsi-translate (from to text)
  (erbtranslate-enabled-check)
  (require 'shs)
  (shsp
   (list 
    "translate-bin" "-f" (format "%s" from)
    "-t" (format "%s" to))
   nil text))




(defalias 'fsi-t8-l 'fsi-translate-list-pairs)


(defun fsi-translate-list-pairs ()
  (erbtranslate-enabled-check)
  (erbn-shell-command-to-string "translate-bin --list-pairs"
				t))



(defalias 'fsi-t8-s 'fsi-translate-list-services)


(defun fsi-translate-list-services ()
   (erbtranslate-enabled-check)
   (erbn-shell-command-to-string "translate-bin --list-services"
				 t))




(defun fsi-translate-web-page (from to url)
  (erbtranslate-enabled-check)
  (shsp (list "translate-bin" "-f" 
	      (format "%s" from) "-t"
	      (format "%s" to)
	      (format "%s" url))))

(defalias 'fsi-t8-w 'fsi-translate-web-page)

(provide 'erbtranslate)
;;; erbtranslate.el ends here
