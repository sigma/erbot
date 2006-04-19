;;; erbtranslate.el --- Natural Language translation functions.  CURRENTLY INSECURE.
;; Time-stamp: <2006-04-19 16:01:44 deego>
;; Copyright (C) 2002 Alejandro Benitez
;; Emacs Lisp Archive entry
;; Filename: erbc
;; Package: erbotn
;; Author: "Alejandro Benitez" <benitezalejandrogm@gmail.com>
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


;; See also:




(defvar erbtranslate-version "0.0dev")



(defun fsi-translate (from to text)
  (erbn-translate from to text))

(defalias 'fsi-t8 'fsi-translate)

(defcustom erbn-translate-p nil
  "This is true by default.. (shell-command \"translate-bin\") has
exploits.
todo: erbot-paranoid-p
")


(defun erbn-translate (from to text)
  (error "Disabled for now because of security")
  (cond
   (t
    (erbn-shell-command-to-string (format "echo \"%s\" | translate-bin -f %s -t %s" text from to)
				  (list erbn-translate-p)
				  ))))

(defun fsi-translate-list-pairs ()
  (erbn-translate-list-pairs))

(defalias 'fs-t8-l 'fs-translate-list-pairs)

(defcustom erbn-translate-list-pairs-p t
  "This is true by default.. since (shell-command \"translate-bin\") is not
risky.. ")

(defun erbn-translate-list-pairs ()
  (cond
   (t
    (erbn-shell-command-to-string "translate-bin --list-pairs"
				  (list erbn-translate-list-pairs-p)
				  ))))

(defun fsi-translate-list-services ()
  (erbn-translate-list-services))

(defalias 'fs-t8-s 'fs-translate-list-services)

(defcustom erbn-translate-list-services-p t
  "This is true by default.. since (shell-command \"translate-bin\") is not
risky.. ")

(defun erbn-translate-list-services ()
  (cond
   (t
    (erbn-shell-command-to-string "translate-bin --list-services"
				  (list erbn-translate-list-services-p)
				  ))))

(defun fsi-translate-web-page (from to url)
  (erbn-translate-web-page from to url))

(defalias 'fs-t8-w 'fs-translate-web-page)

(defcustom erbn-translate-web-page-p t
  "This is true by default.. since (shell-command \"translate-bin\") is not
risky.. ")

(defun erbn-translate-web-page (from to url)
  (cond
   (t
    (erbn-shell-command-to-string (format "translate-bin -f %s -t %s %s" from to url)
				  (list erbn-translate-web-page-p)
				  ))))
(provide 'erbtranslate)
;;; erbtranslate.el ends here
