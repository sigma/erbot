;;; erbtranslate.el --- Natural Language translation functions. 
;; Time-stamp: <2006-05-08 00:17:18 deego>
;; Copyright (C) 2002 Alejandro Benitez
;; Emacs Lisp Archive entry
;; Filename: erbc
;; Package: erbot
;; Authors: Alejandro Benitez <benitezalejandrogm@gmail.com>, 
;;         Deepak Goel <deego@gnufans.org>
;; Version: 0.0DEV
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

(defvar erbn-translate-program "translate" 
  "External program")


(defcustom erbn-translate-p nil 
 "Enabling this should be completely safe.  We do use call-process
here whenever passing any arguments to external commands.")

(defun fsi-translate (from to &rest text)
  (erbtranslate-enabled-check)
  (require 'shs)
  (setq text (mapconcat #'(lambda (arg) (format "%s" arg)) text " "))
  ;; =======================================================================
  ;; the temp file should be written as utf-8, hence coding-system-for-write
  ;; -----------------------------------------------------------------------
  ;; since we dump the file contents already encoded to utf-8 (that's what 
  ;; libtranslate expects), we must force the process to 'no-conversion to 
  ;; avoid double-encoding.
  ;; -----------------------------------------------------------------------
  ;; we might have to force the locale, according to the translate docs,
  ;; but this doesn't actually seem to be necessary at the moment.
  ;; -----------------------------------------------------------------------
  (let ((process-coding-system-alist '(("." . no-conversion)))
	(coding-system-for-write 'utf-8)
	(translation nil)
	(from-lang (format "%s" from))
	(to-lang   (format "%s" to))
	;;(locale (getenv "LC_ALL")) 
	)
    ;;(setenv "LC_ALL" nil)
    ;;(message "=> string is %S" (string-to-sequence text        'vector))
    (setq translation 
	  (shsp (list erbn-translate-program
		      "-f" from-lang "-t" to-lang) nil text))
    ;;(message "0 string is %sbyte" 
    ;;         (if (multibyte-string-p translation) "MULTI" "UNI"))
    ;;(setq translation (string-make-unibyte translation))
    ;;(message "1 string is %sbyte" 
    ;;         (if (multibyte-string-p translation) "MULTI" "UNI"))
    (setq translation (decode-coding-string translation 'utf-8))
    ;;(message "2 string is %sbyte" 
    ;;     (if (multibyte-string-p translation) "MULTI" "UNI"))
    ;;(message "<= string is %S" (string-to-sequence translation 'vector))
    translation))

(defalias 'fsi-t8-l 'fsi-translate-list-pairs)



(defun fsi-translate-list-pairs (&rest args)
  (erbtranslate-enabled-check)
  (erbn-shell-command-to-string (concat erbn-translate-program 
					" --list-pairs")
			       '(t)))



(defalias 'fsi-t8-s 'fsi-translate-list-services)


(defun fsi-translate-list-services (&rest args)
   (erbtranslate-enabled-check)
   (erbn-shell-command-to-string 
    (concat erbn-translate-program " --list-services")
    '(t)))




(defun fsi-translate-web-page (from to url &rest args)
  (erbtranslate-enabled-check)
  (shsp (list erbn-translate-program
	      "-f" 
	      (format "%s" from) "-t"
	      (format "%s" to)
	      (format "%s" url))))

(defalias 'fsi-t8-w 'fsi-translate-web-page)

(provide 'erbtranslate)
;;; erbtranslate.el ends here
