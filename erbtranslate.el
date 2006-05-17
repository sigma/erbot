;;; erbtranslate.el --- Natural Language translation functions. 
;; Time-stamp: <2006-05-12 02:53:13 +0100 vivek>
;; Copyright (C) 2002 Alejandro Benitez
;; Emacs Lisp Archive entry
;; Filename: erbtranslate.el
;; Package: erbot
;; Authors: Alejandro Benitez <benitezalejandrogm@gmail.com>, 
;;          Vivek Dasmohapatra  <vivek@etla.org>
;;          Deepak Goel <deego@gnufans.org>
;; Maintainer: Vivek Dasmohapatra <vivek@etla.org>
;; Version: 0.1DEV
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

(defvar erbtranslate-version "0.1dev")

(require 'translate)

(defun erbtranslate-enabled-check ()
  (erbutils-enabled-check erbn-translate-p))

(defalias 'fsi-t8 'fsi-translate)

(defcustom erbn-translate-p nil 
 "Enabling this should be completely safe.  We do use call-process
here whenever passing any arguments to external commands.")

(defun fsi-translate (from to &rest text)
  (erbtranslate-enabled-check)
  (setq text (mapconcat #'(lambda (arg) (format "%s" arg)) text " ")
        from (format "%s" from)
        to   (format "%s" to  ))
  (condition-case caught
      (translate from to text)
    (error (concat "libtranslate error:" (cdr caught) )) ))

(defalias 'fsi-t8-l 'fsi-translate-list-pairs)

(defun fsi-translate-list-pairs (&optional from to &rest args)
  "Allow the user to search for translation pairs. Only gives counts 
unless both from and to are specified. *, any, - are allowed as wildcards."
  (erbtranslate-enabled-check)
  (let ((pair-data))
    (setq from      (format "%s" (or from "*"))
          to        (format "%s" (or to   "*"))
          pair-data (translate-list-pairs from to))
    (if (string-match "^\\(?:\*\\|any\\|-\\|\\)$" from) (setq from nil))
    (if (string-match "^\\(?:\*\\|any\\|-\\|\\)$" to  ) (setq to   nil))
    (cond 
     ( (and (not from) (not to)) ;; neither end point specified
       (concat 
        (format "%d language pair(s) available.\n" (length pair-data))
        "Specify an origin and/or destination language to see a list:\n"
        "  translate-list-pairs es ja\n"
        "  translate-list-pairs castilian\n"
        "  translate-list-pairs * zh-TW\n") )
     ( (or (not to) (not from)) ;; one end point specified
       (let ( (dir (if from "From" "To")) 
              (op  (if from 'cadr 'car))
              (s   nil)
              (x   (length pair-data)) 
              (fl  (format "%s" (or from to))) )
         (setq s  (mapcar (lambda (p) (car (funcall op p))) pair-data)
               fl (or (translate-full-name fl) fl))
         (apply 'concat 
                (format "%s %s: %d language(s) available.\n" dir fl x) 
                (if (<= (length s) 100) 
                    (list 
                     (mapconcat 
                      (lambda (x) (translate-full-name x)) s ", ")) ))) )
     (t ;; fully spec'd translation 
      (let ( (x (length pair-data)) )
        (setq from (or (translate-full-name from) from)
              to   (or (translate-full-name to  ) to  ))
        (apply 'concat 
               (format "%s -> %s: %d pair(s) available.\n" from to x) 
               (mapcar (lambda (x) 
                         (format "%s -> %s\n" 
                                 (princ (car  x)) 
                                 (princ (cadr x)))) pair-data)) )) ) ))

(defalias 'fsi-t8-s 'fsi-translate-list-services)

(defun fsi-translate-list-services (&rest args)
   (erbtranslate-enabled-check)
   (erbn-shell-command-to-string 
    (concat translate-program " --list-services")
    '(t)))

;; temporarily disabled till clean support is provided by translate.el

;; (defun fsi-translate-web-page (from to url &rest args)
;;   (erbtranslate-enabled-check)
;;   (shsp (list erbn-translate-program
;; 	      "-f" 
;; 	      (format "%s" from) "-t"
;; 	      (format "%s" to)
;; 	      (format "%s" url))))

;; (defalias 'fsi-t8-w 'fsi-translate-web-page)

(provide 'erbtranslate)
;;; erbtranslate.el ends here
