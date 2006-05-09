;;; erbtranslate.el --- Natural Language translation functions. 
;; Time-stamp: <2006-05-09 14:14:50 deego>
;; Copyright (C) 2002 Alejandro Benitez
;; Emacs Lisp Archive entry
;; Filename: erbc
;; Package: erbot
;; Authors: Alejandro Benitez <benitezalejandrogm@gmail.com>, 
;;          Deepak Goel <deego@gnufans.org>
;;          Vivek Dasmohapatra  <vivek@etla.org>
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

(defvar erbtranslate-pairs nil 
  "A cache for the language pairs. A list of entries of the form: \n
     '((fromaliases) (toaliases) (types)).\n
The first elements of fromaliases and toaliases are the canonical two letter
language codes (possibly with a -XX country variant extension). Any remaining
elements are human-readable aliases. (types) is a list of translation types, 
usually text, and occasionally web-page as well. No other types are currently 
known.")

(defvar erbtranslate-unsupported-langs '("ar" "he")
  "Languages (two letter codes) that we cannot utf-8 encode yet.")

(defun erbtranslate-enabled-check ()
  (erbutils-enabled-check erbn-translate-p))

(defalias 'fsi-t8 'fsi-translate)

(defvar erbn-translate-program "translate" 
  "External program")

(defcustom erbn-translate-p nil 
 "Enabling this should be completely safe.  We do use call-process
here whenever passing any arguments to external commands.")

(defun erbtranslate-req-to-pair (from to)
  (let ( (code nil) )
    (mapc 
     (lambda (p) 
       (if (and (member-ignore-case from (car  p)) 
                (member-ignore-case to   (cadr p)))
           (setq code (cons (caar p) (car (cadr p))) )) )
     erbtranslate-pairs)
    code))

(defun erbtranslate-full-name (code-or-name)
  "Return the full name of a language based on a code or one of its aliases."
  (let ((name nil) (lang nil) (ldata erbtranslate-pairs))
    (while (and ldata (not name))
      (setq lang (car ldata) ldata (cdr ldata))
      (if (member-ignore-case code-or-name (car lang))
          (setq lang (car lang))
        (if (member-ignore-case code-or-name (cadr lang)) 
            (setq lang (cadr lang)) 
          (setq lang nil)))
      (when lang  
        (setq name (mapconcat (lambda (l) (format "%s" l)) (cdr lang) " ")) ))
    name))

(defun fsi-translate (from to &rest text)
  (erbtranslate-enabled-check)
  (require 'shs)
  (setq text (mapconcat #'(lambda (arg) (format "%s" arg)) text " "))
  ;; =======================================================================
  ;; the temp file should be written as utf-8, hence coding-system-for-write
  ;; -----------------------------------------------------------------------
  ;; since we dump the file contents already encoded to utf-8 (that's what 
  ;; libtranslate expects), we must force the process to 'no-conversion to 
  ;; avoid double-encoding (with process-coding-system-alist).
  ;; -----------------------------------------------------------------------
  ;; we might have to force the locale, according to the translate docs,
  ;; but this doesn't actually seem to be necessary at the moment.
  ;; -----------------------------------------------------------------------
  (let ((process-coding-system-alist '(("." . no-conversion)))
	(coding-system-for-write 'utf-8)
	(translation nil)
        (code        nil)
	(from-lang (format "%s" from))
	(to-lang   (format "%s" to))
        ;;(locale (getenv "LC_ALL")) 
        )
    (setq code (erbtranslate-req-to-pair from-lang to-lang))
    (cond 
     ( (not code)
       (concat (format "%s -> %s: no matching translation services found.\n" 
                       from-lang to-lang)
               "Syntax: translate FROM TO TEXT\n") )
      ( (member (car code) erbtranslate-unsupported-langs)
        (format "Sorry, unicode support for %s is not yet complete." 
                (erbtranslate-full-name from-lang)) )
      ( (member (cdr code) erbtranslate-unsupported-langs)
        (format "Sorry, unicode support for %s is not yet complete." 
                (erbtranslate-full-name to-lang)) )
      (t
       (setq translation 
             (shsp (list erbn-translate-program
                         "-f" (car code) "-t" (cdr code)) nil text)
             translation (decode-coding-string translation 'utf-8))
      translation)) ))

(defalias 'fsi-t8-l 'fsi-translate-list-pairs)

(makunbound 'erbtranslate-pair-regex)
(defconst erbtranslate-pair-regex 
  (concat "^\\([a-z]\\{2,3\\}\\(?:-..\\)?\\)" ;; language code (from)
          "\\s-+" 
          "(\\(.*\\))"                        ;; language names (from)
          "\\s-+->\\s-+" 
          "\\([a-z]\\{2,3\\}\\(?:-..\\)?\\)"  ;; language code (to)
          "\\s-+"
          "(\\(.*\\)):"                       ;; language aliases (to)
          "\\s-+"
          "\\(.*\\)"))                        ;; capabilities

(defun erbtranslate-parse-pair (pair-line)
  "Parse a single line of output from translate --list-pairs, returns
an element for insertion into erbtranslate-pairs."
  (if (string-match erbtranslate-pair-regex pair-line)
    (let ( (from       (match-string 1 pair-line))
           (from-alias (match-string 2 pair-line))
           (to         (match-string 3 pair-line))
           (to-alias   (match-string 4 pair-line))
           (cap        (match-string 5 pair-line)) 
           (cleanup    (lambda (x) (replace-regexp-in-string ",.*" "" x))) 
           (from-names nil) 
           (to-names   nil))
      (setq from-alias (split-string from-alias ";")
            to-alias   (split-string to-alias   ";")
            from-alias (mapcar cleanup from-alias)
            to-alias   (mapcar cleanup to-alias  )
            cap        (split-string cap ",\\s-+"))
      (mapc (lambda (x)
              (let ((pos 0))
                (while (setq pos (string-match "\\<\\(\\S-+\\)\\>" x pos))
                  (setq  from-names (cons (match-string 1 x) from-names)
                         pos        (match-end 1)) ))) 
            from-alias)
      (mapc (lambda (x)
              (let ((pos 0))
                (while (setq pos (string-match "\\<\\(\\S-+\\)\\>" x pos))
                  (setq to-names (cons (match-string 1 x) to-names)
                        pos      (match-end 1)) ))) 
            to-alias)
      (list (cons from from-names) 
            (cons to   to-names  ) cap))
    (message "%S does not match.\n" pair-line) nil))

(defun erbtranslate-load-pairs ()
  "Parse the output of `erbn-translate-program' -l into `erbtranslate-pairs'"
  (when (not erbtranslate-pairs)
    (let ( (y nil) 
           (pair-text (erbn-shell-command-to-string
                       (concat erbn-translate-program " -l") '(t))) )
      (mapc
       (lambda (x) 
         (when (setq y (erbtranslate-parse-pair x)) 
           (setq erbtranslate-pairs (cons y erbtranslate-pairs))))
       (split-string pair-text "\n")) ))
  erbtranslate-pairs)

(defun fsi-translate-list-pairs (&optional from to &rest args)
  "Allow the user to search for translation pairs. Only gives counts 
unless both from and to are specified. *, any, - are allowed as wildcards."
  (erbtranslate-enabled-check)
  (setq from (format "%s" (or from "*"))
        to   (format "%s" (or to   "*")))
  (if (string-match "^\\(?:\*\\|any\\|-\\|\\)$" from) (setq from nil))
  (if (string-match "^\\(?:\*\\|any\\|-\\|\\)$" to  ) (setq to   nil))
  (if (not (erbtranslate-load-pairs))
      "translate doesn't seem to have been setup - no languages found."
    (cond 
     ( (and (not from) (not to)) ;; neither end point specified
       (concat 
        (format "%d language pair(s) available.\n" (length erbtranslate-pairs))
        "Specify an origin and/or destination language to see a list:\n"
        "  translate-list-pairs es ja\n"
        "  translate-list-pairs castilian\n"
        "  translate-list-pairs * zh-TW\n") )
     ( (or (not to) (not from)) ;; one end point specified
       (let ( (dir (if from "From" "To")) 
              (op  (if from 'car 'cadr))
              (s   nil)
              (x   0) 
              (fl  (format "%s" (or from to))) )
         (mapc 
          (lambda (p) (if (member-ignore-case fl (funcall op p)) 
                          (setq x (1+ x) s (cons p s))))
          erbtranslate-pairs)
         (setq fl (or (erbtranslate-full-name fl) fl))
         (apply 'concat 
                (format "%s %s: %d language(s) available.\n" dir fl x) 
                (if (<= (length s) 20) 
                    (mapcar (lambda (x) 
                              (format "%s -> %s\n" 
                                      (princ (car  x)) 
                                      (princ (cadr x)))) s)) )) )
     (t ;; fully spec'd translation 
      (let ( (s nil) 
             (x   0) 
             (fl (format "%s" from)) 
             (tl (format "%s" to  )) )
        (mapc 
         (lambda (p) 
           (if (and (member-ignore-case fl (car p)) (member tl (cadr p))) 
               (setq x (1+ x) s (cons p s)) )) 
         erbtranslate-pairs)
        (setq fl (or (erbtranslate-full-name fl) fl)
              tl (or (erbtranslate-full-name tl) tl))
        (apply 'concat 
               (format "%s -> %s: %d pair(s) available.\n" fl tl x) 
               (mapcar (lambda (x) 
                         (format "%s -> %s\n" 
                                 (princ (car  x)) 
                                 (princ (cadr x)))) s)) )) ) ))

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
