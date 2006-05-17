;; Emacs Lisp Archive Entry
;; Package: translate
;; Filename: translate.el
;; Version: 0.01
;; Keywords: natural language, language, translate, translation
;; Author: Vivek Dasmohapatra <vivek@etla.org>
;; Maintainer: Vivek Dasmohapatra <vivek@etla.org>
;; Created: 2006-05-10
;; Description: use gnome translate/libtranslate to translate text
;; Compatibility: Emacs21, Emacs22
;; Last modified: Fri 2006-05-12 02:52:44 +0100


;; Based on work by:
;; Deepak Goel <deego@gnufans.org>
;; Alejandro Benitez <benitezalejandrogm@gmail.com>

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
;; translate and the library libtranslate.so are provided (for example) 
;; in Ubuntu Dapper: http://packages.ubuntu.com/dapper/libs/libtranslate-bin

(defvar translate-version "0.01")

(defvar translate-pairs nil 
  "A cache for the language pairs. A list of entries of the form: \n
     '((fromaliases) (toaliases) (types)).\n
The first elements of fromaliases and toaliases are the canonical two letter
language codes (possibly with a -XX country variant extension). Any remaining
elements are human-readable aliases. (types) is a list of translation types, 
usually text, and occasionally web-page as well. No other types are currently 
known.")

(defvar translate-unsupported-langs '("ar" "he" "pap")
  "Languages (two/three letter codes) that we cannot utf-8 encode yet.")

(defgroup translate nil
  "Translate natural languages using gnome translate (or workalikes)."
  :group 'external
  :prefix "translate-")

(defcustom translate-program "translate" 
  "External translation program."
  :group 'translate
  :type '(choice string file))

(defun translate-req-to-pair (from to)
  "Taking a pair of string arguments, find a matching translation service
and return it as a cons of the form (\"origin\" . \"dest\")"
  (translate-load-pairs)
  (let ( (code nil) )
    (mapc (lambda (p) (if (and (member-ignore-case from (car  p)) 
                               (member-ignore-case to   (cadr p)))
                          (setq code (cons (caar p) (car (cadr p))) )) )
          translate-pairs)
    code))

(defun translate-full-name (code-or-name)
  "Return the full name of a language based on a code or one of its aliases."
  (interactive "sLanguage (eg en or zh-TW): ")
  (translate-load-pairs)
  (let ((name nil) (lang nil) (ldata translate-pairs))
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

(defconst translate-pair-regex 
  (concat "^\\([a-z]\\{2,3\\}\\(?:-..\\)?\\)" ;; language code (from)
          "\\s-+" 
          "(\\(.*\\))"                        ;; language names (from)
          "\\s-+->\\s-+" 
          "\\([a-z]\\{2,3\\}\\(?:-..\\)?\\)"  ;; language code (to)
          "\\s-+"
          "(\\(.*\\)):"                       ;; language aliases (to)
          "\\s-+"
          "\\(.*\\)"))                        ;; capabilities

(defun translate-parse-pair (pair-line)
  "Parse a line of output from `translate-program' --list-pairs, return
an element for insertion into `translate-pairs'."
  (if (string-match translate-pair-regex pair-line)
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

(defun translate-load-pairs (&optional reload)
  "Parse the output of `translate-program' -l into `translate-pairs'
Called interactively with a prefix argument, or non-interactively with a 
non-nil reload argument, it will empty translate-pairs first. Otherwise,
if translate-pairs has already been loaded, it will not do anything."
  (interactive "P")
  (if reload (setq translate-pairs nil))
  (when (not translate-pairs)
    (let ( (y nil) 
           (pair-text (shell-command-to-string
                       (concat translate-program " -l"))) )
      (mapc
       (lambda (x) 
         (when (setq y (translate-parse-pair x)) 
           (setq translate-pairs (cons y translate-pairs))))
       (split-string pair-text "\n")) ))
  translate-pairs)

(defun translate-list-pairs (&optional from to)
  "Return the subset of `translate-pairs' that matches the FROM and TO
arguments."
  (if (string-match "^\\(?:\*\\|any\\|-\\|\\)$" from) (setq from nil))
  (if (string-match "^\\(?:\*\\|any\\|-\\|\\)$" to  ) (setq to   nil))
  (if (not (translate-load-pairs))
      (error "translate doesn't seem to have been setup - no languages found.")
    (cond 
     ( (and (not from) (not to)) ;; neither end point specified
       translate-pairs )
     ( (or (not to) (not from))  ;; one end point specified
       (let ( (op  (if from 'car 'cadr))
              (op2 (if from 'cadr 'car))
              (s   nil)
              (fl  (format "%s" (or from to))) )
         (mapc (lambda (p) (if (member-ignore-case fl (funcall op p)) 
                               (setq s (cons p s))))
               translate-pairs)
         s ))
     (t ;; fully spec'd translation 
      (let ( (s nil) (fl (format "%s" from)) (tl (format "%s" to  )) )
        (mapc (lambda (p) 
                (if (and (member-ignore-case fl (car  p)) 
                         (member-ignore-case tl (cadr p))) 
                    (setq s (cons p s)) )) 
              translate-pairs)
        s) )) ))

(defun translate (from to &rest text)
  "Given a language code or language name for the origin and destination 
languages FROM and TO (see `translate-pairs') and some TEXT, returns a string
containing the translated text from `translate-program' (gnome translate
or a work-alike). If an error occurs, either internally or while invoking 
`translate-program', signals an `error' instead."
  (setq text (mapconcat #'(lambda (arg) (format "%s" arg)) text " "))
  ;; =======================================================================
  ;; we might have to force the locale, according to the translate docs,
  ;; but this doesn't actually seem to be necessary at the moment.
  ;; -----------------------------------------------------------------------
  ;; call-process should use utf-8, that's what libtranslate wants: hence 
  ;; we set process-coding-system-alist.
  ;; -----------------------------------------------------------------------
  (let ( (process-coding-system-alist '(("." . utf-8)))
         (from-lang (format "%s" from))
         (to-lang   (format "%s" to)) 
         (translation nil)  ;; translated text, or libtranslate error
         (code        nil)  ;; cons of (origin-lang . dest-lang)
         (status      nil) );; return code of command. 0 => success.
    (setq code (translate-req-to-pair from-lang to-lang)
          from (car code)
          to   (cdr code))
    (cond 
     ( (not code)
       (error "%s -> %s: no matching translation services found.\n" 
              (or (translate-full-name from-lang) from-lang)
              (or (translate-full-name to-lang  ) to-lang  )) )
     ( (member (car code) translate-unsupported-langs)
       (error "Sorry, unicode support for %s is not yet complete." 
              (translate-full-name from-lang)) )
     ( (member (cdr code) translate-unsupported-langs)
       (error "Sorry, unicode support for %s is not yet complete." 
              (translate-full-name to-lang)) )
     ( t
       (with-temp-buffer 
         (insert text)
         (setq status 
               (call-process-region (point-min) (point-max) translate-program 
                                    :delete-input (current-buffer) nil
                                    "-f" from "-t" to) 
               translation (buffer-string)) )) ) 
    (if (/= 0 status) 
        (error "%d - %s" status translation)) 
    translation))

(provide 'translate)