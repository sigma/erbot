;;; erbutils.el --- 
;; Time-stamp: <2007-11-23 11:29:44 deego>
;; Copyright (C) 2002,2003,2004,2005  D. Goel
;; Emacs Lisp Archive entry
;; Filename: erbutils.el
;; Package: erbutils
;; Author: D. Goel <deego@gnufans.org>
;; Version: 0.0dev
;; URL:  http://www.emacswiki.org/cgi-bin/wiki.pl?ErBot
 

(defvar erbutils-home-page
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
 
(defvar erbutils-version "0.0dev")

;;==========================================
;;; Code:
(require 'rot13)

(defgroup erbutils nil 
  "The group erbutils"
   :group 'applications)
(defcustom erbutils-before-load-hooks nil "" :group 'erbutils)
(defcustom erbutils-after-load-hooks nil "" :group 'erbutils)
(run-hooks 'erbutils-before-load-hooks)


(defalias 'erbutils-stringize 'erbutils-stringify)
;; should please not eval anyting... since called by erbc..

(defun erbutils-stringify (msg-list)
  (if (stringp msg-list)
      msg-list
    (mapconcat 
     '(lambda (arg)
  (if (stringp arg) arg
    (format "%s" arg)))
     msg-list " " )))


(defun erbutils-string= (foo bar &optional ignore-case)
  (and foo bar 
       (if ignore-case 
     (string= (downcase foo) (downcase bar))
   (string= foo bar))))


(defun erbutils-errors-toggle ()
  (interactive)
  (setq erbutils-ignore-errors-p 
  (not erbutils-ignore-errors-p))
  (message "erbutils-ignore-errors-p set to %s"
     erbutils-ignore-errors-p))


(defvar erbutils-ignore-errors-p t)
(defmacro erbutils-ignore-errors (&rest body)
  "DOES NOT return nil, unlike ignore-errors.."
  (let ((err (gensym)))
    `(condition-case ,err (progn ,@body)
       (error
  (progn
          ;(ding t)
          ;(ding t)
    ;;(message "ERROR: %s" (error-message-string ,err))
    ;;(sit-for 1)
    (ding t)
    (unless erbutils-ignore-errors-p
      (error (error-message-string ,err)))
    (unless fs-found-query-p 
      (erbutils-error 
       "%s"
       (fs-limit-lines
        (error-message-string ,err)))))))))

(defvar erbutils-error-debug-p nil
  "Turn on for debugging.."
  )
(defun erbutils-error (&rest args)
  (cond
   (erbutils-error-debug-p (apply 'error args))
   (t 
    (unless args (error 
		  (format "Syntax: , (fs-error msg &rest format-args)")))
    (let* ((main
	    (erbutils-random 
	     '("oops, error.  %s"
	       ;;"Blue Screen: %s"
	       "BEEEP: %s"
	       "ERROR: %s"
	       "err..%s"
	       ":(   %s"
	       "Doh!  %s"
	       "Oh sh**!  %s"
	       "Nooo!  %s"
	       "oops,  %s"
	       "Uh oh,  %s"
	       "whoops,  %s"
	       )))
	   (result
	    (format main
		    (apply 'format args))))
      (or
       (ignore-errors
	 (fs-h4x0r-maybe
	  (fs-studlify-maybe
	   result)))
       result)))))



(defun erbutils-matching-functions (string)
  "returns all functions that start with string"
  (apropos-internal (concat "^" (regexp-quote string))
        'fboundp)
  
  ;; (let* ((results nil)
;;;    (len (- (length obarray) 1))
;;;    (ctr 0))
;;;     (while (< ctr len)
;;;       (incf ctr)
;;;       (if (and
;;;      (equal (string-match string (format "%s" (aref obarray
;;;                 ctr)))
;;;       0)
;;;      (fboundp (aref obarray ctr))
;;;      )
;;;     (push (aref obarray ctr) results)))
;;;     results)
)





 (defun erbutils-quote-list (ls)
   "ls is, in general, a tree...

 We will make sure here that each element of the tree that is a symbol gets
 quoted...    


 "
   (mapcar '(lambda (arg)
       (list 'quote arg))
    ls))

(defun erbutils-random (list &optional weights)
  "Return a random element from list. 
Optional WEIGHTS are relative.  They should be integers. 
example:  (erbutils-random '(a b c) '(1 1 2)) should return c twice
as many times as it returns a...
"
  (cond
   ((null weights) 
    (nth (random (length list)) list))
   (t
    (let* ((len (length list))
	   (revw (reverse weights))
	   (fir (car revw))
	   )
      ;; If weights are partially specified, fill in missing entries. 
      (while (< (length revw) len)
	(setq revw (cons fir revw)))
      (setq weights (reverse revw))
      (let* ((total (apply '+ weights))
	     (choice (random total))
	     (curw weights)
	     (ctr 0)
	     (num 0))
	
	(while (>= choice (+ ctr (car curw)))
	  (setq ctr (+ ctr (car curw)))
	  (incf num)
	  (setq curw (cdr curw)))
	(nth num list))))))



(defun erbutils-describe-variable (&optional variable buffer)
  "Like describe-variable, but doesn't print the actual value.."
  (unless (bufferp buffer) (setq buffer (current-buffer)))
  (if (not (symbolp variable))
      (message "Unknown variable or You did not specify a variable")
    (let (valvoid)
      (with-current-buffer buffer
  (with-output-to-temp-buffer "*Help*"
    (terpri)
    (if (erbcompat-local-variable-p variable)
        (progn
	  (princ (format "Local in buffer %s; " (buffer-name)))

	  (terpri)))
    (terpri)
    (let ((doc 
	   (documentation-property variable 'variable-documentation)))
      (princ (or doc "not documented as a variable.")))
    (help-setup-xref (list #'describe-variable variable (current-buffer))
		     (interactive-p))
    
    ;; Make a link to customize if this variable can be customized.
    ;; Note, it is not reliable to test only for a custom-type property
    ;; because those are only present after the var's definition
    ;; has been loaded.
    (if (or (get variable 'custom-type) ; after defcustom
	    (get variable 'custom-loads) ; from loaddefs.el
	    (get variable 'standard-value)) ; from cus-start.el
        (let ((customize-label "customize"))
	  (terpri)
	  (terpri)
	  (princ (concat "You can " customize-label " this variable."))
	  (with-current-buffer "*Help*"
	    (save-excursion
	      (re-search-backward
	       (concat "\\(" customize-label "\\)") nil t)
              (if (> 22 emacs-major-version)
                  (help-xref-button 1 (lambda (v)
                                        (if help-xref-stack
                                            (pop help-xref-stack))
                                        (customize-variable v))
                                    variable
                                    "mouse-2, RET: customize variable")
                (help-xref-button 1 'help-customize-variable variable))
              ))))
    ;; Make a hyperlink to the library if appropriate.  (Don't
    ;; change the format of the buffer's initial line in case
    ;; anything expects the current format.)
    (let ((file-name (symbol-file variable)))
      (when file-name
        (princ "\n\nDefined in `")
        (princ file-name)
        (princ "'.")
        (with-current-buffer "*Help*"
    (save-excursion
      (re-search-backward "`\\([^`']+\\)'" nil t)
      (if (> 22 emacs-major-version)
          (help-xref-button
           1 (lambda (arg)
               (let ((location
                      (find-variable-noselect arg)))
                 (pop-to-buffer (car location))
                 (goto-char (cdr location))))
           variable "mouse-2, RET: find variable's definition") 
        (help-xref-button 1 'help-variable-def variable file-name)) 
      ))))

    (print-help-return-message)
    (save-excursion
      (set-buffer standard-output)
      ;; Return the text we displayed.
      (buffer-substring-no-properties (point-min) (point-max))))))))


(defvar erbutils-itemize-style
  (list "[%s] %s\n\n" "[%s] %s\n\n" "[%s] %s,\n\n")

  "Another good choice, for example, and used by petekaz's petebot, is  
  \(list \"[%s] %s,\n\n\" \"and also [%s] %s\n\n\" \"and [%s] %s,\n\n\")
")

(defun erbutils-itemize (result &optional N shortenedp style)
  (unless style (setq style erbutils-itemize-style))
  (unless (integerp N) (setq N 0))
  (let 
      ((st1 (first style))
       (st2 (second style))
       (st3 (third style))
       (ctr N)
       (rem result)
       (sofar ""))
    (if (equal (length result) 1)
	(setq sofar (format "%s" (car result)))
      (while rem
	(setq sofar 
	      (concat 
	       sofar 
	       (format 
		(cond 
		 ((= ctr 0) 
		  st1)
		 ((null (rest rem))
		  st2)
		 (t st3))
		ctr 
		(car rem))))
	(setq ctr (+ ctr 1))
	(setq rem (cdr rem))))
    (when shortenedp 
      (setq sofar (concat sofar " .. + other entries")))
    sofar))



(defun erbutils-function-minus-doc (fstr &rest ignore)
  "fstr is the string containing the function"
  (let* ((fdoc (if (stringp fstr) fstr (format "%s" fstr)))
   newdoc)
    (setq newdoc
    (with-temp-buffer 
      (insert fdoc)
      (goto-char (point-min))
      (search-forward "(" nil t)
      (forward-sexp 4)
      (if (stringp (sexp-at-point))
    ;; this sets mark.. bad programming, i know..
    (backward-kill-sexp 1))
      (erbutils-buffer-string)))
    (erbutils-single-lines newdoc)))

(defun erbutils-single-lines (str)
  "Eliminates all \n or lines comprising entirely of whitespace"
  (mapconcat 
   'identity
   (delete-if
    (lambda (str) 
      (string-match "^[ \t]*$" str))
    (split-string str
      "\n"))
   "\n"))

(defun erbutils-cleanup-whitespace (str)
  "Strip all leading whitespace and replace one or more tabs, newlines,
or spaces with a single space."
  (let ((result (replace-regexp-in-string "[\t\n ]+" " " str)))
    (subseq result (or (position ?  result :test-not 'eq) 0))))

(defun erbutils-downcase (str)
  (if (stringp str)
      (downcase str) 
    str))






(defun erbutils-add-nick (msg)
  (if
      (and (not fs-found-query-p)
     (not fs-internal-directed)
     (> (random 100) 30)
     (stringp msg))
      (eval 
       (erbutils-random
  '(
    ;;(concat msg ", " fs-nick)
    (concat fs-nick ": " msg)
    (concat fs-nick ", " msg)
    )
  '(1 1 )))
    msg))


(defun erbutils-add-nick-maybe (msg)
  (eval 
   (erbutils-random
    '((erbutils-add-nick msg)
      msg)
    fs-internal-add-nick-weights
    )))


(defun erbutils-convert-sequence (arg)
  (if (sequencep arg)
    arg
    (format "%s" arg)))


(defvar erbutils-eval-until-limited-length 70)
(defun erbutils-eval-until-limited (expr)
  (let 
      ((ans nil) (donep nil))
    (while (not donep)
      (setq ans
      (eval expr))
      (setq donep (<= (length (format "%s" ans)) 
         erbutils-eval-until-limited-length)))
    ans))



(defun erbutils-replace-strings-in-string (froms tos str &rest
             args)
  (let ((st str))
    (mapcar*
     (lambda (a b)
       (setq st (apply 'erbutils-replace-string-in-string
           a b st args)))
     froms tos)
    st))
  
;;;###autoload
(if (featurep 'xemacs)
    (defun erbutils-replace-string-in-string (from to string &optional
                   delimited start end)
    (save-excursion
      (with-temp-buffer
        (insert string)
        (save-restriction
          (narrow-to-region (or start (point-min)) (or end (point-max)))
          (goto-char (point-min))
          (replace-string from to delimited))
        (buffer-substring-no-properties (point-min) (point-max)))))
  (defun erbutils-replace-string-in-string (from to string &optional
                   delimited start end)
    (save-excursion
      (with-temp-buffer
        (insert string)
        (goto-char (point-min))
        (replace-string from to delimited start end)
        (buffer-substring-no-properties (point-min) (point-max))))))

(defun erbutils-sublist-p (a b &optional start)
  "tells if list a is a member of list b.  If start is true, the match
should start at the beginning of b."
 (cond
  ((null a) t)
  ((null b) nil)
  (start (and
    (equal (car a) (car b))
    (erbutils-sublist-p (cdr a) (cdr b) t)))
  (t
   (let ((foo (member (car a) b)))
     (and foo 
    (or 
     (erbutils-sublist-p (cdr a) (cdr foo) t)
     (erbutils-sublist-p a (cdr foo))))))))

;;;###autoload
(defun erbutils-flatten (tree)
  (cond
   ((null tree) nil)
   ((listp tree) (apply 'append
      (mapcar 'erbutils-flatten tree)))
   (t (list tree))))
 
(provide 'erbutils)
(run-hooks 'erbutils-after-load-hooks)


(defun erbutils-remove-text-properties (str1)
;;;   (with-temp-buffer
;;;     (insert text)
;;;     (buffer-substring-no-properties (point-min) (point-max))))
  ;; fledermaus' code: avoid with-temp-buffer becuse of i8n problems.
  (let ((str (copy-sequence str1)))
    (set-text-properties 0 (length str) nil str) 
    str))



(defun erbutils-defalias-i (ls &optional prefix prefix-rm
			       functionpref)
  "Similar to erbutils-defalias, except that for functions, it
defaliases a 'fsi-"
  (unless functionpref (setq functionpref "fsi-"))
  (erbutils-defalias ls prefix prefix-rm functionpref))


(defun erbutils-defalias (ls &optional prefix prefix-rm functionpref)
  "Define new fs- aliases from ls. 

If the entry in the ls is a function, it is defaliased.  If it is a
variable, we define a new function, that will return the value of the
variable.

When prefix and prefix-rm is provided, we assume that the entry is of
the form prefix-rmENTRY. And we then (defalias fs-prefixENTRY
prefix-rmENTRY. 

functionpref should usually be fs-.  If you want fsi- instead, you
might prefer calling erbutils-defalias-i instead.
"
  (unless functionpref (setq functionpref "fs-"))
  (let* ((pref (if prefix (format "%s" prefix) ""))
   (pref-rm (if prefix-rm (format "%s" prefix-rm) ""))
   (lenrm (length pref-rm))
   (reg (concat "^" (regexp-quote pref-rm))))
    (mapcar 
     (lambda (arg)
       (let* (      
	      (argst (format "%s" arg))
	      (gop (string-match reg argst))
	      (arg2 (and gop (substring argst lenrm)))
	      (foo (and gop (intern (format (concat functionpref "%s%s")
					    pref arg2)))))
	      
	 (when gop
	   (if (functionp arg)
	       (defalias foo arg)
	     (erbutils-defalias-vars (list arg prefix prefix-rm))
	     ;;`(defun ,foo () 
	     ;;   ,(concat "Pseudo function that returns the value of `"
	     ;;    argst "'. ")
	     ;;,arg)
	     ))))
     ls)))

(defun erbutils-defalias-vars (ls &optional prefix prefix-rm)
  (let* ((pref (if prefix (format "%s" prefix) ""))
   (pref-rm (if prefix-rm (format "%s" prefix-rm) ""))
   (lenrm (length pref-rm))
   (reg (concat "^" (regexp-quote pref-rm))))
    (mapcar 
     (lambda (arg)
       (let* (      
        (argst (format "%s" arg))
        (gop (string-match reg argst))
        (arg2 (and gop (substring argst lenrm)))
        (foo (and gop (intern (format "fs-%s%s" pref arg2)))))

   (when gop
     (eval 
      `(defun ,foo () 
         ,(concat "Pseudo function that returns the value of `"
      argst "'. ")
         ,arg)))))
     ls)))
      

(defun erbutils-region-to-string (fcn &rest  str)
  (with-temp-buffer
    (while str 
      (let ((aa (car str)))
  (when aa
    (insert (format "%s " aa))))
      (pop str))
    (goto-char (point-min))
    (funcall fcn (point-min) (point-max))
    (buffer-substring-no-properties (point-min) (point-max))))


(defun erbutils-rot13 (str)
  (apply
   'string
   (mapcar
    (lambda (i)
      (let ((foo (aref rot13-display-table i)))
  (if foo (aref foo 0) i)))
    str)))

(defun erbutils-file-contents (file)
  (cond
   ((not (file-exists-p file))
    "")
   (t 
    (with-temp-buffer 
      (insert-file-contents file)
      (buffer-substring-no-properties (point-min) (point-max))))))


(defun erbutils-file-sexps (file)
  (let ((str (erbutils-file-contents file))
	expr)
    (and 
     (stringp str)
     (not (string= str ""))
     (setq expr (erbn-read (concat " ( " str " )"))))))


(defun erbutils-functions-in-file (file)
  "Returns the list of functions in the file.  File should be a valid
lisp file, else error. "
  (let ((str (erbutils-file-contents file))
	expr)
    (and 
     (stringp str)
     (not (string= str ""))
     (setq expr (erbn-read (concat " ( " str " )")))
     (ignore-errors (mapcar 'second expr)))))


    
(defun erbutils-mkback-maybe (file)
  (ignore-errors (require 'mkback))
  (ignore-errors 
    (let ((mkback-interactivity -100))
      (mkback file))))


(defun erbutils-listp-proper (l) 
  "from <Riastradh>"
  (or (null l) (and (consp l)
		    (erbutils-listp-proper (cdr l)))))


(defun erbutils-html-url-p (str)
  "Guesses if the string is a url that will yield HTML content.
Basically, look for any url that doesn't have any extension or
one that has .html, .shtml, or .htm.  Returns the str if it is
a valid url that might generate HTML."
  (when (string-match "^http://[^/]+/?\\(.*\\)?$" str)
    (let* ((path (match-string 1 str))
           (pos (position ?. path :from-end)))
      (when (or (null pos)
                (string-match "html?" (subseq path pos)))
        str))))


;;;###autoload
(defun erbutils-concat-symbols (&rest args)
  "Like `concat' but applies to symbols, and returns an interned
concatted symbol.  Also see fsbot's
`erbn-command-list-from-prefix'.  

Thanks to edrx on #emacs for suggesting 'symbol-name.."
  (let* ((strings (mapcar 'symbol-name args))
	 (str (apply 'concat strings)))
    (intern str)))




(defun erbutils-remove-text--properties (str)
  (let (str2)
    (cond
     ((stringp str)
      (setq str2 (copy-sequence str))
      (set-text-properties 0 (length str2) nil str2)
      str2)
     (t (error "Not a string.")))))




(defun erbutils-remove-text-properties-maybe (str)
  (if (stringp str) 
      (erbutils-remove-text-properties str)
    str))


(defun erbutils-buffer-string ()
  (buffer-substring-no-properties (point-min) (point-max)))


(defmacro erbutils-enabled-check (var)
  `(when (or erbot-paranoid-p (not ,var))
     (error "Variable %s is disabled, or erbot-paranoid-p is t. " ',var)))

;;; erbutils.el ends here
