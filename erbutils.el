;;; erbutils.el --- 
;; Time-stamp: <2004-07-01 11:53:28 deego>
;; Copyright (C) 2002 D. Goel
;; Emacs Lisp Archive entry
;; Filename: erbutils.el
;; Package: erbutils
;; Author: D. Goel <deego@gnufans.org>
;; Version: 99.99
;; URL:  http://www.emacswiki.org/cgi-bin/wiki.pl?ErBot
 

(defvar erbutils-home-page
  "http://www.emacswiki.org/cgi-bin/wiki.pl?ErBot")


 
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


;; Quick start:
(defvar erbutils-quick-start
  "Help..."
)

(defun erbutils-quick-start ()
  "Provides electric help regarding variable `erbutils-quick-start'."
  (interactive)
  (with-electric-help
   '(lambda () (insert erbutils-quick-start) nil) "*doc*"))

(defvar erbutils-version "99.99")

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
  ;;   (interactive
  ;;    (let ((v (variable-at-point))
  ;;   (enable-recursive-minibuffers t)
  ;;   val)
  ;;      (setq val (completing-read (if (symbolp v)
  ;;            (format
  ;;             "Describe variable (default %s): " v)
  ;;          "Describe variable: ")
  ;;        obarray 'boundp t nil nil
  ;;        (if (symbolp v) (symbol-name v))))
  ;;      (list (if (equal val "")
  ;;         v (intern val)))))
  (unless (bufferp buffer) (setq buffer (current-buffer)))
  (if (not (symbolp variable))
      (message "Unknown variable or You did not specify a variable")
    (let (valvoid)
      (with-current-buffer buffer
  (with-output-to-temp-buffer "*Help*"
    ;; (prin1 variable)
    ;;    (if (not (boundp variable))
    ;;        (progn
    ;;    (princ " is void")
    ;;    (setq valvoid t))
    ;;      (let ((val (symbol-value variable)))
    ;;        (with-current-buffer standard-output
    ;;    (princ "'s value is ")
    ;;    (terpri)
    ;;    (let ((from (point)))
    ;;      (pp val)
    ;;      (help-xref-on-pp from (point))
    ;;      (if (< (point) (+ from 20))
    ;;          (save-excursion
    ;;      (goto-char from)
    ;;      (delete-char -1)))))))
    (terpri)
    (if (erbcompat-local-variable-p variable)
        (progn
    (princ (format "Local in buffer %s; " (buffer-name)))
    ;; (if (not (default-boundp variable))
    ;;        (princ "globally void")
    ;;      (let ((val (default-value variable)))
    ;;        (with-current-buffer standard-output
    ;;          (princ "global value is ")
    ;;  (terpri) Fixme: pp can take an age if you happen
    ;;          ;; to ask for a very large
    ;;          ;; expression.  We should
    ;;          ;; probably print it raw once
    ;;          ;; and check it's a sensible
    ;;          ;; size before prettyprinting.
    ;;          ;; -- fx
    ;;          (let ((from (point)))
    ;;      (pp val)
    ;;      (help-xref-on-pp from (point))
    ;;      (if (< (point) (+ from 20))
    ;;          (save-excursion
    ;;            (goto-char from)
    ;;            (delete-char -1)))))))
    (terpri)))
    (terpri)
    ;; (with-current-buffer standard-output
    ;;      (if (> (count-lines (point-min) (point-max)) 10)
    ;;    (progn
    ;;      ;; Note that setting the syntax table like below
    ;;      ;; makes forward-sexp move over a `'s' at the end
    ;;      ;; of a symbol.
    ;;      (set-syntax-table emacs-lisp-mode-syntax-table)
    ;;      (goto-char (point-min))
    ;;      (if valvoid
    ;;          (forward-line 1)
    ;;        (forward-sexp 1)
    ;;        (delete-region (point) (progn
    ;;        (end-of-line) (point)))
    ;;        (insert " value is shown below.\n\n")
    ;;        (save-excursion
    ;;          (insert "\n\nValue:"))))))
    ;; (princ "Documentation:")
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
        (help-xref-button 1 (lambda (v)
            (if help-xref-stack
                (pop help-xref-stack))
            (customize-variable v))
              variable
              "mouse-2, RET: customize variable")))))
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
      (help-xref-button
       1 (lambda (arg)
           (let ((location
            (find-variable-noselect arg)))
       (pop-to-buffer (car location))
       (goto-char (cdr location))))
       variable "mouse-2, RET: find variable's definition")))))

    (print-help-return-message)
    (save-excursion
      (set-buffer standard-output)
      ;; Return the text we displayed.
      (buffer-string)))))))

(defun erbutils-itemize (result &optional N shortenedp)
  (unless (integerp N) (setq N 0))
  (let ((ctr N)
  (rem result)
  (sofar ""))
    (if (equal (length result) 1)
  (setq sofar (format "%s" (car result)))
      (while rem
  (setq sofar (concat sofar (format "[%s] %s\n\n" ctr (car
                   rem))))
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
      (buffer-string)))
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


(defun erbutils-remove-text-properties (text)
  (with-temp-buffer
    (insert text)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun erbutils-defalias (ls &optional prefix prefix-rm)
  "Define new fs- aliases from ls. 

If the entry in the ls is a function, it is defaliased.  If it is a
variable, we define a new function, that will return the value of the
variable.

When prefix and prefix-rm is provided, we assume that the entry is of
the form prefix-rmENTRY. And we then (defalias fs-prefixENTRY
prefix-rmENTRY. "

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
     (setq expr (read (concat " ( " str " )"))))))


(defun erbutils-functions-in-file (file)
  "Returns the list of functions in the file.  File should be a valid
lisp file, else error. "
  (let ((str (erbutils-file-contents file))
	expr)
    (and 
     (stringp str)
     (not (string= str ""))
     (setq expr (read (concat " ( " str " )")))
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

;;; erbutils.el ends here
