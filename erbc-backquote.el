;; 2004-08-20 T14:53:35-0400 (Friday)    D. Goel
;; This file is work in progress.  INCOMLPETE AND BUGGY.   DO NOT REQUIRE
;; THIS FILE IN A BOT.


(defvar backquote-symbols (list (intern (string 96)) 'backquote))


(defmacro backquote-parse (sexp)
  "Will parse a sexp and return an equivalent sexp with no backquotes
in it. Any backquotes in the sexp are converted them to a
nonbackquoted form. "
  (cond
   ((atom sexp) sexp)
   (t (cons 'quote (backquote-parse-unread sexp)))))



(defun backquote-parse-unread (sexp)
  (cond 
   ;;;((vectorp sexp)
   ;;;(error "this backquote parse does not deal with vectors. "))
   ((null sexp)
    nil)
   ((atom sexp)
    sexp)
   ((equal (car sexp) 'quote)
    (message "Answer is %s" sexp)
    sexp)
   ((member (car sexp) backquote-symbols)
    (backquote-inside-parse (cadr sexp)))
   ;; None of them:
   (t (cons (backquote-parse-unread (car sexp))
	    (backquote-parse-unread (cdr sexp))))))






(defun backquote-inside-parse (sexp)
  (cond
   ((null sexp)
    nil)
   ((atom sexp)
    (list 'quote sexp))
   ((equal (car sexp) ',)
    `(eval ,(backquote-parse-unread  (cadr  sexp))))
   (t (cons (backquote-inside-parse (car sexp))
	    (backquote-inside-parse (cdr sexp))))))



    



