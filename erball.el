;;; erball.el --- Functions on all files. 
;; Time-stamp: <2005-07-01 08:20:54 deego>
;; Copyright (C) 2002 D. Goel
;; Emacs Lisp Archive entry
;; Filename: erbc.el
;; Package: erbc
;; Author: D. Goel <deego@gnufans.org>
;; Version: 



;; Usually maintenance
;; not all of these may be required depending on how you use erbot..
(require 'cl)

;; Compilation

(defvar erball-compilation-paths-rel-to
  (let (args ret)
    (while command-line-args-left
      (if (string= "--paths-rel-to" (car command-line-args-left))
          (progn
            (setq ret (cadr command-line-args-left))
            (setq command-line-args-left (cddr command-line-args-left)))
        (add-to-list 'args (car command-line-args-left) t)
        (setq command-line-args-left (cdr command-line-args-left))))
    (setq command-line-args-left args)
    ret)
  "Text to be prepended to each element in `erball-compilation-paths'.
Can be specified by passing \"--paths-rel-to ARG\" on the emacs
command line.
This value is also added to the load-path.
A trailing backslash is required.")

(defvar erball-compiling-p
  (if (assoc "--compile-erbot" command-line-args-left)
      (progn
        (message (concat "\nCompiling source in "
                         (file-name-nondirectory (expand-file-name "."))
                         " ...\n"))
        (setq command-line-args-left
              (delete "--compile-erbot" command-line-args-left))
        t)
    nil)
  "Determine whether erbot is currently being compiled.")

(defcustom erball-compilation-paths
  '("contrib"
    ".."
    "../erc")
  "Elements to add to the load path during compilation.
If `erball-compilation-paths-rel-to' is specified, it is
prepended to each element and also added verbatim to the path.
The current directory is automatically added to the path."
  :group 'erball
  )

(when erball-compiling-p
  (add-to-list 'load-path ".")
  (when erball-compilation-paths-rel-to
    (add-to-list 'load-path erball-compilation-paths-rel-to))
  (dolist (dir erball-compilation-paths)
    (add-to-list 'load-path
                 (concat erball-compilation-paths-rel-to dir))))

;; Load all erbot files

(defmacro erball-ignore-errors-loudly (&rest body)
  "Like ignore-errors, but tells the error..

Copied from deego's `ignore-errors-my', which owes some of its work
to: Kalle on 7/3/01:
 * used backquote: something i was too lazy to convert my macro to..
 * removed the progn: condition-case automatically has one..
 * made sure that the return is nil.. just as it is in ignore-errors. "
  (let ((err (gensym)))
    `(condition-case ,err (progn ,@body)
       (error
	(ding t)
	(ding t)
	(ding t)
	(message "IGNORED ERROR: %s" (error-message-string ,err))
	(sit-for 1)
	nil))))



(erball-ignore-errors-loudly (require 'doctor))
(erball-ignore-errors-loudly (require 'erc))
(erball-ignore-errors-loudly (require 'erc-stamp))
(unless noninteractive (erball-ignore-errors-loudly (require 'dunnet)))
(erball-ignore-errors-loudly (require 'erbot))
(erball-ignore-errors-loudly (require 'erbcountry))
(erball-ignore-errors-loudly (require 'erbutils))
(erball-ignore-errors-loudly (require 'erblog))
(erball-ignore-errors-loudly (require 'erbeng))
(erball-ignore-errors-loudly (require 'erbdata))
(erball-ignore-errors-loudly (require 'erbkarma))
(erball-ignore-errors-loudly (require 'erblisp))
(erball-ignore-errors-loudly (require 'erbc))
(erball-ignore-errors-loudly (require 'erbc2))
(erball-ignore-errors-loudly (require 'erbc3))
(erball-ignore-errors-loudly (require 'erbc4))
(erball-ignore-errors-loudly (require 'erbc5))
(erball-ignore-errors-loudly (require 'erbc6))
(erball-ignore-errors-loudly (require 'erbcspecial))
(erball-ignore-errors-loudly (require 'erbbdb))
(erball-ignore-errors-loudly (require 'erbforget))
(erball-ignore-errors-loudly (require 'erbedit))
(erball-ignore-errors-loudly (require 'erbtrain))
(erball-ignore-errors-loudly (require 'erbwiki))
(erball-ignore-errors-loudly (require 'erbunlisp))
(erball-ignore-errors-loudly (require 'erbcompat))

(erball-ignore-errors-loudly (require 'erbmsg))

(erball-ignore-errors-loudly (require 'flame))

(erball-ignore-errors-loudly (require 'mkback))
(erball-ignore-errors-loudly (require 'lines))
(erball-ignore-errors-loudly (require 'google))
(erball-ignore-errors-loudly (require 'oct))

;; the rest of the commands here are useful to the author when editing erbot. 

(defcustom erball-files
  (if erball-compiling-p
      (directory-files "." nil "\.el$")
    '("erbot.el"
      "erbutils.el"
      "erblog.el"
      "erbeng.el"
      "erbcountry.el"
      "erbdata.el"
      "erbedit.el"
      "erbforget.el"
      "erbkarma.el"
      "erblisp.el"
      "erbunlisp.el"
      "erbtrain.el"
      "erbwiki.el"
      "erbc.el"
      "erbc2.el"
      "erbc3.el"
      "erbc4.el"
      "erbc5.el"
      "erbc6.el"
      ))

    ""
    :group 'erball
    )

(defun erball-reload ()
  (interactive)
  (mapcar
   'load
   erball-files))

(defun erball-visit ()
  (interactive)
  (mapcar
   (lambda (a)
     (find-file (locate-library a))
     (auto-revert-mode 1))
   erball-files))

;;;###autoload
(defun erball-compile ()
  (interactive)
  (if erball-compiling-p
      (progn
        (ignore-errors (erball-reload))
        (mapcar
         (lambda (arg)
           (erball-ignore-errors-loudly
            (byte-compile-file arg)))
         erball-files)
        (message "\nCompilation complete!\n"))
    (ignore-errors (kill-buffer "*Compile-Log*"))
    (erball-visit)
    (erball-reload)
    (mapcar
     (lambda (arg)
       (erball-ignore-errors-loudly
        (byte-compile-file (locate-library arg))))
     erball-files)
    (switch-to-buffer "*Compile-Log*")
    (delete-other-windows)
    (goto-char (point-min))))


(provide 'erball)
