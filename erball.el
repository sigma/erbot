;;; erball.el --- Functions on all files. 
;; Time-stamp: <2004-12-31 23:06:48 deego>
;; Copyright (C) 2002 D. Goel
;; Emacs Lisp Archive entry
;; Filename: erbc.el
;; Package: erbc
;; Author: D. Goel <deego@gnufans.org>
;; Version: 



;; Usually maintenance
;; not all of these may be required depending on how you use erbot..
(require 'cl)

(require 'doctor)
(require 'erc)
(require 'erc-stamp)
(unless noninteractive (require 'dunnet))
(require 'erbot)
(require 'erbcountry)
(require 'erbutils)
(require 'erblog)
(require 'erbeng)
(require 'erbdata)
(require 'erbkarma)
(require 'erblisp)
(require 'erbc)
(require 'erbc2)
(require 'erbc3)
(require 'erbc4)
(require 'erbc5)
(require 'erbc6)
(require 'erbcspecial)
(require 'erbbdb)
(require 'erbforget)
(require 'erbedit)
(require 'erbtrain)
(ignore-errors (require 'erbwiki))
(require 'erbunlisp)
(require 'erbcompat)


(ignore-errors 
  (require 'erbmsg))

(ignore-errors (require 'flame))

(ignore-errors (require 'mkback))
(ignore-errors (require 'lines))
(ignore-errors (require 'google))
(ignore-errors (require 'oct))

;; the rest of the commands here are useful to the author when editing erbot. 

(defcustom erball-files 
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
    )

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
  (ignore-errors (kill-buffer "*Compile-Log*"))
  (erball-visit)
  (erball-reload)
  (mapcar
   (lambda (arg)
     (byte-compile-file (locate-library arg)))
   erball-files)
  (switch-to-buffer "*Compile-Log*")
  (delete-other-windows)
  (goto-char (point-min)))












(provide 'erball)


