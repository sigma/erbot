;;; erball.el --- Functions on all files. 
;; Time-stamp: <2003-05-23 08:43:45 deego>
;; Copyright (C) 2002 D. Goel
;; Emacs Lisp Archive entry
;; Filename: erbc.el
;; Package: erbc
;; Author: D. Goel <deego@gnufans.org>
;; Version: 
;; Author's homepage: http://deego.gnufans.org/~deego
;; For latest version:
;; Usually maintenance
;; not all of these may be required depending on how you use erbot..
(require 'doctor)
(require 'erc)
(require 'erc-stamp)
(require 'dunnet)
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
(require 'erbbdb)
(require 'erbforget)
(require 'erbtrain)
(require 'erbwiki)
(require 'erbunlisp)

(require 'flame)

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
    "erbforget.el"
    "erbkarma.el"
    "erblisp.el"
    "erbunlisp.el"
    "erbtrain.el"
    "erbwiki.el"
    "erbc.el"
    "erbc2.el"
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
     (find-file a)
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


