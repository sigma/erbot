;;; erbim.el --- input method searching
;; Time-stamp: <2006-08-21 12:14:53 fledermaus>
;; Copyright (C) 2006 V. Dasmohapatra
;; Emacs Lisp Archive entry
;; Filename: erbim.el
;; Package: erbim
;; Author: V. Dasmohapatra <vivek@etla.org>
;; Keywords:  
;; Version:  
;; URL:  http://www.emacswiki.org/cgi-bin/wiki.pl?ErBot
;; For latest version: 

(require 'quail)
(require 'iso-transl)

(defvar erbim-keymaps-map nil)

(defun erbim-enc (thing) 
  (encode-coding-string thing 'utf-8))

(defun erbim-c2s (thing)
  (if (> 256 thing) (single-key-description thing) (char-to-string thing)))

(defun erbim-map (map)
  (let ((char-map nil))
    (mapc (lambda (M) (erbim-map-internal M "")) (cdr map)) char-map))

(defun erbim-interpret-target (target)
  ;;(message "target %S" target)
  (if (vectorp target) 
      (mapcar (lambda (T) (if (integerp T) (erbim-c2s T) T)) target)
    (if (and (listp target) (listp (car target)))
        (progn (message "weird target: %S" target)
          (erbim-interpret-target (cdr target)))
      (if (symbolp target)
          (and (fboundp target) 
               (and (vectorp (symbol-function target)) 
                    (erbim-interpret-target (symbol-function target)) )) 
        (list (if (integerp target) (string target) target)) )) ))

(defun erbim-map-internal (map &optional so-far)
  (let ((iseq-str
         (format (if (symbolp (car map)) "%s %S " "%s%c") (or so-far "")
                 (car map)))
        (tgt  nil)
        (tail nil))
    ;;(message "%S %S" map so-far)
    (setq tgt (cdr map))
    (if (setq tgt (or (car-safe tgt) 
                      (and (vectorp tgt) tgt) 
                      (and (symbolp tgt) tgt)))
        (progn
          ;;(message "tgt: %S" tgt)
          (setq char-map
                (append char-map
                        (mapcar 
                         (lambda (T) (cons (erbim-enc T) iseq-str))
                         (erbim-interpret-target tgt)) ))
          (when (and (listp (cdr map)) (setq tail (cddr map)))
            (if (listp (cdar tail))
                (erbim-map-internal (car tail) iseq-str)
              ;;(message "path B: %S" tail)
              (mapcar (lambda (M) (erbim-map-internal M iseq-str)) tail)) ))
      (when (listp (cdr map))
        (mapcar
         (lambda (M) (erbim-map-internal M iseq-str)) (cddr map))) ) ))

(defun erbim-package-list ()
  (cons "iso-transl"
        (mapcar 
         (lambda (I) 
           (if (eq (caddr I) 'quail-use-package) (car I))) input-method-alist)))

(defun erbim-keymap-map (im)
  (or (cdr (assoc im erbim-keymaps-map))
      (let ( (map (erbim-map 
                   (nth 2 (assoc im quail-package-alist)))) )
        (setq erbim-keymaps-map (cons (cons im map) erbim-keymaps-map)) map) ))

(defun where-is-char (c &optional im-list)
  ;; assume we got a string: char functions are broken in fsbot
  (let ((char (erbim-enc c))
        (res   nil)
        (qsec  nil))
    (mapc (lambda (Q)
            (when (and Q 
                       (not (string-match "^chinese-"    Q))
                       (not (string-match "^devanagari-" Q))
                       (not (member Q '("greek-ibycus4" 
                                        "tibetan-wylie"))))
              (with-temp-buffer
                (or (assoc Q quail-package-alist) 
                    (equal Q "iso-transl")
                    (activate-input-method Q))
                (message "checking %s" Q)
                (when (or (equal Q "iso-transl") (assoc Q quail-package-alist))
                  ;;(message "%s keymap - %d" Q (length (erbim-keymap-map Q)))
                  ;;(message "%S vs %S" 
                  ;;         char
                  ;;         (car 
                  ;;         (rassoc "^{TM}" 
                  ;;                  (erbim-keymap-map Q)) ))
                  (when (setq qsec (assoc char (erbim-keymap-map Q)))
                    ;;(message "found sequence %s" qsec)
                  (setq res (cons (cons Q (cdr qsec)) res))) )) ))
          (or im-list (erbim-package-list)))
    ;; feed the results to the user:
    (mapconcat
     (lambda (R) 
       (if (equal (car R) "iso-transl")
	   (mapconcat 'identity 
		      (cons "C-x 8" (split-string (cdr R) "")) " ")
	 (format "%s: %s" (car R) (cdr R)) )) res "\n") ))

(defun fsi-where-is-char (&optional key &rest im-list)
  (let ((imlist nil) 
	(key (if key (if (symbolp key) (symbol-name key) key) nil)))
    (if key (where-is-char key (mapcar 'symbol-name im-list)) 
      "No character specified.") ))

(defun fsi-erbim-test () "erbim loaded Ok")
;; (where-is-char "Þ")

;; (assoc (erbim-enc "þ") (erbim-keymap-map "iso-transl"))

(add-to-list 'erbim-keymaps-map 
             (cons "iso-transl" (erbim-map iso-transl-ctl-x-8-map)))

(provide 'erbim)
;; (setq erbim-keymaps-map nil)
;; (insert "\n" (pp (assoc "japanese" erbim-keymaps-map)))
;; (insert "\n" (pp (nth 2 (assoc "greek-babel" quail-package-alist))))
;; (insert "\n" (pp (assoc "greek-babel" erbim-keymaps-map)))

;; (erbim-map iso-transl-ctl-x-8-map)
;; (insert "\n" 
;;   (pp (erbim-map (nth 2 (assoc "greek-babel" quail-package-alist)))))
;; (erbim-map (nth 2 (assoc "greek-babel" quail-package-alist)))
