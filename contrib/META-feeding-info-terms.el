;; this helps prepare an erbtrain file from
;; http://www.emacswiki.org/emacs/info-ref.dat, see also
;; http://www.emacswiki.org/cgi-bin/wiki/EmacsWikiSuggestions
;; or google for emacswiki info ref for pertinent discussions.

;; Author Alex Shroeder <alex@gnu.org>

;; received from kensanata:
(defun meta-feeding-info-k ()
  (let (data (lines 0))
    (with-current-buffer (get-buffer "info-ref.dat")
      (message "Parsing buffer...")
      (goto-char (point-min))
      (while (re-search-forward "^\\(.*\\)\\(.*\\)" nil t)
	(let ((term (match-string 1))
	      (rest (match-string 2)))
	  (setq term (replace-regexp-in-string " " "_" term)
		lines (1+ lines)
		data (cons (cons term
				 (mapcar
				  (lambda (entry)
				    (car (split-string entry "")))
				  (split-string rest "")))
			   data)))))
    (switch-to-buffer (get-buffer-create "info-ref-botsnack"))
    (let ((count 0))
      (dolist (entry data)
	(message "Preparing botsnack...%d%%" (/ (* 100 count) lines))
	(insert (format "%s is at %s" (car entry) (cadr entry)))
	(newline)
	(dolist (url (cddr entry))
	  (insert (format "%s is also at %s" (car entry) url))
	  (newline))))
    (message "Preparing botsnack...done")))

;;; 2006-01-02 T22:04:08-0500 (Monday)    D. Goel
;; minor modifications to the above:
(defun meta-feeding-info-d ()
  (interactive)
  (let (data (lines 0))
    (with-current-buffer (get-buffer "info-ref.dat")
      (message "Parsing buffer...")
      (goto-char (point-min))
      (while (re-search-forward "^\\(.*\\)\\(.*\\)" nil t)
	(let ((term (match-string 1))
	      (rest (match-string 2)))
	  (set-text-properties 0 (length term) nil term)
	  (set-text-properties 0 (length rest) nil rest)
	  (setq term (replace-regexp-in-string " " "_" term)
		lines (1+ lines)
		data (cons (cons term
				 (mapcar
				  (lambda (entry)
				    (car (split-string entry "")))
				  (split-string rest "")))
			   data)))))
    (switch-to-buffer (get-buffer-create "info-ref-botsnack"))
    (let ((count 0) attmp)
      (dolist (entry data)
	(setq attmp (format "at %s" (cadr entry)))
	(message "Preparing botsnack...%d%%" (/ (* 100 count) lines))
	(insert (format "fsbot: (set-term %S %S)" (car entry)  attmp))
	(newline)
	(insert (format "fsbot: (set-also %S %S)" (car entry)  attmp))
	(newline)

	(dolist (url (cddr entry))
	  (insert (format "fsbot: (set-also %S %S)" (car entry) 
			  (format "at %s" url))))
	  (newline)))
    (message "Preparing botsnack...done")))


