; h4x0r.el 0.11
; Time-stamp: <2003-02-22 00:47:54 deego>

; by Charles Sebold <csebold@livingtorah.org>
;
; thanks to Alex Schroeder for typo fix and feature suggestions (which
; I have not begun to implement yet)

;;; Copyright: (C) 2000, 2001 Charles Sebold
;; 
;;     This program is free software; you can redistribute it and/or
;;     modify it under the terms of the GNU General Public License as
;;     published by the Free Software Foundation; either version 3 of
;;     the License, or (at your option) any later version.
;;     
;;     This program is distributed in the hope that it will be useful,
;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;     GNU General Public License for more details.
;;     
;;     You should have received a copy of the GNU General Public
;;     License along with GNU Emacs; if not, write to the Free
;;     Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;;     MA 02111-1307, USA.
;;
;; Latest version should be available at:
;;    <URL:http://www.livingtorah.org/~csebold/emacs/h4x0r.el>
;;


(require 'cl)

(defvar h4x0r-always-replace
      '(("hacker" . "h4x0r") ("hack" . "h4x0r") ("elite" . "31337")
        ("fear" . "ph33r")))

(defvar h4x0r-sometimes-replace
      '(("ea" "33") ("er" "0r") ("a" "4") ("b" "8") ("d" "|>")
        ("e" "3" "E") ("f" "|=") ("h" "|-|") ("i" "1" "|") ("k" "|<" "x")
        ("l" "1" "|_") ("m" "|\\/|") ("n" "|\\|") ("o" "0") ("q" "@") ("s"
        "5" "Z" "$") ("t" "+" "7") ("ck" "x") ("u" "U") ("v" "\\/") ("x"
        "X" "><") ("y" "j")))

(defvar h4x0r-unreadable 5)

(defvar h4x0r-replace-with-symbols-p nil)

(defun h4x0r-region (beg end)
  "Convert region to h4x0r-talk."
  (interactive "r")
  (save-excursion
    (let ((starting-buffer (current-buffer)))
      (set-buffer (get-buffer-create "h4x0r-temp"))
      (insert-buffer-substring starting-buffer beg end)
      (downcase-region (point-min) (point-max))
      (dotimes (i (length h4x0r-always-replace))
        (beginning-of-buffer)
        (let ((old-word (car (nth i h4x0r-always-replace)))
              (new-word (cdr (nth i h4x0r-always-replace))))
          (while (search-forward old-word nil t)
            (replace-match new-word))))
      (dotimes (i (length h4x0r-sometimes-replace))
        (if (< (random 9) h4x0r-unreadable)
            (progn
              (beginning-of-buffer)
              (let ((old-char (car (nth i h4x0r-sometimes-replace))))
                (let ((new-char (h4x0r-assoc old-char)))
                  (while (search-forward old-char nil t)
                    (replace-match new-char nil t)))))))
      (set-buffer starting-buffer)
      (delete-region beg end)))
  (insert-buffer "h4x0r-temp")
  (message "%s" "J00 h4v3 b33n h4x0r3d!")
  (kill-buffer "h4x0r-temp"))

(defun h4x0r-assoc (normal-char)
  (let ((h4-out (cdr (assoc normal-char h4x0r-sometimes-replace))))
    (if (nlistp h4-out)
        h4-out
      (nth (random (length h4-out)) h4-out))))

(defun h4x0r-buffer ()
  "Convert entire buffer to h4x0r-talk."
  (interactive)
  (save-excursion
    (h4x0r-region (point-max) (point-min))))

(defun h4x0r-word-at-point ()
  (interactive)
  (save-excursion
    (forward-word -1)
    (insert (h4x0r-string (current-word)))
    (kill-word 1)))

(defun h4x0r-string (h4-input-string)
  (save-excursion
    (let ((starting-buffer (current-buffer)))
      (set-buffer (get-buffer-create "h4x0r-string-temp"))
      (insert h4-input-string)
      (h4x0r-buffer)
      (setq h4-input-string (buffer-string))
      (kill-buffer "h4x0r-string-temp")
      (set-buffer starting-buffer)))
  h4-input-string)

(provide 'h4x0r)
