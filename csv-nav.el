;;; csv-nav.el --- navigate and edit CSV files

;; Copyright (C) 2006  Alex Schroeder <alex@gnu.org>

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Commentary:

;; Use `csv-nav-mode' to edit CSV files such as contacts exported from
;; other applications.

;;; Code:

;;;###autoload
(define-generic-mode csv-nav-mode
  nil '(",") nil '(".csv\\'")
  '((lambda ()
      (local-set-key (kbd "RET") 'csv-nav-show)))
  "Major mode for viewing CSV files.")

(defvar csv-nav-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\"  "\"" table)
    (modify-syntax-entry ?,  "." table)
    table))

(define-generic-mode csv-show-mode
  nil nil nil nil '(csv-show-setup)
  "Major mode for viewing CSV file records.")

(setq csv-show-map 
      (let ((map (make-sparse-keymap)))
	(define-key map "n" (lambda () (interactive) (csv-show-next/prev 1)))
	(define-key map "p" (lambda () (interactive) (csv-show-next/prev -1)))
	map))

(defun csv-show-setup ()
  "Main code to setup the csv-show major mode."
  (setq font-lock-defaults nil) 
  (use-local-map csv-show-map)
  (make-local-variable 'csv-nav-source-marker)
  (make-local-variable 'csv-nav-source-line-no))

(defun csv-nav-parse-field (start)
  "Return field starting at START and ending at point."
  (let ((field (buffer-substring start (point))))
    ;; remove double quotes, fix newlines
    (when (and (> (point) start); no quotes in zero length fields 
	       (= (aref field 0) ?\")
	       (= (char-before) ?\"))
      (setq field
	    (replace-regexp-in-string
	     "\r" "" (replace-regexp-in-string
		      "\"\"" "\"" (substring field 1 -1)))))
    ;; Remove leading spaces from field
    (setq field (replace-regexp-in-string "^ " "" field))
    ;; Remove trailing spaces from field
    (setq field (replace-regexp-in-string " $" "" field))
    field))

(defun csv-nav-parse-line ()
  "Parse the current line and return the list of values."
  (let ((start (point))
	result)
    (with-syntax-table csv-nav-syntax-table
      (while start
	(skip-syntax-forward "^.\" ")
	(cond ((eq (char-after) ?,)
	       (setq result (cons (csv-nav-parse-field start) result)
		     start (1+ (point)))
	       (forward-char 1))
	      ((eq (char-after) ?\n)
	       (setq result (cons (csv-nav-parse-field start)
				  result)
		     start nil)
	       (forward-char 1))
	      ((eq (char-after) ?\")
	       (forward-sexp 1))
	      (t
	       (forward-char 1))))	; break
      (nreverse result))))

(defun csv-nav-get-columns ()
  "Get the field names of the buffer."
  (save-excursion
    (goto-char (point-min))
    (csv-nav-parse-line)))

(defun csv-nav-show ()
  "Show the current row."
  (interactive)
  (let ((columns (csv-nav-get-columns))
	(current-buffer-v (current-buffer))
	line-no start
	cells)
    (save-excursion
      (beginning-of-line)
      (setq line-no (line-number-at-pos (point))
	    start (point-marker)
	    cells (csv-nav-parse-line)))
    (when (< (length columns)
	     (length cells))
      (error "Not enough columns for all the cells"))
    (pop-to-buffer (get-buffer-create "*CSV Detail*"))
    (setq csv-nav-source-marker start
	  csv-nav-source-line-no line-no)
    (csv-show-fill-buffer columns cells)
    (pop-to-buffer current-buffer-v)))

(defun csv-show-fill-buffer (columns cells)
  "Fills the buffer with the content of the cells."
    (setq buffer-read-only nil)
    (erase-buffer)

    (insert "FILE: " (buffer-name (marker-buffer csv-nav-source-marker))
	    " LINE: " (format "%d" csv-nav-source-line-no) "\n\n")
    
    (let ((width (reduce 'max columns :key 'length)))
      (while columns
	(when (> (length (car cells)) 0)
	  (insert (propertize (concat  (car columns) ":")
			      'field 'column
			      'face 'font-lock-keyword-face
			      'rear-nonsticky t))
	  (move-to-column (+ 4 width) t)
	  (insert (car cells) "\n"))
	(setq columns (cdr columns)
	      cells (cdr cells)))
      (setq buffer-read-only t))
    (goto-char (point-min)))

(defun csv-show-next/prev (&optional dir)
  "Shows the next or previous record."
  (interactive "p")
  (let ((old-marker csv-nav-source-marker)
	new-marker line-no 
	cells columns)
    (save-excursion
      (set-buffer (marker-buffer old-marker))
      (goto-char old-marker)
      (forward-line (or dir 1))
      (setq line-no (line-number-at-pos (point))
	    new-marker (point-marker)
	    cells (csv-nav-parse-line)
	    columns (csv-nav-get-columns)))

    (setq csv-nav-source-marker new-marker
	  csv-nav-source-line-no line-no)
    (csv-show-fill-buffer columns cells)))

(provide 'csv-nav)
;;; csv-nav.el ends here
