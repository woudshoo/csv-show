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

(defun csv-nav-parse-buffer ()
  "Parse the current line and return the list of values."
  (goto-char (point-min))
  (let (data (max (point-max)))
    (while (< (point) max)
      (setq data (cons (csv-nav-parse-line) data)))
    (nreverse data)))

(defun csv-nav-get-columns ()
  "Get the field names of the buffer."
  (save-excursion
    (goto-char (point-min))
    (csv-nav-parse-line)))

(defun csv-nav-show ()
  "Show the current row."
  (interactive)
  (let ((columns (csv-nav-get-columns))
	start end cells source)
    (save-excursion
      (beginning-of-line)
      (setq start (point-marker)
	    cells (csv-nav-parse-line)
	    end (point-marker)
	    source (list (current-buffer) start end)))
    (when (< (length columns)
	     (length cells))
      (error "Not enough columns for all the cells"))
    (pop-to-buffer (get-buffer-create
		    (car (delete "" (copy-sequence cells)))))
    (erase-buffer)
    ;; (text-mode)
    (set (make-local-variable 'csv-nav-source) source)
    (while columns
      (when (> (length (car cells)) 0)
	(insert (propertize (concat (car columns) ": ")
			    'field 'column
			    'face 'bold
			    'rear-nonsticky t)
		(car cells) "\n"))
      (setq columns (cdr columns)
	    cells (cdr cells)))
    (set 'buffer-read-only 0)
    (goto-char (point-min))))

(provide 'csv-nav)
;;; csv-nav.el ends here
