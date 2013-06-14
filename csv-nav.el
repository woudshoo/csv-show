;;; csv-nav.el --- navigate and edit CSV files

;; Copyright (C) 2006  Alex Schroeder <alex@gnu.org>
;; Copyright (C) 2013  Tom Koelman
;; Copyright (C) 2013  Willem Rein Oudshoorn <woudshoo@xs4all.nl>
;;
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

;; Use the `csv-select-show-mode' minor mode in a CSV file to activate
;; the csv-show feature.
;;
;; When this minor mode is enabled C-return will open up a new buffer
;; showing the content of the current CSV row in a table format.
;;
;; In this CSV-Show buffer the keys `n' and 'p' will select the next
;; or previous row to display.

;;; Code:

(require 'cl)

(setq csv-show-select-map
      (let ((map (make-sparse-keymap)))
	(define-key map [?\C-.] 'csv-show-toggle-timer)
	(define-key map [C-return] 'csv-show-select)
	map))

;;;###autoload
(define-minor-mode csv-select-show-mode 
  "Shows a row in a CSV file in a separate buffer."
  nil " CSV-SHOW" csv-show-select-map)

(setq csv-show-map 
      (let ((map (make-sparse-keymap)))
	(define-key map "n" (lambda () (interactive) (csv-show-next/prev 1)))
	(define-key map "N" (lambda () (interactive) (csv-show-next/prev-statistictime 1)))
	(define-key map "." (lambda () (interactive) (csv-show-current)))
	(define-key map [?\C-.] 'csv-show-toggle-timer)
	(define-key map "p" (lambda () (interactive) (csv-show-next/prev -1)))
	(define-key map "P" (lambda () (interactive) (csv-show-next/prev-statistictime -1)))
	map))

(define-generic-mode csv-show-mode
  nil nil nil nil '(csv-show-setup)
  "Major mode for viewing CSV file records.

This mode is enabled for buffers that are created by the
`csv-show-select' function.  It should not be toggled by the user.")

(defun csv-show-setup ()
  "Main code to setup the csv-show major mode.
This mode should not be selected by the user, but by 
the `csv-show-select' function."
  (setq font-lock-defaults nil) 
  (use-local-map csv-show-map)
  (make-local-variable 'csv-show-source-marker)
  (make-local-variable 'csv-show-source-line-no)
  (make-local-variable 'csv-show-columns)
  (make-local-variable 'csv-show-cells))


(defvar csv-show-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\"  "\"" table)
    (modify-syntax-entry ?,  "." table)
    table))

(defun csv-show-parse-field (start)
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

(defun csv-show-parse-line ()
  "Parse the current line and return the list of values."
  (let ((start (point))
	result)
    (with-syntax-table csv-show-syntax-table
      (while start
	(skip-syntax-forward "^.\" ")
	(cond ((eq (char-after) ?,)
	       (setq result (cons (csv-show-parse-field start) result)
		     start (1+ (point)))
	       (forward-char 1))
	      ((eq (char-after) ?\n)
	       (setq result (cons (csv-show-parse-field start)
				  result)
		     start nil)
	       (forward-char 1))
	      ((eq (char-after) ?\")
	       (forward-sexp 1))
	      (t
	       (forward-char 1))))	; break
      (nreverse result))))

(defun csv-show--get-columns ()
  "Get the field names of the buffer."
  (save-excursion
    (goto-char (point-min))
    (csv-show-parse-line)))

(defun csv-show--get-cells ()
  (save-excursion
    (csv-show-parse-line)))

(defun csv-show-select ()
  "Show the current row."
  (interactive)
  (let ((current-buffer-v (current-buffer))
	(start (point-marker)))
    (pop-to-buffer (get-buffer-create "*CSV Detail*"))
    (csv-show-mode)
    (setq csv-show-source-marker start)
    (csv-show-current)
    (pop-to-buffer current-buffer-v)))


(defvar csv-show-update-timer nil
  "Holds the timer used to keep the *CSV Detail* buffer in sync
with the underlying CSV buffer.

If nil the timer is not active.")

(defun csv-show-toggle-timer ()
  "When enabled, the *CSV Detail* buffer tracks the cursor in the
underlying CSV buffer.  This function turns toggles this
functionality."
  (interactive)
  (if csv-show-update-timer 
      (progn
	(cancel-timer csv-show-update-timer)
	(setq csv-show-update-timer nil))
    (setq csv-show-update-timer 
	  (run-with-idle-timer 0.1 t 'csv-show-update-detail-buffer))))

(defun csv-show-update-detail-buffer ()
  "Updates the *CSV Detail* buffer with the content of the line
containing point in the underlying CSV buffer.  It is similar to the 
`csv-show-select', except that it does not create a *CSV Detail* buffer
if it exists."
  (interactive)
  (let ((detail-buffer (get-buffer "*CSV Detail*")))
    (when detail-buffer
      (save-match-data
	(with-current-buffer detail-buffer
	  (csv-show-current))))))

(defun csv-show-fill-buffer ()
  "Fills the buffer with the content of the cells."
    (setq buffer-read-only nil)
    (erase-buffer)

    (insert "FILE: " (buffer-name (marker-buffer csv-show-source-marker))
	    " LINE: " (format "%d" csv-show-source-line-no) "\n\n")
    
    (let ((width (reduce 'max csv-show-columns :key 'length))
	  (columns csv-show-columns)
	  (cells csv-show-cells))
      (while (and columns cells)
	(when (> (length (car cells)) 0)
	  (insert (propertize (concat  (car columns) ":")
			      'field 'column
			      'face 'font-lock-keyword-face
			      'rear-nonsticky t))
	  (move-to-column (+ 4 width) t)
	  (insert (car cells) "\n"))
	(pop columns)
	(pop cells))
      (setq buffer-read-only t))
    (goto-char (point-min)))

(defun csv-show--mark-forward/backward (dir &optional do-not-parse-headers)
  "Move the selection to the next or previous record.
Note that this does not updat the content of the buffer,
it will parse the column, cells and put these into the
corresponding local variables of the CSV-Detail buffer.

Also move the mark down or up and update the line-no
variable.

For updatint the content see the function `csv-show-fill-buffer'."
  (let ((old-marker csv-show-source-marker)
	new-marker line-no cells columns)
    (with-current-buffer (marker-buffer old-marker)
      (save-excursion
	(goto-char old-marker)
	(forward-line (or dir 1))
	(beginning-of-line)
	(setq new-marker (point-marker)
	      line-no (line-number-at-pos (point))
	      columns (unless do-not-parse-headers (csv-show--get-columns))
	      cells (csv-show--get-cells))))
    (setq csv-show-source-marker new-marker
	  csv-show-source-line-no line-no
	  csv-show-cells cells)
    (unless do-not-parse-headers
      (setq csv-show-columns columns))))

(defun csv-show-current ()
  "Update the content of the *CSV-Detail* buffer with the content
of the current line.  
This function requires that the current buffer is a *CSV-Detail* buffer."
  (interactive)
  (setq csv-show-source-marker 
	(with-current-buffer (marker-buffer csv-show-source-marker)
	  (save-excursion
	    (beginning-of-line)
	    (point-marker))))
  (csv-show--mark-forward/backward 0)
  (csv-show-fill-buffer))

(defun csv-show-next/prev (&optional dir)
  "Shows the next or previous record."
  (interactive "p")
  (csv-show--mark-forward/backward dir t)
  (csv-show-fill-buffer))

(defun csv-get-current-value-for-field (field)
  "Returns the value of the given field for the current record"
  (let ((columns csv-show-columns)
	(cells csv-show-cells))
    (while (and columns cells
		(not (equal (car columns) field)))
      (pop columns)
      (pop cells))
    (and columns cells (car cells))))

(defun csv-get-current-statistictime ()
  "Returns the StatisticTime value of the current record"
  (csv-get-current-value-for-field "StatisticTime"))

(defun csv-get-current-instanceid ()
  "Returns the InstanceID value of the current record"
  (csv-get-current-value-for-field "InstanceID"))
  
; csv-show-next/prev-statistictime needs a check on the beginning and the end of the
; csv buffer
(defun csv-show-next/prev-statistictime (&optional dir)
  "Shows the next or previous record for which the StatisticTime
field is different than the current, and InstanceID is
identical."
  (interactive)
  (let ((current-statistictime (csv-get-current-statistictime))
        (current-instanceid (csv-get-current-instanceid)))
    (while (or (equal current-statistictime (csv-get-current-statistictime))
               (not (equal current-instanceid (csv-get-current-instanceid))))
      (csv-show--mark-forward/backward dir t))
    (csv-show-fill-buffer)))


(provide 'csv-show)
;;; csv-nav.el ends here
