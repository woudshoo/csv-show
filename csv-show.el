;;; csv-show.el --- navigate and edit CSV files

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

;; Use the `csv-show-mode' minor mode in a CSV file to activate
;; the csv-show feature.
;;
;; When this minor mode is enabled C-return will open up a new buffer
;; showing the content of the current CSV row in a table format.
;;
;; In this CSV-Show buffer the keys `n' and 'p' will select the next
;; or previous row to display.

;;; Code:

(require 'cl)

;;
;;  (in-other-buffer marker ((var-a  expr-a) (var-b expr-b)) ...)
;;
;;  (let (tmp-a tmp-b tmp-c)
;;    (with-current-buffer (marker-buffer marker)
;;       (save-excursion
;;         ...body...
;;         (setq tmp-a expr-a
;;               tmp-b expr-b)))
;;     (setq var-a tmp-a
;;           var-b tmp-b))
;;
;;
;;  --
;;  body = (1 2 3)
;;
;;  `(a ,body) ==> (a (1 2 3))
;;  `(a ,@body) ==> (a 1 2 3)
;;  `(ksadjflasj ,sdlfkj
;;

(defun parallel-mapcar (function list-a list-b)
  (let (result)
    (while (and list-a list-b)
      (push (funcall function (pop list-a) (pop list-b)) result))
    result))

(defmacro in-other-buffer (marker bindings &rest body)
  (let* ((old-mark (make-symbol "OLD-MARKER"))
	 (tmps (mapcar (lambda (v) (make-symbol "TMP")) bindings))
	 ;; tmps = (tmp-a tmp-b tmp-c ..)
	 (set-tmps (parallel-mapcar (lambda (v tmp) (list tmp (cadr v))) bindings tmps))
	 ;; set-tmps = ((tmp-a expr-a) (tmp-b expr-b) ...)
	 (set-vars (parallel-mapcar (lambda (v tmp) (list (car v) tmp)) bindings tmps)))
    `(let ,tmps
       (let ((,old-mark ,marker))
	 (with-current-buffer (marker-buffer ,marker)
	   (save-excursion
	     (goto-char ,old-mark)
	     ,@body
	     ,@(mapcar (lambda (tmp-form) `(setq ,@tmp-form)) set-tmps))))
       ,@(mapcar (lambda (var-form) `(setq ,@var-form)) set-vars))))


(setq csv-show-map
      (let ((map (make-sparse-keymap)))
	(define-key map [?\C-.] 'csv-show-toggle-timer)
	(define-key map [C-return] 'csv-show-select)
	map))

;;;###autoload
(define-minor-mode csv-show-mode 
  "Shows a row in a CSV file in a separate buffer."
  nil " csv-show" csv-show-map)

(setq csv-show-detail-map 
      (let ((map (make-sparse-keymap)))
	(define-key map "n" (lambda () (interactive) (csv-show-next/prev 1)))
	(define-key map "N" (lambda () (interactive) (csv-show-next/prev-statistictime 1)))
	(define-key map "." (lambda () (interactive) (csv-show-current)))
	(define-key map [?\C-.] 'csv-show-toggle-timer)
	(define-key map "p" (lambda () (interactive) (csv-show-next/prev -1)))
	(define-key map "P" (lambda () (interactive) (csv-show-next/prev-statistictime -1)))
	(define-key map "h" 'csv-show-hide-column)
	(define-key map "s" 'csv-show-column-state-toggle)
	map))

(define-generic-mode csv-show-detail-mode
  nil nil nil nil '(csv-show--detail-setup)
  "Major mode for viewing CSV file records.

This mode is enabled for buffers that are created by the
`csv-show-select' function.  It should not be toggled by the user.")

(defun csv-show--detail-setup ()
  "Main code to setup the csv-show major mode.
This mode should not be selected by the user, but by 
the `csv-show-select' function."
  (setq font-lock-defaults nil) 
  (use-local-map csv-show-detail-map)
  (make-local-variable 'csv-show-source-marker)
  (make-local-variable 'csv-show-source-line-no)
  (make-local-variable 'csv-show-columns)
  (make-local-variable 'csv-show-cells)
  (make-local-variable 'csv-show-column-state)
  (make-local-variable 'csv-show-column-state-toggle)
  (setq csv-show-column-state (list))
  (setq csv-show-column-state-toggle nil))


(defvar csv-show-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\"  "\"" table)
    (modify-syntax-entry ?,  "." table)
    table))

(defun csv-show-parse-field (start)
  "Return field starting at START and ending at point."
  (let ((field (buffer-substring-no-properties start (point))))
    ;; remove double quotes, fix newlines
    (when (and (> (point) start); no quotes in zero length fields 
	       (= (aref field 0) ?\")
	       (= (char-before) ?\"))
      (setq field
	    (replace-regexp-in-string
	     "\r" "" (replace-regexp-in-string
		      "\"\"" "\"" (substring field 1 -1)))))
    ;; Remove leading spaces from field
    ;(setq field (replace-regexp-in-string "^ " "" field))
    ;; Remove trailing spaces from field
    ;(setq field (replace-regexp-in-string " $" "" field))
    field))

(defun csv-show--field-index-for-column (column)
  "Returns the index of COLUMN."
  (position column (csv-show--get-columns) :test #'equal))
    
(require 'ert)
(ert-deftest csv-show--field-index-for-column-test ()
  (let ((csv-show--get-columns-cache (list "Header1" "Header2" "Header3")))
    (assert (equal (csv-show--field-index-for-column "Header2") 1))))

(defun csv-show-parse-line (&optional indices)
  "Parse the current line and return the list of values. When 
   INDICES is specified, returns a list with values on those INDICES."
  (let ((start (point))
        (index -1)
        (all-indices (not indices))
        current-value
	result)
    (with-syntax-table csv-show-syntax-table
      (while (and start
                  (or all-indices
                      indices))
        (setq current-value nil)
	(skip-syntax-forward "^.\" ")
	(cond ((eq (char-after) ?,)
               (setq current-value (csv-show-parse-field start)
                     index (1+ index)
                     start (1+ (point)))
	       (forward-char 1))
              ((eq (char-after) ?\n)
               (setq current-value (csv-show-parse-field start)
                     index (1+ index)
                     start nil)
	       (forward-char 1))
	      ((eq (char-after) ?\")
	       (forward-sexp 1))
	      (t
	       (forward-char 1)))
        (when (and current-value
                   (or all-indices
                       (and indices
                            (equal index (car indices)))))
          (push current-value result)
          (pop indices)
          )
        )) ;break
      (nreverse result)))

(defvar csv-show--get-columns-cache nil)
(defun csv-show--get-columns ()
  "Get the field names of the buffer."
  (or csv-show--get-columns-cache
      (save-excursion
        (goto-char (point-min))
        (csv-show-parse-line))))

(defun csv-show--get-cells ()
  (save-excursion
    (csv-show-parse-line)))

(defun csv-show-select ()
  "Show the current row."
  (interactive)
  (let ((current-buffer-v (current-buffer))
	(start (point-marker)))
    (pop-to-buffer (get-buffer-create "*CSV Detail*"))
    (csv-show-detail-mode)
    (setq csv-show-source-marker start)
    (csv-show-current)
    (pop-to-buffer current-buffer-v)))

(defvar csv-show-update-timer nil
  "Holds the timer used to keep the *CSV Detail* buffer in sync
with the underlying CSV buffer.

If nil the timer is not active.")

(defun csv-show-toggle-timer ()
  "When enabled, the *CSV Detail* buffer tracks the cursor in the
underlying CSV buffer.  This function toggles this
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
	  (csv-show-current t))))))

(defun csv-show--statistictime-to-string ( statistictime )
  "Returns a nicely formatted STATISTICTIME."
  (interactive)
  (let ( year month day hour minute second offset )
    (setq year (substring statistictime 0 4)
          month (substring statistictime 4 6)
          day (substring statistictime 6 8)
          hour (substring statistictime 8 10)
          minute (substring statistictime 10 12)
          second (substring statistictime 12 14)
          offset (number-to-string (/ (string-to-number (substring statistictime -4)) 60)))
          (concat year "-" month "-" day " " hour ":" minute ":" second " (" offset ")*" )))

(defun csv-show--usagerestriction-to-string ( usagerestriction )
  "Returns a nicely formatted USAGERESTRICTION."
  (interactive)
  (cond ((equal usagerestriction "0")
         "Unknown*")
        ((equal usagerestriction "2")
         "Front-end only*")
        ((equal usagerestriction "3")
         "Back-end only*")
        ((equal usagerestriction "4")
         "Not restricted*")
        (t usagerestriction)))

(defun csv-show--format-huge-number( hugenumber )
  "Returns a nicely formatted HUGENUMBER."
  (interactive)
  (let (groups)
    (while (> (length hugenumber) 0)
      (if (>= (length hugenumber) 3)
          (progn
            (push (substring hugenumber -3) groups)
            (setq hugenumber (substring hugenumber 0 (- (length hugenumber) 3)))
            )
        (progn
         (push hugenumber groups)
         (setq hugenumber "")
         )))
    (concat (mapconcat 'identity groups " ") "*")
  ))

(defvar csv-show-column-format-functions
  `(("StatisticTime" . csv-show--statistictime-to-string)
    ("IM_OriginalStatisticTime" . csv-show--statistictime-to-string)
    ("UsageRestriction" . csv-show--usagerestriction-to-string)
    ("Consumed" . csv-show--format-huge-number)
    ("ConsumableBlocks" . csv-show--format-huge-number)
    ("NumberOfBlocks" . csv-show--format-huge-number)
    ("MaxSpeed" . csv-show--format-huge-number)
    ("Speed" . csv-show--format-huge-number)
    )
)

(defun csv-show--format-function-for-column (column)
  "Return the format function for COLUMN."
  (or
   (cdr (assoc column csv-show-column-format-functions ))
   #'identity))

(defun csv-show-set-column-state (column state)
  (push (cons column state) csv-show-column-state))

(defun csv-show-column-name (&optional point)
  (save-excursion
    (when point (goto-char point))
    (beginning-of-line)
    (buffer-substring-no-properties (point)
				    (1- (search-forward ":")))))

(defun csv-show-hide-column ()
  (interactive)
  (let ((column (csv-show-column-name)))
    (case (csv-show-column-state column)
      ('hidden (csv-show-set-column-state column 'normal))
      (t (csv-show-set-column-state column 'hidden))))
  (csv-show-fill-buffer))

(defun csv-show-column-state-toggle ()
  (interactive)
  (setq csv-show-column-state-toggle (not csv-show-column-state-toggle))
  (csv-show-fill-buffer))

(defun csv-show-column-state (column)
  (cdr (assoc column csv-show-column-state)))

(defun csv-show-fill-buffer ()
  "Fills the buffer with the content of the cells."
    (setq buffer-read-only nil)
    (let ((line-no (line-number-at-pos))
	  (column-no (current-column)))
      (erase-buffer)

      (insert "FILE: " (buffer-name (marker-buffer csv-show-source-marker))
	      " LINE: " (format "%d" csv-show-source-line-no) "\n\n")
      
      (let ((width (reduce 'max csv-show-columns :key 'length))
	    (columns csv-show-columns)
	    (cells csv-show-cells))
	(while (and columns cells)
	  (let ((start (point))
		(column (pop columns))
		(cell (pop cells)))
	    (insert (propertize (concat  column ":")
				'face 'font-lock-keyword-face
				'rear-nonsticky t))
	    (move-to-column (+ 4 width) t)
	    (insert (funcall (csv-show--format-function-for-column column) cell) "\n")
	    (case (csv-show-column-state column)
	      ('hidden (put-text-property start (point) 'face 'highlight)
		       (unless csv-show-column-state-toggle
			 (put-text-property start (point) 'invisible t))))))
	(setq buffer-read-only t))
      (goto-char (point-min))
      (forward-line (1- line-no))
      (move-to-column column-no)))

(defun csv-show--mark-forward/backward (dir &optional do-not-parse-headers)
  "Move the selection to the next or previous record.
Note that this does not update the content of the buffer,
it will parse the column, cells and put these into the
corresponding local variables of the CSV-Detail buffer.

Also move the mark down or up and update the line-no
variable.

For updating the content see the function `csv-show-fill-buffer'."
  (let (new-show-columns)
    (in-other-buffer csv-show-source-marker 
		     ((csv-show-source-marker (point-marker))
		      (csv-show-source-line-no (line-number-at-pos (point)))
		      (csv-show-cells (csv-show--get-cells))
		      (new-show-columns (unless do-not-parse-headers
					  (csv-show--get-columns))))
		     
		     (forward-line (or dir 1))
		     (beginning-of-line))
    (unless do-not-parse-headers
      (setq csv-show-columns new-show-columns))))

(defun csv-show-current (&optional do-not-parse-headers)
  "Update the content of the *CSV-Detail* buffer with the content
of the current line.  
This function requires that the current buffer is a *CSV-Detail* buffer."
  (interactive)
  (setq csv-show-source-marker 
	(with-current-buffer (marker-buffer csv-show-source-marker)
	  (save-excursion
	    (beginning-of-line)
	    (point-marker))))
  (csv-show--mark-forward/backward 0 do-not-parse-headers) 
  (csv-show-fill-buffer))

(defun csv-show-next/prev (&optional dir)
  "Shows the next or previous record."
  (interactive "p")
  (csv-show--mark-forward/backward dir t)
  (csv-show-fill-buffer))

(defun csv-show--get-current-value-for-index (index)
  "Returns the value of the INDEXth item on the current line. Returns nil when index not given."
  (when index
    (let* ((values (csv-show-parse-line (list index))))
      (car values))))

; csv-show-next/prev-statistictime needs a check on the beginning and the end of the
; csv buffer
(defun csv-show-next/prev-statistictime (&optional dir)
  "Shows the next or previous record for which the StatisticTime
field is different than the current, and InstanceID is
identical."
  (interactive)
  (in-other-buffer csv-show-source-marker 
		     ((csv-show-source-marker (point-marker))
		      (csv-show-source-line-no (line-number-at-pos (point)))
		      (csv-show-cells (csv-show--get-cells)))
                     (let* ( (csv-show--get-columns-cache (csv-show--get-columns) ) 
                             (statistictime-index (csv-show--field-index-for-column "StatisticTime"))
                             (instanceid-index (csv-show--field-index-for-column "InstanceID"))
                             current-statistictime (csv-show--get-current-value-for-index statistictime-index)
                             current-instanceid (csv-show--get-current-value-for-index instanceid-index))
                       (while (or
                               (not (equal current-instanceid (csv-show--get-current-value-for-index instanceid-index)))
                               (equal current-statistictime (csv-show--get-current-value-for-index statistictime-index))
                               )
                         (forward-line (or dir 1))
                         (beginning-of-line))))
  (csv-show-fill-buffer))

(provide 'csv-show)
;;; csv-show.el ends here
