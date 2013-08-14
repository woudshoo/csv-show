;;; csv-lens.el --- navigate and edit CSV files

;; Copyright (C) 2006  Alex Schroeder <alex@gnu.org>
;; Copyright (C) 2013  Tom Koelman
;; Copyright (C) 2013  Willem Rein Oudshoorn <woudshoo@xs4all.nl>
;;
;; Author: Alex Shroeder <alex@gnu.org>
;;     Tom Koelman
;;     Willem Rein Oudshoorn <woudshoo@xs4all.nl>
;; Maintainer: ???
;; Version: 0.1
;; Keywords: data
;; Homepage: http://github.com/woudshoo/csv-show
;; Package-Requires: ((cl-lib "1.0")
;;                    (s "1.6.1")
;;                    (dash "1.5.0")
;;                    (ht "1.3")
;;                    (sparkline "0.3")
;;                    (vendor-from-wwn "0.1.0"))
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

;; Use the `csv-lens-mode' minor mode in a CSV file to activate
;; the csv-lens feature.
;;
;; When this minor mode is enabled C-return will open up a new buffer
;; showing the content of the current CSV row in a table format.
;;
;; In this csv-lens buffer the keys `n' and 'p' will select the next
;; or previous row to display.

;;; Code:

(require 'cl-lib)
(require 's)
(require 'dash)
(require 'vl)
(require 'ht)
(require 'calc)
(require 'simple)
(require 'sparkline)
(require 'vendor-from-wwn)
(require 'csv-lens-cell)

;; Variables
(defvar csv-lens-key-column-name nil 
  "Name of the key column.  
This is set by the user (or defaults to InstanceID) 
and is used for the navigation commands to go the next line
with the same key value.
Also used when making sparkline graphs to create the sparkline
for the element indicated by the key column value.")

(defvar csv-lens--key-column-field-index nil
  "Column number of the csv-lens-key-column-name in the source buffer.
This should not be set by the user, but the code that updates the 
`csv-lens-key-column-name' should also update this value.")

(defvar csv-lens-source-line-no)
(defvar csv-lens-source-marker)
(defvar csv-lens-previous-cells)
(defvar csv-lens-detail-map)
(defvar csv-lens-column-state)
(defvar csv-lens-spark-line-incremental)
(defvar csv-lens-column-state-toggle)
(defvar csv-lens-format-toggle)
(defvar csv-lens-columns)
(defvar csv-lens-cells)
(defvar csv-lens-previous-line)
(defvar csv-lens-previous-cells)

(defvar csv-lens--get-columns-cache nil)

(defvar csv-lens-map)

(defvar csv-lens-update-timer nil
  "Holds the timer used to keep the *CSV Lens* buffer in sync
with the underlying CSV buffer.

If nil the timer is not active.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro in-other-buffer (marker bindings &rest body)
  "Executes BODY in the buffer indicated by MARKER.  
The point in the buffer is set to the point of the MARKER.

The BINDINGS are a list of bindings of the form (var expr).
Each expr is evaluated after the body but in the buffer indicated
by the MARKER.  The value of the expresion is assigned to var
but the var is in context of the current buffer.

This is useful for updating buffer local variables with values
from another buffer.  e.g. 

 (in-other-buffer marker-of-other-buffer
      ((buffer-local-var buffer-local-var)))

will copy the buffer-local-var from (marker-buffer marker-of-other-buffer) 
to buffer-local-var in the current buffer."
  (declare (indent 2))
  (let* ((old-mark (make-symbol "OLD-MARKER"))
	 (tmps (mapcar (lambda (v) (make-symbol "TMP")) bindings))
	 (set-tmps (cl-mapcar (lambda (v tmp) (list tmp (cadr v))) bindings tmps))
	 (set-vars (cl-mapcar (lambda (v tmp) (list (car v) tmp)) bindings tmps)))
    (if bindings
	`(let ,tmps
	   (let ((,old-mark ,marker))
	     (with-current-buffer (marker-buffer ,marker)
	       (save-excursion
		 (goto-char ,old-mark)
		 ,@body
		 ,@(mapcar (lambda (tmp-form) `(setq ,@tmp-form)) set-tmps))))
	   ,@(mapcar (lambda (var-form) `(setq ,@var-form)) set-vars))
      `(let ((,old-mark ,marker))
	(with-current-buffer (marker-buffer ,marker)
	  (save-excursion 
	    (goto-char ,old-mark)
	    ,@body))))))

(defun csv-lens--marker-for-source-buffer ()
  "Returns a marker for the source buffer location which is used 
in the Detail buffer.  If the current buffer is not a detail buffer
it should be a CSV file and it will return (point-marker)."
  (if (boundp 'csv-lens-source-marker)
      csv-lens-source-marker
    (point-marker)))

(defmacro csv-lens--in-source-buffer (bindings &rest body)
  (declare (indent 1))
  `(in-other-buffer (csv-lens--marker-for-source-buffer) ,bindings ,@body))


(setq csv-lens-map
      (let ((map (make-sparse-keymap)))
	(define-key map [?\C-.] 'csv-lens-toggle-timer)
	(define-key map [C-return] 'csv-lens-select)
	map))

(defun set-key-column-field-index ()
  ""
  (setq-local csv-lens--key-column-field-index (csv-lens--field-index-for-column csv-lens-key-column-name)))

;;;###autoload
(define-minor-mode csv-lens-mode 
  "Shows a row in a CSV file in a separate buffer.

This is a minor mode to show in a separate buffer the content
of the current line as a table.

\\{csv-show-map}"
  nil " csv-lens" csv-lens-map
  (make-local-variable 'csv-lens-key-column-name)
  (make-local-variable 'csv-lens--key-column-field-index)
  (setq-local csv-lens-key-column-name "InstanceID") ;Holds the name of the column that is used as key column.
  (setq-local csv-lens--key-column-field-index nil)  ;Holds the field index of the column that is used as key column.
  (set-key-column-field-index))

(setq csv-lens-detail-map 
      (let ((map (make-sparse-keymap)))
	(set-keymap-parent map special-mode-map)
	(define-key map "n" 'csv-lens-next)
	(define-key map "N" (lambda () (interactive) (csv-lens-next/prev-statistictime 1)))
	(define-key map "." 'csv-lens-current)
	(define-key map [?\C-.] 'csv-lens-toggle-timer)
	(define-key map "p" 'csv-lens-prev)
	(define-key map "P" (lambda () (interactive) (csv-lens-next/prev-statistictime -1)))
	(define-key map "h" 'csv-lens-hide-column)
        (define-key map "c" 'csv-lens-hide-constant-columns)
	(define-key map "b" 'csv-lens-bold-column)
	(define-key map "u" 'csv-lens-normal-column)
	(define-key map "U" 'csv-lens-normal-all)
	(define-key map "s" 'csv-lens-column-state-toggle)
	(define-key map "S" 'csv-lens-spark-line)
        (define-key map "Z" 'csv-lens-spark-line-for-all-visible-columns)
	(define-key map "I" 'csv-lens-spark-line-toggle-incremental)
        (define-key map "o" 'csv-lens-switch-to-source-buffer)
        (define-key map "j" 'csv-lens-next-value)
        (define-key map "k" 'csv-lens-prev-value)
	(define-key map "K" 'csv-lens-set-key-column)
        (define-key map "Q" 'csv-lens-kill-detail-buffer)
        (define-key map [C-return] 'csv-lens-switch-to-source-buffer)
        (define-key map "f" 'csv-lens-format-toggle)
        (define-key map "<" 'csv-lens-jump-first-line-for-key-value)
        (define-key map ">" 'csv-lens-jump-last-line-for-key-value)
	map))

(define-generic-mode csv-lens-detail-mode
  nil nil nil nil '(csv-lens--detail-setup)
  "Major mode for viewing CSV file records.

This mode is enabled for buffers that are created by the
`csv-lens-select' function.  It should not be toggled by the user.

\\{csv-show-detail-map}")

(defun csv-lens--detail-setup ()
  "Main code to setup the csv-lens major mode.
This mode should not be selected by the user, but by 
the `csv-lens-select' function."
  (setq font-lock-defaults nil) 
  (use-local-map csv-lens-detail-map)
  (make-local-variable 'csv-lens-source-marker)
  (make-local-variable 'csv-lens-source-line-no)
  (make-local-variable 'csv-lens-columns)
  (make-local-variable 'csv-lens-cells)
  (make-local-variable 'csv-lens-previous-cells)
  (make-local-variable 'csv-lens-previous-line)
  (setq-local csv-lens-spark-line-incremental nil)
  (setq-local csv-lens-column-state (list))
  (setq-local csv-lens-column-state-toggle nil)
  (setq-local csv-lens-format-toggle t)
  (setq buffer-read-only t))

(defvar csv-lens-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\"  "\"" table)
    (modify-syntax-entry ?,  "." table)
    table))

(defun csv-lens-parse-field (start)
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
    (s-trim field)))

(defun csv-lens--field-index-for-column (column)
  "Return the index of COLUMN."
  (position column (csv-lens--get-columns) :test #'equal))
    

(defun csv-lens-parse-line (&optional indices)
  "Parse the current line and return the list of values.
When  INDICES is specified, returns a list with values on those INDICES."
  (let ((start (point))
        (index -1)
        (all-indices (not indices))
        current-value
	result)
    (with-syntax-table csv-lens-syntax-table
      (while (and start
                  (or all-indices
                      indices))
        (setq current-value nil)
	(skip-syntax-forward "^.\" ")
	(cond ((eq (char-after) ?,)
               (setq current-value (csv-lens-parse-field start)
                     index (1+ index)
                     start (1+ (point)))
	       (forward-char 1))
              ((eq (char-after) ?\n)
               (setq current-value (csv-lens-parse-field start)
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

(defun csv-lens-parse-line-vec ()
  "Dumb `csv-lens-parse-line' that is fast but not always correct."
  (vconcat (mapcar 's-trim 
		   (split-string (buffer-substring-no-properties 
				  (progn (beginning-of-line) (point)) 
				  (progn (end-of-line) (point))) 
				 ","))))




(defun csv-lens--get-columns ()
  "Get the field names of the buffer."
  (or csv-lens--get-columns-cache
      (save-excursion
        (goto-char (point-min))
        (csv-lens-parse-line))))

(defun csv-lens--get-cells (&optional indices)
  (save-excursion
    (csv-lens-parse-line indices)))

(defun csv-lens--get-cells-fast (indices)
  "Return a list of values at the current line indicated by the INDICES.

The resulting list is of the same length as `indices'.
If an index, the corresponding value will be nil.

The assumption is that indices is sorted from low to high!"
  (let* ((end (progn (end-of-line) (+ (point) 1)))
	 (column-pos 0)
	 index
	 old-pos
	 new-pos
	 result)
    (beginning-of-line)
    (setq new-pos (point))
    (while (and new-pos
	    (setq index (pop indices)))
      (setq index (- index column-pos))
      (when (> index 0)
	(setq new-pos (search-forward "," end t index)))
      (setq old-pos (point))
      (setq column-pos (+ column-pos index 1))
      (when new-pos
	(setq new-pos (- (or (search-forward "," end t) end) 1))
	(push (buffer-substring-no-properties old-pos new-pos) result)))
    (nreverse result)))

(defun csv-lens--get-cells-ht (indices)
  "Return a list of values at the current line indicated by the INDICES.

The resulting list is of the same length as `indices'.
If an index, the corresponding value will be nil.

The assumption is that indices is sorted from low to high!"
  (let ((end (progn (end-of-line) (+ (point) 1)))
        (column-pos 0)
        index
        i
        old-pos
        new-pos
        (result (ht-create)))
    (beginning-of-line)
    (setq new-pos (point))
    (while (and new-pos
                (setq i (car indices))
	    (setq index (pop indices)))
      (setq index (- index column-pos))
      (when (> index 0)
	(setq new-pos (search-forward "," end t index)))
      (setq old-pos (point))
      (setq column-pos (+ column-pos index 1))
      (when new-pos
	(setq new-pos (- (or (search-forward "," end t) end) 1))
        (ht-set result i (buffer-substring-no-properties old-pos new-pos))))
    result))

(defun csv-lens-buffer-name-for-lens-buffer ( csv-buffer )
  ""
  (concat "*CSV Lens " (f-filename (buffer-file-name csv-buffer)) "*" ))

(defun csv-lens-select ()
  "Show the current row."
  (interactive)
  (let ((current-buffer-v (current-buffer))
	(start (point-marker)))
    (pop-to-buffer (get-buffer-create (csv-lens-buffer-name-for-lens-buffer current-buffer-v)))
    (csv-lens-detail-mode)
    (setq csv-lens-source-marker start)
    (csv-lens-current)))


(defun csv-lens-toggle-timer ()
  "When enabled, the *CSV Lens* buffer tracks the cursor in the
underlying CSV buffer.  This function toggles this
functionality."
  (interactive)
  (if csv-lens-update-timer 
      (progn
	(cancel-timer csv-lens-update-timer)
	(setq csv-lens-update-timer nil))
    (setq csv-lens-update-timer 
	  (run-with-idle-timer 0.1 t 'csv-lens-update-detail-buffer))))



(defun csv-lens-update-detail-buffer ()
  "Updates the *CSV Lens* buffer with the content of the line
containing point in the underlying CSV buffer.  It is similar to the 
`csv-lens-select', except that it does not create a *CSV Lens* buffer
if it exists."
  (interactive)
  (let ((detail-buffer (csv-lens-buffer-name-for-lens-buffer current-buffer-v)))
    (when detail-buffer
      (save-match-data
	(with-current-buffer detail-buffer
	  (csv-lens-current t))))))


(defun csv-lens-set-column-state (column state)
  "Sets the state of `column' to `state'.  
See also `csv-lens-column-state'"
  (let ((assoc-pair (assoc column csv-lens-column-state)))
    (if assoc-pair
	(setcdr assoc-pair csv-lens-column-state))
    (push (cons column state) csv-lens-column-state)))


(defun csv-lens-column-state (column)
  "Return the state of the `COLUMN'.
The valid states are 

  - nil    -- meaning the state is never set.
  - normal -- should have the same meaning as nil.
  - hidden -- hides the column in CSV Lens buffer, 
              but see also `csv-lens-column-state-toggle'
  - constant -- hides the column in CSV Lens buffer"
  (assoc-default column csv-lens-column-state))

(defun csv-lens-column-name (&optional point)
  "Return the column name for the line containing `POINT'.
If `point' is nil or not provided, use the current point in the
buffer."
  (save-excursion
    (when point (goto-char point))
    (beginning-of-line)
    (when (get-text-property (point) 'invisible)
      (goto-char (next-single-property-change (point) 'invisible)))
    (buffer-substring-no-properties (point)
				    (1- (search-forward ":")))))

(defun csv-lens-hide-constant-columns ()
  "Hides all columns that have constant value."
  (interactive)
  (let (constant-columns)
    (csv-lens--in-source-buffer ((constant-columns (csv-lens-constant-columns))))
    (message "%d constant columns hidden." (length constant-columns))
    (dolist (column constant-columns)
      (csv-lens-set-column-state column 'constant)))
  (csv-lens-fontify-detail-buffer))

(defadvice csv-lens-hide-constant-columns (around time-csv-show-hide-constant-columns)
  ""
  (interactive)
  (let ((c-s-s (current-time)))
    ad-do-it
    (let ((elapsed (float-time (time-subtract (current-time) c-s-s))))
      (message "Hiding constant columns took %.3fs" elapsed))))
(ad-activate 'csv-lens-hide-constant-columns)

(defun csv-lens-diff-values (list)
  (let ((first-value (first list))
	result)
    (dolist (element (rest list))
      (push (- element first-value) result)
      (setq first-value element))
    (nreverse result)))


(defun csv-lens-spark-line-toggle-incremental ()
  "Toggle between using diff's of values for the sparkle lines"
  (interactive)
  (setq csv-lens-spark-line-incremental (not csv-lens-spark-line-incremental))
  (csv-lens-fill-buffer))

(defun csv-lens-spark-line-for-all-visible-columns ()
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (forward-line 3)
    (let ((list-of-non-sparkling-column-names (list "InstanceID" "StatisticTime" "ElementType")))
      (while (thing-at-point 'symbol t)
        (let ((column-name (thing-at-point 'symbol t)))
          (if (and (not (equal (csv-lens-column-state column-name) 'hidden))
                   (not (-contains? list-of-non-sparkling-column-names column-name)))
            (csv-lens-spark-line)
            (forward-line)))))))


(defun csv-lens-spark-line ()
  (interactive)

  (let ((column (csv-lens-column-name))
	(result (list)))
    (message (concat "Spark line for " column ))
    (csv-lens--in-source-buffer
	nil
     (let ((key-index csv-lens--key-column-field-index)
	   (value-index (csv-lens--field-index-for-column column))
	   indices key--index value--index key-value)

       (if key-index
	   (if (> value-index key-index)
	       (progn 
		 (setq key--index 0)
		 (setq value--index 1)
		 (setq indices (list key-index value-index)))
	     (progn 
	       (setq key--index 1)
	       (setq value--index 0)
	       (setq indices (list value-index key-index))))
	 (progn
	   (setq key--index nil)
	   (setq value--index 0)
	   (setq indices (list value-index))))
       
       (cl-flet ((value-to-plot (line-values)
			     (nth value--index line-values))
	      (key-value (line-values)
			 (when key-index (nth key--index line-values))))

	 (setq key-value (key-value (csv-lens--get-cells-fast indices)))

	 (goto-char (point-min))
	 (while (and (forward-line)
		     (not (eobp)))
	   
	   (let* ((line-values (csv-lens--get-cells-fast indices))
		  (value (value-to-plot line-values))
		  (key (key-value line-values)))
	     
	     (when (and value (equal key key-value))
	       (let ((value (string-to-number value)))
		 (when value
		   (push value result)))))))))
    
    (setq result (nreverse result))
    (when csv-lens-spark-line-incremental
      (setq result (csv-lens-diff-values result)))
    (csv-lens-set-column-state column (sparkline-make-sparkline 80 11 result))
    (csv-lens-fontify-detail-buffer)
    (next-line)))

(defun csv-lens-set-key-column ()
  "Will mark the column as Key column."
  (interactive)
  (let ((column (csv-lens-column-name)))
    (csv-lens--in-source-buffer 
	nil 
     (setq csv-lens-key-column-name column)
     (set-key-column-field-index))))

(defun csv-lens-hide-column ()
  "Will mark the column on the current row for hiding. 
Depending on the `column-state-toggle' it will either immediate hide
the column, or it will mark it visibly as hidden.

If used on an already hidden column (displayed with the highlight),
unhide the column.

See also `csv-lens-column-state-toggle'"
  (interactive)
  (let ((column (csv-lens-column-name)))
    (case (csv-lens-column-state column)
      ('hidden (csv-lens-set-column-state column 'normal))
      (t (csv-lens-set-column-state column 'hidden))))
  (csv-lens-fontify-detail-buffer)
  (when csv-lens-column-state-toggle
    (next-line)))

(defun csv-lens-normal-column ()
  "Remove all state: bold, hidden, sparkline etc.  from the current column."
  (interactive)
  (csv-lens-set-column-state (csv-lens-column-name) 'normal)
  (csv-lens-fontify-detail-buffer)
  (next-line))

(defun csv-lens-normal-all ()
  "Remove all states from all columns."
  (interactive)
  (setq csv-lens-column-state nil)
  (csv-lens-fontify-detail-buffer))

(defun csv-lens-bold-column ()
  "Will mark the column on the current row for bolding. 

See also `csv-lens-column-state-toggle'"
  (interactive)
  (let ((column (csv-lens-column-name)))
    (case (csv-lens-column-state column)
      ('bold (csv-lens-set-column-state column 'normal))
      (t (csv-lens-set-column-state column 'bold))))
  (csv-lens-fontify-detail-buffer)
  (next-line))

(defun csv-lens-column-state-toggle ()
  "Toggles between showing all columns and hiding the columns that
are marked for hiding.  See also `csv-lens-hide-column'"
  (interactive)
  (setq csv-lens-column-state-toggle (not csv-lens-column-state-toggle))
  (csv-lens-fontify-detail-buffer))

(defun csv-lens-format-toggle ()
  "Toggle between showing raw values and their formatted counterparts."
  (interactive)
  (setq csv-lens-format-toggle (not csv-lens-format-toggle))
  (csv-lens-fill-buffer))

(defun csv-lens--insert-cell ( column cell )
  ""
  (if csv-lens-format-toggle
      (insert (funcall (csv-lens-cell-format-function-for-column column) cell))
    (insert cell)))

(defun smis-time-to-time-string ( smis-time )
  (format "%s-%s-%s %s:%s:%s"
          (substring smis-time 0 4)
          (substring smis-time 4 6)
          (substring smis-time 6 8)
          (substring smis-time 8 10)
          (substring smis-time 10 12) 
          (substring smis-time 12 14)))

(defun parse-smis-time-string ( smis-time )
  "Convert SMIS-TIME to a time."
  (date-to-time (smis-time-to-time-string smis-time)))

(defun float-smis-time ( smis-time )
  "Return a float representing the epoch for SMIS-TIME."
  (float-time (parse-smis-time-string smis-time)))

(defun diff-smis-times ( smis-time1 smis-time2 )
  "Return the difference in seconds of SMIS-TIME1 - SMIS-TIME2."
  (- (float-smis-time smis-time1) (float-smis-time smis-time2)))

(defun seconds-to-string (seconds)
  "Convert SECONDS to a nicely formatted string with hours, minutes and seconds."
  (let (result)
    (dolist (divider (list (cons 3600 nil) (cons 60 ":") (cons 1 "'")))
      (let ((amount (truncate (/ seconds (car divider)))))
          (setq result (concat result (cdr divider) (format "%02d" amount))
                seconds (- seconds (* amount (car divider))))))
    result))

(defun csv-lens--diff-statistictime ( time1 time2 )
  "Return a nice string representation of TIME1 - TIME2."
  (seconds-to-string (diff-smis-times time1 time2)))

(defun csv-lens--make-sure-string-doesnt-start-with ( prefix s )
  "Remove as much instances of PREFIX from the start of S so that it doesn't start with PREFIX anymore.
However, if S has a length greater than 0 to begin with, it never leaves S at length 0."
  (if (= (length prefix) 0)
      s
    (progn
      (while (and (s-starts-with? prefix s)
                  (> (length s) (length prefix)))
        (setq s (substring s (length prefix))))
      s)))


(defun csv-lens--diff-number (num1 num2)
  "Given two strings num1 and num2 containing arbitrary numbers,
returns a string representing the difference.
Think of it as num1 - num2."
  (let ((num1 (math-read-number num1))
	(num2 (math-read-number num2)))
    (if (and num1 num2)
	(math-format-number (math-sub num1 num2))
      "")))


(defun csv-lens--diff-cells ( column cell1 cell2 )
  "Given CELL1 and CELL2 and their COLUMN, returns an appropriate diff between them. Think of it as CELL1 - CELL2."
  (cond ((member column '("InstanceID" "ElementType"))
                 nil)
         ((equal column "StatisticTime")
          (csv-lens--diff-statistictime cell1 cell2))
         (t (csv-lens--diff-number cell1 cell2))))

(defun csv-lens--line-col-position ()
  "Returns the position of point as a line number, column number combination"
  (cons (line-number-at-pos) (current-column)))

(defun csv-lens--restore-line-col-position (line-col)
  "Move point to the `LINE-COL' position, see `csv-lens--line-col-position'."
  (goto-char (point-min))
  (forward-line (1- (car line-col)))
  (move-to-column (cdr line-col)))

(defun csv-lens--fill-line (column width cell cell-width previous-cell)
  (insert column ":")
  (move-to-column (+ 4 width) t)
  (csv-lens--insert-cell column cell)
  (move-to-column (+ 4 width cell-width 1) t)
  (when previous-cell
    (csv-lens--insert-cell column previous-cell)
    (let ((diff (csv-lens--diff-cells column cell previous-cell)))
      (move-to-column (+ 4 width cell-width 1 cell-width 1) t)
      (when diff
	(insert diff)
	(move-to-column (+ 4 width cell-width 1 cell-width 1 cell-width 1) t))))
  (insert " \n"))

(defun csv-lens-fill-buffer-cell-width ()
  "Calculate the maximum width of csv-lens-cells, taking formatting into account."
  (if csv-lens-format-toggle
      (let ((columns csv-lens-columns)
            (cells csv-lens-cells)
            widths)
        (while columns
          (let ((column (pop columns))
                (cell (pop cells)))
            (push (length (funcall (csv-lens-cell-format-function-for-column column) cell))
                  widths)))
        (reduce 'max widths))
    (reduce 'max csv-lens-cells :key 'length)))

(defun csv-lens-fill-buffer ()
  "Fills the buffer with the content of the cells."
    (let ((current-position (csv-lens--line-col-position))
	  (buffer-read-only nil))
      (erase-buffer)

      (insert "FILE: " (buffer-name (marker-buffer csv-lens-source-marker))
	      " Spark Lines use " (if csv-lens-spark-line-incremental
				      "DIFFs" "Values") 
	      " for plotting"
	      "\n\n")
      
      (let ((width (reduce 'max csv-lens-columns :key 'length))
            (cell-width (csv-lens-fill-buffer-cell-width))
            (display-columns (-flatten (list "Line" csv-lens-columns)))
            (display-cells (-flatten (list (format "%d" csv-lens-source-line-no) csv-lens-cells))))
	(if csv-lens-previous-cells
            (let ((display-previous-cells (-flatten (list (format "%d" csv-lens-previous-line) csv-lens-previous-cells))))
              (cl-mapcar (lambda (column cell previous-cell)
                           (csv-lens--fill-line column width cell cell-width previous-cell))
                         display-columns display-cells display-previous-cells))
	  (cl-mapcar (lambda (column cell)
		       (csv-lens--fill-line column width cell cell-width nil))
		     display-columns display-cells)))
      (csv-lens-fontify-detail-buffer)
      (csv-lens--restore-line-col-position current-position)))

(defun csv-lens-fontify-detail-buffer ()
  "Fontifies the detail buffer, assumes that the detail buffer is current buffer."
  (save-excursion
    (let ((buffer-read-only nil))
      (goto-char (point-min))
      (set-text-properties (point) (point-max) nil)
      (forward-line 2)
      (while (not (eobp))
	(let ((column (csv-lens-column-name))
	      (start (point)))
	  (case (csv-lens-column-state column)
	    ((hidden constant)
	     (forward-line)
	     (if csv-lens-column-state-toggle
		 (put-text-property start (point) 'face 'highlight)
	       (put-text-property start (point) 'invisible t)))
	    (bold
	     (forward-line)
	     (put-text-property start (point) 'face '(:weight bold)))
	    (t
	     (put-text-property start (search-forward ":") 'face 'font-lock-keyword-face)
	     (when (and (consp (csv-lens-column-state column)))
	       (end-of-line)
	       (put-text-property (- (point) 1) (point) 'display (csv-lens-column-state column)))
	     (forward-line))))))))

(defun csv-lens--mark-forward/backward (dir &optional do-not-parse-headers)
  "Move the selection to the next or previous record.
Note that this does not update the content of the buffer,
it will parse the column, cells and put these into the
corresponding local variables of the CSV-Detail buffer.

Also move the mark down or up and update the line-no
variable.

For updating the content see the function `csv-lens-fill-buffer'."
  (let (new-show-columns)
    (setq csv-lens-previous-cells nil)
    (setq csv-lens-previous-line nil)
    (csv-lens--in-source-buffer
	((csv-lens-source-marker (point-marker))
	 (csv-lens-source-line-no (line-number-at-pos (point)))
	 (csv-lens-cells (csv-lens--get-cells))
	 (new-show-columns (unless do-not-parse-headers
			     (csv-lens--get-columns))))
      
      (forward-line (or dir 1))
      (beginning-of-line))
    (unless do-not-parse-headers
      (setq csv-lens-columns new-show-columns))))

(defun csv-lens-current (&optional do-not-parse-headers)
  "Update the content of the *CSV-Detail* buffer with the content
of the current line.  
This function requires that the current buffer is a *CSV-Detail* buffer."
  (interactive)
  (setq csv-lens-source-marker 
	(with-current-buffer (marker-buffer csv-lens-source-marker)
	  (save-excursion
	    (beginning-of-line)
	    (point-marker))))
  (csv-lens--mark-forward/backward 0 do-not-parse-headers) 
  (csv-lens-fill-buffer))

(defun csv-lens-next/prev (&optional dir)
  "Shows the next or previous record."
  (interactive "p")
  (csv-lens--mark-forward/backward dir t)
  (csv-lens-fill-buffer))

(defun csv-lens-next ()
  "Shows the next record of the underlying CSV file."
  (interactive)
  (csv-lens-next/prev 1))

(defun csv-lens-prev ()
  "Shows the previous record of the underlying CSV file."
  (interactive)
  (csv-lens-next/prev -1))

(defun csv-lens--get-current-value-for-index (index)
  "Returns the value of the INDEXth item on the current line. Returns nil when index not given."
  (when index
    (beginning-of-line)
    (car (csv-lens-parse-line (list index)))))

(defun csv-lens-next/prev-statistictime (&optional dir)
  "Shows the next or previous record for which the StatisticTime
field is different than the current, and InstanceID is
identical."
  (interactive)
  (setq csv-lens-previous-cells csv-lens-cells)
  (setq csv-lens-previous-line csv-lens-source-line-no)
  (csv-lens--in-source-buffer
		   ((csv-lens-source-marker (point-marker))
		    (csv-lens-source-line-no (line-number-at-pos (point)))
		    (csv-lens-cells (csv-lens--get-cells)))
		   
		   (csv-lens--next/prev-value "StatisticTime" (or dir 1)))
  (csv-lens-fill-buffer))


(defun csv-lens-next/prev-value (&optional dir)
  "Shows the next or previous record for which the value of the
current column is different than the current value for the
current column, and InstanceID is identical."
  (interactive)
  (setq csv-lens-previous-cells csv-lens-cells)
  (setq csv-lens-previous-line csv-lens-source-line-no)
  (let ((variable-column (csv-lens-column-name)))
    (csv-lens--in-source-buffer
     ((csv-lens-source-marker (point-marker))
      (csv-lens-source-line-no (line-number-at-pos (point)))
      (csv-lens-cells (csv-lens--get-cells)))
     (csv-lens--next/prev-value variable-column (or dir 1))))
  (csv-lens-fill-buffer))

(defun csv-lens--jump-first/last-line-for-key-value ( key-value first-last )
  ""
  (let ((start-point (point-min))
        (progress-function 'forward-line))
    (if (equal first-last 'last)
        (setq start-point (point-max)
              progress-function (lambda() (forward-line -1))))
    (goto-char start-point)
  (while (and (funcall progress-function)
              (not (eobp))
              (not (equal 
                    key-value
                    (car (csv-lens-parse-line (list csv-lens--key-column-field-index)))))))))
  
(defun csv-lens-jump-first/last-line-for-key-value ( first-last )
  "Expected to be performed in the detail buffer. Jumps to the first or last line in the
source file that has the same value for `csv-lens-key-column' as the current line. When FIRST-LAST
is 'first, jumps to the first, when FIRST-LAST is 'last, jumps to the last."
  (interactive)
  (setq csv-lens-previous-cells nil)
  (setq csv-lens-previous-line nil)
  (let (key-index indices)
    (csv-lens--in-source-buffer ((key-index csv-lens--key-column-field-index)
                                 (indices (csv-lens--indices-of-columns))))
    (let ((key-value (nth key-index csv-lens-cells)))
      (csv-lens--in-source-buffer ((csv-lens-source-marker (point-marker))
                                   (csv-lens-source-line-no (line-number-at-pos (point)))
                                   (csv-lens-cells (csv-lens--get-cells-fast indices)))
                                  (csv-lens--jump-first/last-line-for-key-value key-value first-last))))
  (csv-lens-fill-buffer))

(defun csv-lens-jump-first-line-for-key-value ()
  "Expected to be performed in the detail buffer. Jumps to the first line in the
source file that has the same value for `csv-lens-key-column' as the current line."
  (interactive)
  (csv-lens-jump-first/last-line-for-key-value 'first))

(defun csv-lens-jump-last-line-for-key-value ()
  "Expected to be performed in the detail buffer. Jumps to the last line in the
source file that has the same value for `csv-lens-key-column' as the current line."
  (interactive)
  (csv-lens-jump-first/last-line-for-key-value 'last))

(defun csv-lens--all-key-values ()
  "Return a list of all values for the `csv-lens-key-column'."
  (let ((key-values (ht-create)))
    (csv-lens--in-source-buffer nil
     (goto-char (point-min))
     (while (and (forward-line)
                 (not (eobp)))
       (let ((current-key-value (car (csv-lens--get-cells-fast (list csv-lens--key-column-field-index)))))
         (ht-set key-values current-key-value 1))))
    (ht-keys key-values)))

(defun csv-lens-next-value ()
  "Show next record for which the current field is different, see `csv-lens-next/prev-value'"
  (interactive)
  (csv-lens-next/prev-value 1))

(defun csv-lens-prev-value ()
  "Show previous record for which the current field is different, see `csv-lens-next/prev-value'"
  (interactive)
  (csv-lens-next/prev-value -1))

(defun csv-lens--next/prev-value (column dir)
  "Moves up or down in the CSV file (current buffer) until a line is encountered 
with a different value for column but the same instance id. Returns t if such a
line was found, nil otherwise.

Pre conditions are:  
 - point is at the beginning of a line.

Post conditions:
 - point is at the beginning of the new line.
"
  (let* ((csv-lens--get-columns-cache (csv-lens--get-columns)) 
	 (variable-column-index (csv-lens--field-index-for-column column))
	 (instanceid-index csv-lens--key-column-field-index)
	 (current-value (csv-lens--get-current-value-for-index variable-column-index))
	 (current-instanceid (csv-lens--get-current-value-for-index instanceid-index))
         (found t))
    (while (or
	    (not (equal current-instanceid (csv-lens--get-current-value-for-index instanceid-index)))
	    (equal current-value (csv-lens--get-current-value-for-index variable-column-index)))
      (beginning-of-line)
      (unless (equal (forward-line dir) 0)
	(error "No more records")
        (setq found nil)))
    (beginning-of-line)
    found))

(defun csv-lens--indices-of-columns()
  "Returns a list of indices of all the columns."
  (let (column-indices)
    (--dotimes (length (csv-lens--get-columns))
      (!cons it column-indices))
    (nreverse column-indices)))

(defun csv-lens--key-value-from-column-indices-and-values (column-indices values)
  "Given a list of COLUMN-INDICES and a corresponding list of VALUES, returns the value
   corresponding to csv-lens-key-column-field-index."
  (-list-item-in-list-where-item-in-other-list values column-indices csv-lens--key-column-field-index))

(defun csv-lens--constant-columns (candidate-constant-columns key-column-index current-values previous-values)
  "Return a list of indices for which `current-values' and `previous-values' are equal 
and which occur in `candidate-constant-columns'.  Also note that
the values at `key-column-index' are always considered equal, even if
they are not (they are not compared).  So `key-column-index' is
never removed from the result.

The order of the indices in the result is the same as the order in
input `candidate-constant-columns'."
  (--filter 
   (or (equal it key-column-index)
       (equal (ht-get previous-values it)
              (ht-get current-values it)))
   candidate-constant-columns))


(defun csv-lens-constant-columns ()
  "Analyzes a csv buffer and returns a list of the column names that contain constant values."
  (interactive)
  (let* ((constant-columns-indices (csv-lens--indices-of-columns))
         (all-columns-indices constant-columns-indices)
         (line-number 0)
         (reporter (make-progress-reporter "Scanning for constant columns..." 0 (count-lines (point-min) (point-max))))
         (previous-cells (ht-create)))

    (goto-char (point-min))

    (while (and (cdr constant-columns-indices)
		(forward-line) 
		(not (eobp)))
      (progress-reporter-update reporter (incf line-number))

      ;; Processing new line
      (let* ((current-values-ht (csv-lens--get-cells-ht constant-columns-indices))
             (key-value (ht-get current-values-ht csv-lens--key-column-field-index))
	     (previous (ht-get previous-cells key-value)))
        (ht-set previous-cells key-value current-values-ht)
        (when previous
          (let ((previous-number-of-constant-columns (length constant-columns-indices)))
            (setq constant-columns-indices 
                  (csv-lens--constant-columns constant-columns-indices 
                                              csv-lens--key-column-field-index
                                              current-values-ht
                                              previous))
            (when (not (equal previous-number-of-constant-columns (length constant-columns-indices)))
              (progress-reporter-force-update
               reporter
               line-number
               (format "%d possible constant columns left... " (- (length constant-columns-indices) 1))))))))

      ;; Remove key column from constant list
      (setq constant-columns-indices (delete csv-lens--key-column-field-index 
                                             constant-columns-indices))

      (progress-reporter-done reporter)
      (goto-char (point-min))
      (when constant-columns-indices
        (csv-lens--get-cells constant-columns-indices))))

(defun csv-lens-switch-to-source-buffer ()
  "Switch to the source line in the underlying CSV file."
  (interactive)
  (let ((line-no csv-lens-source-line-no))
    (pop-to-buffer (marker-buffer csv-lens-source-marker))
      (goto-line line-no)))

(defun csv-lens-kill-detail-buffer ()
  "Expected to be performed from the detail buffer."
  (interactive)
  (let ((source (marker-buffer csv-lens-source-marker)))
    (kill-buffer)
    (pop-to-buffer source))
)

(defun csv-lens-kill-both-buffers ()
  "Expected to be performed from the detail buffer."
  (interactive)
  (kill-buffer (marker-buffer csv-lens-source-marker))
  (csv-lens-kill-detail-buffer))

(provide 'csv-lens)
;;; csv-lens.el ends here
