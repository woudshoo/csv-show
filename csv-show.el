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
(require 'cl-lib)
(require 's)
(require 'dash)
(require 'calc)

(require 'spark-lines)

(defmacro in-other-buffer (marker bindings &rest body)
  "Executes `body' in the buffer indicated by `marker'.  
The point in the buffer is set to the point of the `marker'.

The `bindings' are a list of bindings of the form (var expr).
Each expr is evaluated after the body but in the buffer indicated by the `marker'.
The value of the expresion is assigned to var but the var is in context
of the current buffer.  

This is useful for updating buffer local variables with values
from another buffer.  e.g. 

`(in-other-buffer marker-of-other-buffer ((buffer-local-var buffer-local-var)))`

will copy the buffer-local-var from (marker-buffer marker-of-other-buffer) to 
buffer-local-var in the current buffer. "
  (declare (indent 1))
  (let* ((old-mark (make-symbol "OLD-MARKER"))
	 (tmps (mapcar (lambda (v) (make-symbol "TMP")) bindings))
	 (set-tmps (cl-mapcar (lambda (v tmp) (list tmp (cadr v))) bindings tmps))
	 (set-vars (cl-mapcar (lambda (v tmp) (list (car v) tmp)) bindings tmps)))
    `(let ,tmps
       (let ((,old-mark ,marker))
	 (with-current-buffer (marker-buffer ,marker)
	   (save-excursion
	     (goto-char ,old-mark)
	     ,@body
	     ,@(mapcar (lambda (tmp-form) `(setq ,@tmp-form)) set-tmps))))
       ,@(mapcar (lambda (var-form) `(setq ,@var-form)) set-vars))))

(defun csv-show--marker-for-source-buffer ()
  "Returns a marker for the source buffer location which is used 
in the Detail buffer.  If the current buffer is not a detail buffer
it should be a CSV file and it will return the point-marker."
  (if (boundp 'csv-show-source-marker)
      csv-show-source-marker
    (point-marker)))

(defmacro csv-show--in-source-buffer (bindings &rest body)
  `(in-other-buffer (csv-show--marker-for-source-buffer) ,bindings ,@body))

(setq csv-show-map
      (let ((map (make-sparse-keymap)))
	(define-key map [?\C-.] 'csv-show-toggle-timer)
	(define-key map [C-return] 'csv-show-select)
	map))

(defun set-key-column-field-index ()
  ""
  (setq-local csv-show-key-column-field-index (csv-show--field-index-for-column csv-show-key-column-name)))

;;;###autoload
(define-minor-mode csv-show-mode 
  "Shows a row in a CSV file in a separate buffer.

This is a minor mode to show in a separate buffer the content
of the current line as a table.

\\{csv-show-map}"
  nil " csv-show" csv-show-map
  (make-local-variable 'csv-show-key-column-name)
  (make-local-variable 'csv-show-key-column-field-index)
  (setq-local csv-show-key-column-name "InstanceID") ;Holds the name of the column that is used as key column.
  (setq-local csv-show-key-column-field-index nil)  ;Holds the field index of the column that is used as key column.
  (set-key-column-field-index))

(setq csv-show-detail-map 
      (let ((map (make-sparse-keymap)))
	(set-keymap-parent map special-mode-map)
	(define-key map "n" 'csv-show-next)
	(define-key map "N" (lambda () (interactive) (csv-show-next/prev-statistictime 1)))
	(define-key map "." 'csv-show-current)
	(define-key map [?\C-.] 'csv-show-toggle-timer)
	(define-key map "p" 'csv-show-prev)
	(define-key map "P" (lambda () (interactive) (csv-show-next/prev-statistictime -1)))
	(define-key map "h" 'csv-show-hide-column)
        (define-key map "c" 'csv-show-hide-constant-columns)
	(define-key map "b" 'csv-show-bold-column)
	(define-key map "u" 'csv-show-normal-column)
	(define-key map "U" 'csv-show-normal-all)
	(define-key map "s" 'csv-show-column-state-toggle)
	(define-key map "S" 'csv-show-spark-line)
        (define-key map "Z" 'csv-show-spark-line-for-all-visible-columns)
	(define-key map "I" 'csv-show-spark-line-toggle-incremental)
        (define-key map "o" 'csv-show-switch-to-source-buffer)
        (define-key map "j" 'csv-show-next-value)
        (define-key map "k" 'csv-show-prev-value)
	(define-key map "K" 'csv-show-set-key-column)
        (define-key map [C-return] 'csv-show-switch-to-source-buffer)
        (define-key map "f" 'csv-show-format-toggle)
        (define-key map "1" 'csv-show-jump-first-line-for-key-value)
	map))

(define-generic-mode csv-show-detail-mode
  nil nil nil nil '(csv-show--detail-setup)
  "Major mode for viewing CSV file records.

This mode is enabled for buffers that are created by the
`csv-show-select' function.  It should not be toggled by the user.

\\{csv-show-detail-map}")

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
  (make-local-variable 'csv-show-previous-cells)
  (setq-local csv-show-spark-line-incremental nil)
  (setq-local csv-show-column-state (list))
  (setq-local csv-show-column-state-toggle nil)
  (setq-local csv-show-format-toggle t)
  (setq buffer-read-only t))

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
    (s-trim field)))

(defun csv-show--field-index-for-column (column)
  "Returns the index of COLUMN."
  (position column (csv-show--get-columns) :test #'equal))
    
(require 'ert)
(ert-deftest csv-show--field-index-for-column-test ()
  (let ((csv-show--get-columns-cache '("Header1" "Header2" "Header3")))
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

(defun csv-show-parse-line-vec ()
  "Dumb csv-show-parse-line that is fast but not always correct."
  (vconcat (mapcar 's-trim 
		   (split-string (buffer-substring-no-properties 
				  (progn (beginning-of-line) (point)) 
				  (progn (end-of-line) (point))) 
				 ","))))


(defvar csv-show--get-columns-cache nil)

(defun csv-show--get-columns ()
  "Get the field names of the buffer."
  (or csv-show--get-columns-cache
      (save-excursion
        (goto-char (point-min))
        (csv-show-parse-line))))

(defun csv-show--get-cells (&optional indices)
  (save-excursion
    (csv-show-parse-line indices)))

(defun csv-show--get-cells-vec (indices)
  ""
  (vconcat (csv-show--get-cells-fast indices)))

(defun csv-show--get-cells-fast (indices)
  "Returns a list of values at the current
line indicated by the indices. 
The resulting list is of the same length as `indices'.
If an index, the corresponding value will be nil.

The assumption is that indices is sorted from low to high!"
  (let* ((end (progn (end-of-line) (+ (point) 1)))
	 (column-pos 0)
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

(defun csv-show-select ()
  "Show the current row."
  (interactive)
  (let ((current-buffer-v (current-buffer))
	(start (point-marker)))
    (pop-to-buffer (get-buffer-create (concat "*CSV Detail " (buffer-file-name current-buffer-v) "*" )))
    (csv-show-detail-mode)
    (setq csv-show-source-marker start)
    (csv-show-current)))

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

(defun csv-show--statistictime-to-string (statistictime)
  "Returns a nicely formatted STATISTICTIME."
  (interactive)
  (if (> (length statistictime) 18)
      (let (year month day hour minute second offset)
	(setq year (substring statistictime 0 4)
	      month (substring statistictime 4 6)
	      day (substring statistictime 6 8)
	      hour (substring statistictime 8 10)
	      minute (substring statistictime 10 12)
	      second (substring statistictime 12 14)
	      offset (number-to-string (/ (string-to-number (substring statistictime -4)) 60)))
	(concat year "-" month "-" day " " hour ":" minute ":" second " (" offset ")*" ))
    statistictime))

(defun csv-show--usagerestriction-to-string (usagerestriction)
  "Returns a nicely formatted USAGERESTRICTION."
  (interactive)
  (or
   (assoc-default usagerestriction
		  '(("0" .    "Unknown*")
		    ("2" .    "Front-end only*")
		    ("3" .    "Back-end only*")
		    ("4" .    "Not restricted*")))
   usagerestriction))

(require 'vendor-from-wwn)
(defun csv-show--format-wwn (wwn)
  "Returns a nicely formatted WWN."
  (interactive)
  (if (and (vendor-from-wwn/valid-wwn wwn)
           (vendor-from-wwn wwn))
      (concat (vendor-from-wwn/vendor-specific-nice-wwn wwn) " ("  (vendor-from-wwn wwn) ")*" )
    wwn))

(defun csv-show--format-huge-number (hugenumber)
  "Returns a nicely formatted HUGENUMBER."
  (interactive)
  (let (groups)
    (while (> (length hugenumber) 0)
      (if (>= (length hugenumber) 3)
          (progn
            (push (substring hugenumber -3) groups)
            (setq hugenumber (substring hugenumber 0 (- (length hugenumber) 3))))
        (progn
         (push hugenumber groups)
         (setq hugenumber ""))))
    (concat (mapconcat 'identity groups " ") "*")))

(require 'format-human-readable-big-number)

(defun csv-show--format-big-number-of-bytes (big-number-of-bytes)
 ""
 (interactive)
 (format-human-readable-big-number (string-to-number big-number-of-bytes) "%0.1f" *exceptional-format* "B" t :binary ))

(defun csv-show--format-big-number-of-kilobytes (big-number-of-kilobytes)
 ""
 (concat
  (format-human-readable-big-number (* (string-to-number big-number-of-kilobytes) 1024.0) "%0.1f" *exceptional-format* "B" t :binary )
  "*"))

(defun csv-show--format-big-number-of-blocks (big-number-of-blocks)
 ""
 (concat
  (format-human-readable-big-number (* (string-to-number big-number-of-blocks) 512.0) "%0.1f" *exceptional-format* "B" t :binary )
  "*"))

(defvar csv-show-column-format-functions nil)

(setq csv-show-column-format-functions
  `(("StatisticTime" . csv-show--statistictime-to-string)
    ("IM_OriginalStatisticTime" . csv-show--statistictime-to-string)
    ("UsageRestriction" . csv-show--usagerestriction-to-string)
    ("Consumed" . csv-show--format-huge-number)
    ("ConsumableBlocks" . csv-show--format-big-number-of-blocks)
    ("NumberOfBlocks" . csv-show--format-big-number-of-blocks)
    ("KBytesRead" . csv-show--format-big-number-of-kilobytes)
    ("KBytesTransferred" . csv-show--format-big-number-of-kilobytes)
    ("KBytesWritten" . csv-show--format-big-number-of-kilobytes)
    ("MaxSpeed" . csv-show--format-big-number-of-bytes)
    ("RequestedSpeed" . csv-show--format-big-number-of-bytes)
    ("EMCKBytesSPARead" . csv-show--format-big-number-of-kilobytes)
    ("EMCKBytesSPBRead" . csv-show--format-big-number-of-kilobytes)
    ("EMCKBytesSPAWritten" . csv-show--format-big-number-of-kilobytes)
    ("EMCKBytesSPBWritten" . csv-show--format-big-number-of-kilobytes)
    ("PermanentAddress" . csv-show--format-wwn)
    ("SwitchWWPN" . csv-show--format-wwn)
    ("DeviceID" . csv-show--format-wwn)
    ("ElementName" . csv-show--format-wwn)
    ("EMCWWN" . csv-show--format-wwn)
    ("OtherIdentifyingInfo" . csv-show--format-wwn)
    ("Speed" . csv-show--format-big-number-of-bytes)))

(defun csv-show--format-function-for-column (column)
  "Return the format function for COLUMN."
  (or
   (assoc-default column csv-show-column-format-functions )
   #'identity))

(defun csv-show-set-column-state (column state)
  "Sets the state of `column' to `state'.  
See also `csv-show-column-state'"
  (let ((assoc-pair (assoc column csv-show-column-state)))
    (if assoc-pair
	(setcdr assoc-pair csv-show-column-state))
    (push (cons column state) csv-show-column-state)))


(defun csv-show-column-state (column)
  "Returns the state of the `column'.
The valid states are 

  - nil    -- meaning the state is never set.
  - normal -- should have the same meaning as nil.
  - hidden -- hides the column in CSV Detail buffer, 
              but see also `csv-show-column-state-toggle'
  - constant -- hides the column in CSV Detail buffer"
  (assoc-default column csv-show-column-state))

(defun csv-show-column-name (&optional point)
  "Returns the column name for the line containing `point'.
If `point' is nil or not provided, use the current point in the
buffer."
  (save-excursion
    (when point (goto-char point))
    (beginning-of-line)
    (when (get-text-property (point) 'invisible)
      (goto-char (next-single-property-change (point) 'invisible)))
    (buffer-substring-no-properties (point)
				    (1- (search-forward ":")))))

(defun csv-show-hide-constant-columns ()
  "Hides all columns that have constant value."
  (interactive)
  (let (constant-columns)
    (csv-show--in-source-buffer ((constant-columns (csv-show-constant-columns))))
    (message "%d constant columns hidden." (length constant-columns))
    (dolist (column constant-columns)
      (csv-show-set-column-state column 'constant)))
  (csv-show-fontify-detail-buffer))

(defadvice csv-show-hide-constant-columns (around time-csv-show-hide-constant-columns)
  ""
  (interactive)
  (let ((c-s-s (current-time)))
    ad-do-it
    (let ((elapsed (float-time (time-subtract (current-time) c-s-s))))
      (message "Hiding constant columns took %.3fs" elapsed))))
(ad-activate 'csv-show-hide-constant-columns)

(defun csv-show-diff-values (list)
  (let ((first-value (first list))
	result)
    (dolist (element (rest list))
      (push (- element first-value) result)
      (setq first-value element))
    (nreverse result)))

(ert-deftest csv-show-diff-values-test ()
  (should (equal '(1 1 1) (csv-show-diff-values '(10 11 12 13)))))

(defun csv-show-spark-line-toggle-incremental ()
  "Toggle between using diff's of values for the sparkle lines"
  (interactive)
  (setq csv-show-spark-line-incremental (not csv-show-spark-line-incremental))
  (csv-show-fill-buffer))

(defun csv-show-spark-line-for-all-visible-columns ()
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (forward-line)
    (forward-line)
    (while (thing-at-point 'symbol)
      (csv-show-spark-line))))

(defun csv-show-spark-line ()
  (interactive)
  (let ((column (csv-show-column-name))
	(result (list)))
    (message (concat "Spark line for " column ))
    (csv-show--in-source-buffer
     nil
     (let ((key-index csv-show-key-column-field-index)
	   (value-index (csv-show--field-index-for-column column))
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
       
       (flet ((value-to-plot (line-values)
			     (nth value--index line-values))
	      (key-value (line-values)
			 (when key-index (nth key--index line-values))))

	 (setq key-value (key-value (csv-show--get-cells-fast indices)))

	 (goto-char (point-min))
	 (while (and (forward-line)
		     (not (eobp)))
	   
	   (let* ((line-values (csv-show--get-cells-fast indices))
		  (value (value-to-plot line-values))
		  (key (key-value line-values)))
	     
	     (when (and value (equal key key-value))
	       (let ((value (string-to-number value)))
		 (when value
		   (push value result)))))))))
    
    (setq result (nreverse result))
    (when csv-show-spark-line-incremental
      (setq result (csv-show-diff-values result)))
    (csv-show-set-column-state column (wo-make-spark-line 80 11 result))
    (csv-show-fontify-detail-buffer)
    (next-line)))

(defun csv-show-set-key-column ()
  "Will mark the column as Key column"
  (interactive)
  (let ((column (csv-show-column-name)))
    (csv-show--in-source-buffer 
     nil 
     (setq csv-show-key-column-name column)
     (set-key-column-field-index))))

(defun csv-show-hide-column ()
  "Will mark the column on the current row for hiding. 
Depending on the `column-state-toggle' it will either immediate hide
the column, or it will mark it visibly as hidden.

If used on an already hidden column (displayed with the highlight),
unhide the column.

See also `csv-show-column-state-toggle'"
  (interactive)
  (let ((column (csv-show-column-name)))
    (case (csv-show-column-state column)
      ('hidden (csv-show-set-column-state column 'normal))
      (t (csv-show-set-column-state column 'hidden))))
  (csv-show-fontify-detail-buffer)
  (when csv-show-column-state-toggle
    (next-line)))

(defun csv-show-normal-column ()
  "Remove all state: bold, hidden, sparkline etc. from the current column"
  (interactive)
  (csv-show-set-column-state (csv-show-column-name) 'normal)
  (csv-show-fontify-detail-buffer)
  (next-line))

(defun csv-show-normal-all ()
  "Remove all states from all columns"
  (interactive)
  (setq csv-show-column-state nil)
  (csv-show-fontify-detail-buffer))

(defun csv-show-bold-column ()
  "Will mark the column on the current row for bolding. 

See also `csv-show-column-state-toggle'"
  (interactive)
  (let ((column (csv-show-column-name)))
    (case (csv-show-column-state column)
      ('bold (csv-show-set-column-state column 'normal))
      (t (csv-show-set-column-state column 'bold))))
  (csv-show-fontify-detail-buffer)
  (next-line))

(defun csv-show-column-state-toggle ()
  "Toggles between showing all columns and hiding the columns that
are marked for hiding.  See also `csv-show-hide-column'"
  (interactive)
  (setq csv-show-column-state-toggle (not csv-show-column-state-toggle))
  (csv-show-fontify-detail-buffer))

(defun csv-show-format-toggle ()
  "Toggles between showing raw values and their formatted counterparts."
  (interactive)
  (setq csv-show-format-toggle (not csv-show-format-toggle))
  (csv-show-fill-buffer))

(defun csv-show--insert-cell ( column cell )
  ""
  (if csv-show-format-toggle
      (insert (funcall (csv-show--format-function-for-column column) cell))
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
  "Converts SMIS-TIME to a time."
  (date-to-time (smis-time-to-time-string smis-time)))

(defun float-smis-time ( smis-time )
  "Returns a float representing the epoch for SMIS-TIME."
  (float-time (parse-smis-time-string smis-time)))

(defun diff-smis-times ( smis-time1 smis-time2 )
  "Returns the difference in seconds of SMIS-TIME1 - SMIS-TIME2."
  (- (float-smis-time smis-time1) (float-smis-time smis-time2)))

(defun seconds-to-string (seconds)
  "Converts SECONDS to a nicely formatted string with hours, minutes and seconds."
  (let (result)
    (dolist (divider (list (cons 3600 nil) (cons 60 ":") (cons 1 "'")))
      (let ((amount (truncate (/ seconds (car divider)))))
          (setq result (concat result (cdr divider) (format "%02d" amount))
                seconds (- seconds (* amount (car divider))))))
    result))

(defun csv-show--diff-statistictime ( time1 time2 )
  "Returns a nice string representation of TIME1 - TIME2."
  (seconds-to-string (diff-smis-times time1 time2)))

(defun csv-show--make-sure-string-doesnt-start-with ( prefix s )
  "Remove as much instances of PREFIX from the start of S so that it doesn't start with PREFIX anymore.
However, if S has a length greater than 0 to begin with, it never leaves S at length 0."
  (if (= (length prefix) 0)
      s
    (progn
      (while (and (s-starts-with? prefix s)
                  (> (length s) (length prefix)))
        (setq s (substring s (length prefix))))
      s)))

(ert-deftest csv-show--make-sure-string-doesnt-start-with-test ()
  (should (equal (csv-show--make-sure-string-doesnt-start-with "0" "00000123") "123"))
  (should (equal (csv-show--make-sure-string-doesnt-start-with "0" "") ""))
  (should (equal (csv-show--make-sure-string-doesnt-start-with "" "00000123") "00000123"))
  (should (equal (csv-show--make-sure-string-doesnt-start-with "0" "00000000") "0"))
  (should (equal (csv-show--make-sure-string-doesnt-start-with " " "       ") " ")))

(defun csv-show--diff-number (num1 num2)
  "Given two strings num1 and num2 containing arbitrary numbers,
returns a string representing the difference.
Think of it as num1 - num2."
  (let ((num1 (math-read-number num1))
	(num2 (math-read-number num2)))
    (if (and num1 num2)
	(math-format-number (math-sub num1 num2))
      "")))

;; (defun csv-show--diff-integer (int1 int2)
;;   "Given two strings INT1 and INT2 which contain arbitrarily
;; large integers, returns a string representing the difference.
;; Think of it as INT1 - INT2."
;;   (let ((diff (math-sub-bignum (math-read-bignum int1) (math-read-bignum int2)))
;;         (result-is-positive t))
;;     (when (equal diff 'neg)
;;       (setq diff (math-sub-bignum (math-read-bignum int2) (math-read-bignum int1))
;;             result-is-positive nil))
;;     (let ((diff-string (csv-show--make-sure-string-doesnt-start-with "0" (math-format-bignum diff))))
;;       (when (not result-is-positive)
;;         (setq diff-string (concat "-" diff-string)))
;;       diff-string)))

;; (ert-deftest csv-show--diff-integer-test ()
;;   (should (equal (csv-show--diff-integer "5" "3") "2"))
;;   (should (equal (csv-show--diff-integer "55555555555555555555" "33333333333333333333") "22222222222222222222"))
;;   (should (equal (csv-show--diff-integer "3" "5") "-2")))

(ert-deftest csv-show--diff-number-test ()
  (should (equal (csv-show--diff-number "5" "3") "2"))
  (should (equal (csv-show--diff-number "55555555555555555555" "33333333333333333333") "22222222222222222222"))
  (should (equal (csv-show--diff-number "3" "5") "-2"))
  (should (equal (csv-show--diff-number "0.936340455076744" "0.920434747227233") "0.01590570785"))
  (should (equal (csv-show--diff-number "0sdfsaf44" "0.920434747227233") "")))

(defun csv-show--diff-cells ( column cell1 cell2 )
  "Given CELL1 and CELL2 and their COLUMN, returns an appropriate diff between them. Think of it as CELL1 - CELL2."
  (cond ((member column '("InstanceID" "ElementType"))
                 nil)
         ((equal column "StatisticTime")
          (csv-show--diff-statistictime cell1 cell2))
         (t (csv-show--diff-number cell1 cell2))))

(defun csv-show--line-col-position ()
  "Returns the position of point as a line number, column number combination"
  (cons (line-number-at-pos) (current-column)))

(defun csv-show--restore-line-col-position (line-col)
  "Move point to the `line-col' position, see `csv-show--line-col-position'"
  (goto-char (point-min))
  (forward-line (1- (car line-col)))
  (move-to-column (cdr line-col)))

(defun csv-show--fill-line (column width cell cell-width previous-cell)
  (insert column ":")
  (move-to-column (+ 4 width) t)
  (csv-show--insert-cell column cell)
  (move-to-column (+ 4 width cell-width 1) t)
  (when previous-cell
    (csv-show--insert-cell column previous-cell)
    (let ((diff (csv-show--diff-cells column cell previous-cell)))
      (move-to-column (+ 4 width cell-width 1 cell-width 1) t)
      (when diff
	(insert diff)
	(move-to-column (+ 4 width cell-width 1 cell-width 1 cell-width 1) t))))
  (insert " \n"))

; TODO: Make LINE: a field
(defun csv-show-fill-buffer ()
  "Fills the buffer with the content of the cells."
    (let ((current-position (csv-show--line-col-position))
	  (buffer-read-only nil))
      (erase-buffer)

      (insert "FILE: " (buffer-name (marker-buffer csv-show-source-marker))
	      " LINE: " (format "%d" csv-show-source-line-no) 
	      " Spark Lines use " (if csv-show-spark-line-incremental
				      "DIFFs" "Values") 
	      " for plotting"
	      "\n\n")
      
      (let ((width (reduce 'max csv-show-columns :key 'length))
            (cell-width (reduce 'max csv-show-cells :key 'length)))
	(if csv-show-previous-cells
	    (cl-mapcar (lambda (column cell previous-cell)
			 (csv-show--fill-line column width cell cell-width previous-cell))
		       csv-show-columns csv-show-cells csv-show-previous-cells)
	  (cl-mapcar (lambda (column cell)
		       (csv-show--fill-line column width cell cell-width nil))
		     csv-show-columns csv-show-cells)))
      (csv-show-fontify-detail-buffer)
      (csv-show--restore-line-col-position current-position)))

(defun csv-show-fontify-detail-buffer ()
  "Fontifies the detail buffer, assumes that the detail buffer is current buffer."
  (save-excursion
    (let ((buffer-read-only nil))
      (goto-char (point-min))
      (set-text-properties (point) (point-max) nil)
      (forward-line 2)
      (while (not (eobp))
	(let ((column (csv-show-column-name))
	      (start (point)))
	  (case (csv-show-column-state column)
	    ((hidden constant)
	     (forward-line)
	     (if csv-show-column-state-toggle
		 (put-text-property start (point) 'face 'highlight)
	       (put-text-property start (point) 'invisible t)))
	    (bold
	     (forward-line)
	     (put-text-property start (point) 'face '(:weight bold)))
	    (t
	     (put-text-property start (search-forward ":") 'face 'font-lock-keyword-face)
	     (when (and (consp (csv-show-column-state column)))
	       (end-of-line)
	       (put-text-property (- (point) 1) (point) 'display (csv-show-column-state column)))
	     (forward-line))))))))

(defun csv-show--mark-forward/backward (dir &optional do-not-parse-headers)
  "Move the selection to the next or previous record.
Note that this does not update the content of the buffer,
it will parse the column, cells and put these into the
corresponding local variables of the CSV-Detail buffer.

Also move the mark down or up and update the line-no
variable.

For updating the content see the function `csv-show-fill-buffer'."
  (let (new-show-columns)
    (setq csv-show-previous-cells nil)
    (csv-show--in-source-buffer
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

(defun csv-show-next ()
  "Shows the next record of the underlying CSV file."
  (interactive)
  (csv-show-next/prev 1))

(defun csv-show-prev ()
  "Shows the previous record of the underlying CSV file."
  (interactive)
  (csv-show-next/prev -1))

(defun csv-show--get-current-value-for-index (index)
  "Returns the value of the INDEXth item on the current line. Returns nil when index not given."
  (when index
    (beginning-of-line)
    (car (csv-show-parse-line (list index)))))

(defun csv-show-next/prev-statistictime (&optional dir)
  "Shows the next or previous record for which the StatisticTime
field is different than the current, and InstanceID is
identical."
  (interactive)
  (setq csv-show-previous-cells csv-show-cells)
  (csv-show--in-source-buffer
		   ((csv-show-source-marker (point-marker))
		    (csv-show-source-line-no (line-number-at-pos (point)))
		    (csv-show-cells (csv-show--get-cells)))
		   
		   (csv-show--next/prev-value "StatisticTime" (or dir 1)))
  (csv-show-fill-buffer))


(defun csv-show-next/prev-value (&optional dir)
  "Shows the next or previous record for which the value of
the current column is different than the current value for the current
column, and InstanceID is
identical."
  (interactive)
  (setq csv-show-previous-cells csv-show-cells)
  (let ((variable-column (csv-show-column-name)))
    (csv-show--in-source-buffer
     ((csv-show-source-marker (point-marker))
      (csv-show-source-line-no (line-number-at-pos (point)))
      (csv-show-cells (csv-show--get-cells)))
     (csv-show--next/prev-value variable-column (or dir 1))))
  (csv-show-fill-buffer))

(defun csv-show--jump-first-line-for-key-value ( key-value )
  "Expected to be performed in the source buffer."
  (goto-char (point-min))
  (while (and (forward-line)
              (not (eobp))
              (not (equal 
                    key-value
                    (car (csv-show-parse-line (list csv-show-key-column-field-index))))))))
  
(defun csv-show-jump-first-line-for-key-value ()
  "Expected to be performed in the detail buffer. Jumps to the first line in the
source file that has the same value for csv-show-key-column as the current line."
  (interactive)
  (setq csv-show-previous-cells nil)
  (let (key-index indices)
    (csv-show--in-source-buffer ((key-index csv-show-key-column-field-index)
                                 (indices (csv-show--indices-of-columns))))
    (let ((key-value (nth key-index csv-show-cells)))
      (csv-show--in-source-buffer ((csv-show-source-marker (point-marker))
                                   (csv-show-source-line-no (line-number-at-pos (point)))
                                   (csv-show-cells (csv-show--get-cells-fast indices)))
                                  (csv-show--jump-first-line-for-key-value key-value))))
  (csv-show-fill-buffer))

(defun csv-show--all-key-values ()
  "Return a list of all values for the csv-show-key-column"
  (let ((key-values (list)))
    (csv-show--in-source-buffer nil
     (goto-char (point-min))
     (while (and (forward-line)
                 (not (eobp)))
       (let ((current-key-value (car (csv-show-parse-line (list csv-show-key-column-field-index)))))
         (add-to-list 'key-values current-key-value t))))
    key-values))

(defun csv-show-next-value ()
  "Show next record for which the current field is different, see `csv-show-next/prev-value'"
  (interactive)
  (csv-show-next/prev-value 1))

(defun csv-show-prev-value ()
  "Show previous record for which the current field is different, see `csv-show-next/prev-value'"
  (interactive)
  (csv-show-next/prev-value -1))

(defun csv-show--next/prev-value (column dir)
  "Moves up or down in the CSV file (current buffer) until a line is encountered 
with a different value for column but the same instance id. Returns t if such a
line was found, nil otherwise.

Pre conditions are:  
 - point is at the beginning of a line.

Post conditions:
 - point is at the beginning of the new line.
"
  (let* ((csv-show--get-columns-cache (csv-show--get-columns)) 
	 (variable-column-index (csv-show--field-index-for-column column))
	 (instanceid-index (csv-show--field-index-for-column "InstanceID"))
	 (current-value (csv-show--get-current-value-for-index variable-column-index))
	 (current-instanceid (csv-show--get-current-value-for-index instanceid-index))
         (found t))
    (while (or
	    (not (equal current-instanceid (csv-show--get-current-value-for-index instanceid-index)))
	    (equal current-value (csv-show--get-current-value-for-index variable-column-index)))
      (beginning-of-line)
      (unless (equal (forward-line dir) 0)
	(error "No more records")
        (setq found nil)))
    (beginning-of-line)
    found))

(defun csv-show--indices-of-columns()
  "Returns a list of indices of all the columns."
  (let ((i 0) 
        column-indices)
      (dolist (c (csv-show--get-columns))
        (push i column-indices)
        (setq i (+ i 1)))
      (nreverse column-indices)))

(defun csv-show--key-value-from-column-indices-and-values (column-indices values)
  "Given a list of COLUMN-INDICES and a corresponding list of VALUES, returns the value
   corresponding to csv-show-key-column-field-index."
  (let (key-value)
    (while (and (not key-value)
                column-indices)
      (let ((column-index (pop column-indices))
            (value (pop values)))
        (when (equal column-index csv-show-key-column-field-index)
          (setq key-value value))))
    key-value))


(defun csv-show--constant-columns (candidate-constant-columns key-column-index current-values previous-values)
  "Return a list of indices for which `current-values' and `previous-values' are equal 
and which occur in `candidate-constant-columns'.  Also note that
the values at `key-column-index' are always considered equal, even if
they are not (they are not compared).  So `key-column-index' is
never removed from the result.

The order of the indices in the result is the same as the order in
input `candidate-constant-columns'."
  (let ((constant-columns)
	(safe-length (min (length previous-values) (length current-values))))
    (dolist (current-column-index candidate-constant-columns)
      
      (when (or (equal current-column-index key-column-index)
		(and (< current-column-index safe-length)
		     (equal (aref previous-values current-column-index)
			    (aref current-values current-column-index))))
	(push current-column-index constant-columns)))

    (nreverse constant-columns)))


(defun csv-show-constant-columns ()
  "Analyzes a csv buffer and returns a list of the column names that contain constant values."
  (interactive)
  (let* ((constant-columns-indices (csv-show--indices-of-columns))
         (all-columns-indices constant-columns-indices)
         (line-number 0)
         (number-of-lines (count-lines (point-min) (point-max)))
         previous-cells)

    (goto-char (point-min))

    (while (and (cdr constant-columns-indices)
		(forward-line) 
		(not (eobp)))
      ;; Progress Reporting
      (incf line-number)
      (when (= (% line-number 1000) 0)
	(message "%d%%: %d possible constant columns left." 
		 (/ (* line-number 100) number-of-lines) 
		 (- (length constant-columns-indices) 1)))

      ;; Processing new line
      (let* ((current-values (csv-show--get-cells-vec all-columns-indices))
	     (key (aref current-values csv-show-key-column-field-index))
	     (previous-assoc (assoc key previous-cells)))
	(if (not previous-assoc)
	    (push (cons key current-values) previous-cells)
	  (setq constant-columns-indices 
		(csv-show--constant-columns constant-columns-indices 
					    csv-show-key-column-field-index
					    current-values 
					    (cdr previous-assoc)))
	  (setcdr previous-assoc current-values)))) 

    ;; Remove key column from constant list
    (setq constant-columns-indices (delete csv-show-key-column-field-index 
					   constant-columns-indices))
    
    (goto-char (point-min))
    (when constant-columns-indices
        (csv-show--get-cells constant-columns-indices))))

(defun csv-show-switch-to-source-buffer ()
  "Switch to the source line in the underlying CSV file."
  (interactive)
  (let ((line-no csv-show-source-line-no))
    (pop-to-buffer (marker-buffer csv-show-source-marker))
    (goto-line line-no)))
  
(provide 'csv-show)
;;; csv-show.el ends here
