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
(require 's)
(require 'dash)

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
  nil " csv-show" csv-show-map
  (lambda()
    (make-local-variable 'csv-show-key-column-name)
    (make-local-variable 'csv-show-key-column-field-index)
    (setq csv-show-key-column-name "InstanceID") ;Holds the name of the column that is used as key column.
    (setq csv-show-key-column-field-index nil)  ;Holds the field index of the column that is used as key column.
   ))


(setq csv-show-detail-map 
      (let ((map (make-sparse-keymap)))
	(set-keymap-parent map special-mode-map)
	(define-key map "n" (lambda () (interactive) (csv-show-next/prev 1)))
	(define-key map "N" (lambda () (interactive) (csv-show-next/prev-statistictime 1)))
	(define-key map "." (lambda () (interactive) (csv-show-current)))
	(define-key map [?\C-.] 'csv-show-toggle-timer)
	(define-key map "p" (lambda () (interactive) (csv-show-next/prev -1)))
	(define-key map "P" (lambda () (interactive) (csv-show-next/prev-statistictime -1)))
	(define-key map "h" 'csv-show-hide-column)
        (define-key map "c" 'csv-show-hide-constant-columns)
	(define-key map "b" 'csv-show-bold-column)
	(define-key map "s" 'csv-show-column-state-toggle)
        (define-key map "o" 'csv-show-switch-to-source-buffer)
        (define-key map "j" (lambda () (interactive) (csv-show-next/prev-value -1)))
        (define-key map "k" (lambda () (interactive) (csv-show-next/prev-value 1)))
        (define-key map [C-return] 'csv-show-switch-to-source-buffer)
        (define-key map "f" 'csv-show-format-toggle)
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
  (setq-local csv-show-column-state (list))
  (setq-local csv-show-column-state-toggle nil)
  (setq-local csv-show-format-toggle t))

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

(defun csv-show-parse-line-dumb-and-fast (&optional indices)
  "Dumb csv-show-parse-line that is fast but not always correct."
  (let ((cells (split-string (buffer-substring-no-properties (progn (end-of-line 1) (point)) (progn (beginning-of-line 1) (point))) ",")))
    (if indices
      (let ((indexed-cells))
        (dolist (index (reverse indices))
          (push (nth index cells) indexed-cells))
        indexed-cells)
      cells)))

(defun csv-show-parse-line-vec ()
  "Dumb csv-show-parse-line that is fast but not always correct."
  (vconcat (mapcar 's-trim (split-string (buffer-substring-no-properties (progn (end-of-line 1) (point)) (progn (beginning-of-line 1) (point))) ","))))

(defun csv-show-test-parse-speed-for-function (parse-function &optional indices)
  ""
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (and (forward-line) (not (eobp)))
      (funcall parse-function indices))))

(defun csv-show-test-parse-speeds (&optional indices)
  ""
  (interactive)
  (let (result)
    (dolist (parse-function (list 'csv-show-parse-line 'csv-show-parse-line-dumb-and-fast 'csv-show-parse-line-vec))
      (push (cons parse-function (benchmark-run-compiled 1 (csv-show-test-parse-speed-for-function parse-function indices))) result))
    result))

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

(defun csv-show--get-cells-alt ()
  "Returns an assoc list of index -> cell"
  (save-excursion
    (csv-show-parse-line-vec)))

(defun csv-show--get-cells-vec ()
  "Returns an assoc list of index -> cell"
  (save-excursion
    (csv-show-parse-line-vec)))

(defun csv-show-select ()
  "Show the current row."
  (interactive)
  (let ((current-buffer-v (current-buffer))
	(start (point-marker)))
    (pop-to-buffer (get-buffer-create (concat "*CSV Detail " (buffer-file-name current-buffer-v) "*" )))
    (csv-show-detail-mode)
    (setq csv-show-source-marker start)
    (csv-show-current)
    ))

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
  (push (cons column state) csv-show-column-state))


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
    (buffer-substring-no-properties (point)
				    (1- (search-forward ":")))))

(defun csv-show-hide-constant-columns ()
  "Hides all columns that have constant value."
  (interactive)
  (let (constant-columns)
    (in-other-buffer csv-show-source-marker ((constant-columns (csv-show-constant-columns))))
    (message "%d constant columns hidden." (length constant-columns))
    (dolist (column constant-columns)
      (csv-show-set-column-state column 'constant)))
  (csv-show-fontify-detail-buffer))

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
  (forward-line)
  (csv-show-fontify-detail-buffer))

(defun csv-show-bold-column ()
  "Will mark the column on the current row for bolding. 

See also `csv-show-column-state-toggle'"
  (interactive)
  (let ((column (csv-show-column-name)))
    (case (csv-show-column-state column)
      ('bold (csv-show-set-column-state column 'normal))
      (t (csv-show-set-column-state column 'bold))))
  (forward-line)
  (csv-show-fontify-detail-buffer))

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

(defun csv-show-fill-buffer ()
  "Fills the buffer with the content of the cells."
    (let ((line-no (line-number-at-pos))
	  (column-no (current-column))
	  (buffer-read-only nil))
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
	    (insert (concat  column ":"))
	    (move-to-column (+ 4 width) t)
            (if csv-show-format-toggle
                (insert (funcall (csv-show--format-function-for-column column) cell) "\n")
              (insert cell "\n" )))))
      (csv-show-fontify-detail-buffer)
      (goto-char (point-min))
      (forward-line (1- line-no))
      (move-to-column column-no)))

(defun csv-show-fontify-detail-buffer ()
  "Fontifies the detail buffer, assumes that the detail buffer is current buffer."
  (save-excursion
    (let ((buffer-read-only nil))
      (goto-char (point-min))
      (put-text-property (point) (point-max) 'invisible nil)
      (put-text-property (point) (point-max) 'face nil)
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
    (beginning-of-line)
    (car (csv-show-parse-line (list index)))))

(defun csv-show-next/prev-statistictime (&optional dir)
  "Shows the next or previous record for which the StatisticTime
field is different than the current, and InstanceID is
identical."
  (interactive)
  (in-other-buffer csv-show-source-marker 
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
  (let ((variable-column (csv-show-column-name)))
    (in-other-buffer csv-show-source-marker 
                     ((csv-show-source-marker (point-marker))
                      (csv-show-source-line-no (line-number-at-pos (point)))
                      (csv-show-cells (csv-show--get-cells)))
		   
                     (csv-show--next/prev-value variable-column (or dir 1)))
    )
  (csv-show-fill-buffer))

(defun csv-show--next/prev-value (column dir)
  "Moves up or down in the CSV file (current buffer) until a line is encountered 
with a different value for column but the same instance id.

Pre conditions are:  
 - point is at the beginning of a line.

Post conditions:
 - point is at the beginning of the new line.
"
  (let* ((csv-show--get-columns-cache (csv-show--get-columns)) 
	 (variable-column-index (csv-show--field-index-for-column column))
	 (instanceid-index (csv-show--field-index-for-column "InstanceID"))
	 (current-value (csv-show--get-current-value-for-index variable-column-index))
	 (current-instanceid (csv-show--get-current-value-for-index instanceid-index)))
    (while (or
	    (not (equal current-instanceid (csv-show--get-current-value-for-index instanceid-index)))
	    (equal current-value (csv-show--get-current-value-for-index variable-column-index)))
      (beginning-of-line)
      (unless (equal (forward-line dir) 0)
	(error "No more records")))
    (beginning-of-line)))

(defun csv-show--indices-of-columns()
  "Returns a list of indices of all the columns."
  (let ((i 0) 
        column-indices)
      (dolist (c (csv-show--get-columns))
        (push i column-indices)
        (setq i (+ i 1)))
      (reverse column-indices)))

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

(defun setup-key-column-vars ()
  ""
  (setq-local csv-show-key-column-name "InstanceID")
  (setq-local csv-show-key-column-field-index nil)
  )

(defun set-key-column-field-index ()
  ""
  (when (not csv-show-key-column-field-index)
    (setq csv-show-key-column-field-index (csv-show--field-index-for-column csv-show-key-column-name))))

(defun csv-show-constant-columns()
  "Analyzes a csv buffer and returns a list of the column names that contain constant values."
  (interactive)
  (setup-key-column-vars)
  (let ((constant-columns-indices (csv-show--indices-of-columns))
        previous-cells)
    (set-key-column-field-index)
    (goto-char (point-min))
    (let ((line-number 0)
          (number-of-lines (count-lines (point-min) (point-max))))
      (while (and (forward-line) (not (eobp)) (setq line-number (+ line-number 1)))
        (when (= (% line-number 1000) 0)
          (message "%d%%: %d possible constant columns left." (/ (* line-number 100) number-of-lines) (- (length constant-columns-indices) 1)))
        (let* ((current-values (csv-show--get-cells-vec))
               (key (aref current-values csv-show-key-column-field-index))
               (previous-assoc (assoc key previous-cells)))
          (if (not previous-assoc)
              (push (cons key current-values) previous-cells)
            (let ((previous-values (cdr previous-assoc)))
              (dolist (current-column-index constant-columns-indices)
                (when (not (equal current-column-index csv-show-key-column-field-index)) ; The key column might be variable, but we don't want to lose it
                  (let ((current-value (aref current-values current-column-index))
                        (previous-value (aref previous-values current-column-index)))
                    (when (not (equal previous-value current-value)) ; We don't want to lose a column that didn't change on this line
                      ;; (message "Removed column %s from constant column list because %s is not equal to %s for key value %s at line %d." (nth current-column-index columns) previous-value current-value key line-number)
                      (setq constant-columns-indices (delete current-column-index constant-columns-indices))
                      ;; (message "Finding constant columns: %d possible constant columns left." (- (length constant-columns-indices) 1))
                      )))))
            (setf (cdr previous-assoc) current-values))))) ; Set the current constant column indices and values as new previous value for this key
                                        ; The key column might be constant, but we don't want to ignore it ever
    (setq constant-columns-indices (delete csv-show-key-column-field-index constant-columns-indices))
    (goto-char (point-min))
                                        ; csv-show--get-cells returns all cells when given an empty parameter, we don't want that
    (if constant-columns-indices
        (csv-show--get-cells constant-columns-indices)
      (list))
    ))

(defun csv-show-switch-to-source-buffer ()
  "When in detail buffer switch to its source buffer"
  (interactive)
  (let ((line-no csv-show-source-line-no))
    (pop-to-buffer (marker-buffer csv-show-source-marker))
    (goto-line line-no)))
  
(provide 'csv-show)
;;; csv-show.el ends here
