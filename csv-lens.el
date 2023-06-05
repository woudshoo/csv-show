;;; csv-lens.el --- navigate and edit CSV files

;; Copyright (C) 2006  Alex Schroeder <alex@gnu.org>
;; Copyright (C) 2013  Tom Koelman
;; Copyright (C) 2013, 2015  Willem Rein Oudshoorn <woudshoo@xs4all.nl>
;;
;; Author: Alex Shroeder <alex@gnu.org>
;;     Tom Koelman
;;     Willem Rein Oudshoorn <woudshoo@xs4all.nl>
;; Maintainer: ???
;; Created: 2013
;; Version: 0.1
;; Keywords: data
;; Homepage: http://github.com/woudshoo/csv-show
;; Package-Requires: ((emacs "24")
;;                    (cl-lib "1.0")
;;                    (dash "1.5.0")
;;                    (ht "1.3")
;;                    (sparkline "0.3"))
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

;;; History:

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'ht)
(require 'calc)
(require 'simple)
(require 'csv-lens-sparkline)
(require 'csv-lens-column)

(defvar csv-lens-source-line-no nil
  "The line nr in the original CSV file.
The data displayed in a Lens buffer is from this line of the original file.")

(defvar csv-lens-source-marker nil
  "The marker which points to the data being shown in the Lens buffer.
This is a buffer local variable which is used in the Lens buffer to
refer to the original CSV file.")

(defvar csv-lens-cells nil
  "The values of the CSV file row being displayed.
In the Lens buffers this contains the data that is being
displayed.")

(defvar csv-lens-previous-line nil
  "If two rows are displayed, the line nr of the secondary data.
See also `csv-lens-source-line-no'.")

(defvar csv-lens-previous-cells nil
  "If two rows are displayed, the values of the secondary data.
See also `csv-lens-cells'.")

(defvar csv-lens-detail-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map "n" 'csv-lens-next)
    (define-key map "N" (lambda () (interactive) (csv-lens-next/prev-record 1)))
    (define-key map "." 'csv-lens-current)
    (define-key map "p" 'csv-lens-prev)
    (define-key map "P" (lambda () (interactive) (csv-lens-next/prev-record -1)))
    (define-key map "h" 'csv-lens-hide-column)
    (define-key map "c" 'csv-lens-hide-constant-columns)
    (define-key map "b" 'csv-lens-bold-column)
					;	(define-key map "u" 'csv-lens-normal-column)
    (define-key map "U" 'csv-lens-normal-all)
    (define-key map "s" 'csv-lens-column-ignore-state-toggle)
    (define-key map "S" 'csv-lens-spark-line)
    (define-key map "Z" 'csv-lens-spark-line-for-all-visible-columns)
    (define-key map "I" 'csv-lens-spark-line-toggle-incremental)
    (define-key map "o" 'csv-lens-switch-to-source-buffer)
    (define-key map "j" 'csv-lens-next-value)
    (define-key map "k" 'csv-lens-prev-value)
    (define-key map "K" 'csv-lens-toggle-key-column)
    (define-key map "Q" 'csv-lens-kill-detail-buffer)
    (define-key map [C-return] 'csv-lens-switch-to-source-buffer)
    (define-key map "f" 'csv-lens-format-toggle)
    (define-key map "<" 'csv-lens-jump-first-line-for-key-value)
    (define-key map ">" 'csv-lens-jump-last-line-for-key-value)
    map)
  "The keymap of the Lens buffers.")


(defvar csv-lens-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\"  "\"" table)
    (modify-syntax-entry ?,  "." table)
    table)
  "Syntax table used for parsing CSV rows.")

(defvar csv-lens-column-state-toggle nil
  "Determines if all columns are shown, regardless of :hidden state.
If t, show all values, if nil hide the :hidden values.")

(defvar csv-lens-format-toggle nil
  "Determines if the raw values are shown, or the formatted values.
If t format the values, if nil show the raw valus.")

(defvar csv-lens-configuration-name nil
  "The name of the current used configuration.")

(defvar csv-lens-columns nil
  "A list containing the column names in order.")


(defvar csv-lens--get-columns-cache nil
  "The column names in the source buffer.
This should be used to cache a frequently called operation to get
the column names.  However, it is currently not used.")

(defvar csv-lens-map
  (let ((map (make-sparse-keymap)))
    (define-key map [C-return] 'csv-lens-select)
    map)
  "The keymap for the csv-lens minor mode.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Macros to transition between buffers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro in-other-buffer (marker bindings &rest body)
  "Execute BODY in the buffer indicated by MARKER.  
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
  "Get a marker for the source buffer location which is used in the Lens buffer.
If the current buffer is not a detail buffer
it should be a CSV file and it will return (point-marker)."
  (if (boundp 'csv-lens-source-marker)
      csv-lens-source-marker
    (point-marker)))

(defmacro csv-lens--in-source-buffer (bindings &rest body)
  "Execute BODY in the source buffer.
See `in-other-buffer'."
  (declare (indent 1))
  `(in-other-buffer (csv-lens--marker-for-source-buffer) ,bindings ,@body))

(defmacro csv-lens--in-source-buffer-and-update (additional-bindings &rest body)
  "Execute BODY in the source buffer and afterwards update the state."
  (declare (indent 1))
  `(csv-lens--in-source-buffer ,(append 
				 '((csv-lens-source-marker (point-marker))
				   (csv-lens-source-line-no (line-number-at-pos (point)))
				   (csv-lens-cells (csv-lens--get-cells)))
				 additional-bindings)
     ,@body
     (beginning-of-line)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Code that assumes the original CSV buffer as current buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(define-minor-mode csv-lens-mode 
  "Shows a row in a CSV file in a separate buffer.

This is a minor mode to show in a separate buffer the content
of the current line as a table.

\\{csv-show-map}"
  nil " csv-lens" csv-lens-map) ;; FIXME


(easy-menu-define csv-lens-menu csv-lens-detail-map
  "Menu for CSV Lens buffers."
  '("CSV Lens"
    ["Next Record" csv-lens-next]
    ["Previous Record" csv-lens-prev]
    ["Refresh Record" csv-lens-current]
    ["Next Different Value" csv-lens-next-value]
    ["Previous Different Value" csv-lens-prev-value]
    ["First Record same Key" csv-lens-jump-first-line-for-key-value]
    ["Last Record same Key" csv-lens-jump-last-line-for-key-value]
    "----"
    ["Switch to Source" csv-lens-switch-to-source-buffer]
    "----"
    ["Hide Column" csv-lens-hide-column]
    ["Show Hidden Columns" csv-lens-column-ignore-state-toggle]
    ["Bold Column" csv-lens-bold-column]
    ["Key Column" csv-lens-toggle-key-column]
    ["(Un)format Column" csv-lens-format-toggle]
    ["Unmark All" csv-lens-normal-all]
    "----"
    ["Sparkline Column" csv-lens-spark-line]
    ["Sparkline All" csv-lens-spark-line-for-all-visible-columns]
    ["Plot Value/Delta" csv-lens-spark-line-toggle-incremental]))

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
  (make-local-variable 'csv-lens-configuration-name)
  (make-local-variable 'csv-lens-previous-cells)
  (make-local-variable 'csv-lens-previous-line)
  (setq-local csv-lens-column-state-toggle nil)
  (setq-local csv-lens-format-toggle t)
  (setq buffer-read-only t))

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
    (string-trim field)))
    

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
          (pop indices)))) 
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
  "Return the values given by INDICES for the current line.
If indices is nil, return all the values."
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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun csv-lens-buffer-name-for-lens-buffer (csv-buffer)
  "Return a suggested name for the Lens buffer based upon CSV-BUFFER."
  (concat "*CSV Lens " (file-name-nondirectory (directory-file-name (buffer-file-name csv-buffer))) "*" ))

(defun csv-lens-select ()
  "Show the current row."
  (interactive)
  (let ((current-buffer-v (current-buffer))
	(start (point-marker)))
    (pop-to-buffer (get-buffer-create (csv-lens-buffer-name-for-lens-buffer current-buffer-v)))
    (csv-lens-detail-mode)
    (setq csv-lens-source-marker start)
    (csv-lens-current)))


(defun csv-lens-column-name (&optional point)
  "Return the column name for the line containing POINT.
If POINT is nil or not provided, use the current point in the
buffer."
  (save-excursion
    (when point (goto-char point))
    (beginning-of-line)
    (when (get-text-property (point) 'invisible)
      (goto-char (next-single-property-change (point) 'invisible)))
    (let ((start-pos (+ (point) 2))
	  (past-pos (search-forward ":" nil t)))
      (when (and start-pos past-pos)
	(buffer-substring-no-properties start-pos
					(1- past-pos))))))

(defun csv-lens-hide-constant-columns ()
  "Hides all columns that have constant value."
  (interactive)
  (let ((key-indices (csv-lens-column-key-indices))
	constant-columns)
    (csv-lens--in-source-buffer ((constant-columns (csv-lens-constant-columns key-indices))))
    (message "%d constant columns hidden." (length constant-columns))
    (dolist (column csv-lens-columns)
      (csv-lens-set-column-state column :constant nil))
    (dolist (column constant-columns)
      (csv-lens-set-column-state column :constant t)
      (csv-lens-set-column-state column :hidden t)))
  (csv-lens-fontify-detail-buffer))


(defun csv-lens-toggle-key-column ()
  "Will mark the column as Key column."
  (interactive)
  (csv-lens-column-state-toggle (csv-lens-column-name) :key)
  (csv-lens-fontify-detail-buffer))

(defun csv-lens-hide-column ()
  "Will mark the column on the current row for hiding. 
Depending on the `column-state-toggle' it will either immediate hide
the column, or it will mark it visibly as hidden.

If used on an already hidden column (displayed with the highlight),
unhide the column.

See also `csv-lens-column-state-toggle'"
  (interactive)
  (csv-lens-column-state-toggle (csv-lens-column-name) :hidden)
  (csv-lens-fontify-detail-buffer)
  (when csv-lens-column-state-toggle
    (next-line)))


(defun csv-lens-normal-all ()
  "Remove all states from all columns."
  (interactive)
  (setq csv-lens-column-state nil)
  (csv-lens-fontify-detail-buffer))

(defun csv-lens-bold-column ()
  "Will mark the column on the current row for bolding. 

See also `csv-lens-column-state-toggle'"
  (interactive)
  (csv-lens-column-state-toggle (csv-lens-column-name) :bold)
  (csv-lens-fontify-detail-buffer)
  (next-line))

(defun csv-lens-column-ignore-state-toggle ()
  "Toggle between showing or hiding the columns marked for hiding.
See also `csv-lens-hide-column'"
  (interactive)
  (setq csv-lens-column-state-toggle (not csv-lens-column-state-toggle))
  (csv-lens-fontify-detail-buffer))

(defun csv-lens-format-toggle ()
  "Toggle between showing raw values and their formatted counterparts."
  (interactive)
  (setq csv-lens-format-toggle (not csv-lens-format-toggle))
  (csv-lens-fill-buffer))

(defun csv-lens--insert-cell (column cell)
  "Insert COLUMN value CELL into the buffer, optionally formatted."
  (if csv-lens-format-toggle
      (insert (funcall (csv-lens-cell-format-function-for-column column) cell))
    (insert cell)))

(defun csv-lens--diff-cells (column cell1 cell2)
  "Given COLUMN, CELL1 and CELL2, return an appropriate diff.
Think of it as CELL1 - CELL2."

  (funcall (csv-lens-cell-diff-function-for-column column) cell1 cell2))

(defun csv-lens--line-col-position ()
  "Return the position of point as a line number, column number combination."
  (cons (line-number-at-pos) (current-column)))

(defun csv-lens--restore-line-col-position (line-col)
  "Move point to the LINE-COL position, see `csv-lens--line-col-position'."
  (goto-char (point-min))
  (forward-line (1- (car line-col)))
  (move-to-column (cdr line-col)))


(defun csv-lens--insert-column-header-prefix (column)
  "Insert the prefix for the COLUMN at point.
Containing state indication e.g. constant, hidden, marked etc."
  (insert (csv-lens-column-state-indicator column) " "))

(defun csv-lens--insert-column-header (column width)
  "Insert the column header of COLUMN without markup.
The maximum width of all columns is WIDTH."
  (csv-lens--insert-column-header-prefix column)
  (insert column ":")
  (move-to-column (+ 4 width) t))

(defun csv-lens--fill-line (column width cell cell-width previous-cell)
  (csv-lens--insert-column-header column width)
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
	      " Type: " (or csv-lens-configuration-name "<UNKNOWN>")
	      " Spark Lines use " (if csv-lens-spark-line-incremental
				      "DIFFs" "Values") 
	      " for plotting"
	      "\n\n")
      
      (let ((width (cl-reduce 'max csv-lens-columns :key 'length))
            (cell-width (csv-lens-fill-buffer-cell-width))
            (display-columns (cons "Line" csv-lens-columns))
            (display-cells (cons (format "%d" csv-lens-source-line-no) csv-lens-cells)))
	(if csv-lens-previous-cells
            (let ((display-previous-cells (cons (format "%d" csv-lens-previous-line) csv-lens-previous-cells)))
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
	      (start (point))
	      (end (progn (forward-line) (point))))
	  (goto-char start)
	  (delete-char 2)
	  (csv-lens--insert-column-header-prefix column)

	  (progn
	    (goto-char start)
	    (put-text-property start (search-forward ":") 'face 'font-lock-keyword-face))

	  (when (csv-lens-column-state column :hidden)
	    (if csv-lens-column-state-toggle
	       (put-text-property start end 'face 'highlight)
	      (put-text-property start end 'invisible t)))
	  
	  (when (csv-lens-column-state column :bold)
	    (add-face-text-property start end '(:weight bold))
	    (add-face-text-property start end '(:background "yellow")))
	  (when (csv-lens-column-state column :key)
	    (add-face-text-property start end 'underline))
	  (when (csv-lens-column-state column :constant)
	    (add-face-text-property start end 'italic))
	  
	  (when (csv-lens-column-state column 'sparkline)
	    (put-text-property (- end 2) (- end 1) 'display (csv-lens-column-state column 'sparkline)))
	  

	  (goto-char end))))))


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
    (csv-lens--in-source-buffer-and-update
	((new-show-columns (unless do-not-parse-headers (csv-lens--get-columns))))
      (forward-line (or dir 1)))
    (unless do-not-parse-headers
      (csv-lens-column-configure-for-columns new-show-columns))))

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
  "Show the next or previous record.
DIR is the line number offset, +1 advances one line, -1 goes back one line."
  (interactive "p")
  (csv-lens--mark-forward/backward dir t)
  (csv-lens-fill-buffer))

(defun csv-lens-next ()
  "Show the next record of the underlying CSV file."
  (interactive)
  (csv-lens-next/prev 1))

(defun csv-lens-prev ()
  "Show the previous record of the underlying CSV file."
  (interactive)
  (csv-lens-next/prev -1))

(defun csv-lens--get-current-value-for-index (index)
  "Return the value of the INDEXth item on the current line.
Returns nil when index not given."
  (when index
    (beginning-of-line)
    (car (csv-lens-parse-line (list index)))))


(defun csv-lens-next/prev-record (&optional dir)
  "Show the next or previous record with the same key.

It will search forward or backward in the source csv file for a record
with the same values for the key fields as the currently displayed record.

The direction is indicated by DIR.  If DIR is 1 or nil it moves forward
and if DIR is -1 it moves backward."
  (interactive)
  (setq csv-lens-previous-cells csv-lens-cells)
  (setq csv-lens-previous-line csv-lens-source-line-no)
  (let ((key-indices (csv-lens-column-key-indices)))
    (csv-lens--in-source-buffer-and-update nil
      (csv-lens--next/prev-record (or dir 1) key-indices)))
  (csv-lens-fill-buffer))


(defun csv-lens-next/prev-value (&optional dir)
  "Show the next or previous record with a different field value.

If DIR is +1 or nil it searches forward, if DIR is -1 backwards."
  (interactive)
  (setq csv-lens-previous-cells csv-lens-cells)
  (setq csv-lens-previous-line csv-lens-source-line-no)
  (let ((key-indices (csv-lens-column-key-indices))
	(variable-column (csv-lens--field-index-for-column (csv-lens-column-name))))
    (csv-lens--in-source-buffer-and-update nil
     (csv-lens--next/prev-value (or dir 1) key-indices variable-column)))
  (csv-lens-fill-buffer))


(defun csv-lens--jump-first/last-line-for-key-value (first-or-last key-indices key-values)
  "Go to the FIRST-OR-LAST line with values at KEY-INDICES equal to KEY-VALUES.
This function should be called in the CSV buffer.
FIRST-OR-LAST should be either 'first or 'last."
  (let ((progress-dir (if (equal first-or-last 'first) 1 -1)))
    (if (equal first-or-last 'first)
	(goto-char (point-min))
      (goto-char (point-max)))
    (while (and (equal (forward-line progress-dir) 0)
		(not (eobp))
		(not (equal 
		      key-values
		      (csv-lens-parse-line key-indices)))))
    (beginning-of-line)))
  

(defun csv-lens-jump-first/last-line-for-key-value (first-or-last)
  "Jump to the FIRST-OR-LAST record with the same keys as the current line.
Expected to be performed in the detail buffer.  When FIRST-LAST
is 'first, jumps to the first, when FIRST-LAST is 'last, jumps to
the last."
  (interactive)
  (setq csv-lens-previous-cells nil)
  (setq csv-lens-previous-line nil)
  (let* ((key-indices (csv-lens-column-key-indices)))
    (csv-lens--in-source-buffer-and-update nil
      (csv-lens--jump-first/last-line-for-key-value first-or-last 
						    key-indices 
						    (csv-lens--get-cells key-indices))))
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


(defun csv-lens-next-value ()
  "Show next record for which the current field is different.
See `csv-lens-next/prev-value'"
  (interactive)
  (csv-lens-next/prev-value 1))

(defun csv-lens-prev-value ()
  "Show previous record for which the current field is different.
See `csv-lens-next/prev-value'"
  (interactive)
  (csv-lens-next/prev-value -1))

(defun csv-lens--next/prev-value (dir key-indices column-index)
  "Moves up or down in the CSV file (current buffer) until a line is encountered 
with a different value for column but the same instance id.

Pre conditions are:  
 - point is at the beginning of a line.

Post conditions:
 - point is at the beginning of the new line."
  (let* ((current-value (csv-lens--get-current-value-for-index column-index)))
    (beginning-of-line)
    (while (and (csv-lens--next/prev-record dir key-indices)
		(equal current-value (csv-lens--get-current-value-for-index column-index)))
      (beginning-of-line))
    (beginning-of-line)))

(defun csv-lens--next/prev-record (dir key-indices)
  "Moves up or down in the CSV file (current buffer) until a line is encountered 
with the same value for the key columns.

Pre conditions are:  
 - point is at the beginning of a line.

Post conditions:
 - point is at the beginning of the new line."
  (let* ((current-key-values (and key-indices (csv-lens--get-cells key-indices)))
         (found t))

    (unless (equal (forward-line dir) 0)
      (error "No more records")
      (setq found nil))
    (while (not (equal current-key-values (and key-indices (csv-lens--get-cells key-indices))))
      (beginning-of-line)
      (unless (equal (forward-line dir) 0)
	(error "No more records")
        (setq found nil)))
    (beginning-of-line)
    found))

(defun csv-lens--indices-of-columns ()
  "Return a list of indices of all the columns."
  (let (column-indices)
    (--dotimes (length (csv-lens--get-columns))
      (!cons it column-indices))
    (nreverse column-indices)))

(defun csv-lens--constant-columns (candidate-constant-columns current-values previous-values)
  "The sublist of CANDIDATE-CONSTANT-COLUMNS for which the values are equal.
It contains all values of CANDIDATE-CONSTANT-COLUMNS for which
the value in CURRENT-VALUES is equal to the value in
PREVIOUS-VALUES.

CURRENT-VALUES and PREVIOUS-VALUES should be hash tables wich are indexed
by the values in candidate-constant-columns."
  (--filter 
   (equal (ht-get previous-values it)
	  (ht-get current-values it))
   candidate-constant-columns))


;;; FIXME, no more key index etc.
(defun csv-lens-constant-columns (key-indices)
  "Analyzes a csv buffer and returns a list of the column names that contain constant values.
Constant is defined as heving the same value for all occurance with identical key.
The key is determined by the values at KEY-INDICES."
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
      (progress-reporter-update reporter (cl-incf line-number))

      ;; Processing new line
      (let* ((current-values-ht (csv-lens--get-cells-ht constant-columns-indices))
             (key-values (mapcar (lambda (index) (ht-get current-values-ht index))
				 key-indices))
	     (previous (ht-get previous-cells key-values)))
        (ht-set previous-cells key-values current-values-ht)
        (when previous
          (let ((previous-number-of-constant-columns (length constant-columns-indices)))
            (setq constant-columns-indices 
                  (csv-lens--constant-columns constant-columns-indices 
                                              current-values-ht
                                              previous))
            (when (not (equal previous-number-of-constant-columns (length constant-columns-indices)))
              (progress-reporter-force-update
               reporter
               line-number
               (format "%d possible constant columns left... " (- (length constant-columns-indices) 1))))))))

      ;; Remove key column from constant list
      (setq constant-columns-indices 
	    (-difference constant-columns-indices key-indices))

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
    (pop-to-buffer source)))

;; (defun csv-lens-kill-both-buffers ()
;;   "Expected to be performed from the detail buffer."
;;   (interactive)
;;   (kill-buffer (marker-buffer csv-lens-source-marker))
;;   (csv-lens-kill-detail-buffer))

(provide 'csv-lens)
;;; csv-lens.el ends here
