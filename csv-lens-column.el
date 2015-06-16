;;; csv-lens-column.el --- Data structure to store column information


;;; Commentary:

;;;
;;;
;;;  Column --->   Formatter        = function
;;;         --->   value-type      [cummulative|point|category|...]
;;;         --->   value-function
;;;         --->   diff-function
;;;                plotable
;;;         --->   column-type     [key|x-axis|...]
;;;                display  [hidden|...]
;;;
;;;   Private
;;;         --->  index (column nummer)
;;;
;;;  Sparklify  : String         ---> Number
;;;             : String, String ---> Number
;;;


;; (("Column" :format-function value :key-1 value :column-index))



;;; Code:


(defvar csv-lens-column-state)
(make-variable-buffer-local 'csv-lens-column-state)

;; Declare again so the functions here will not give a warning.
(defvar csv-lens-columns)

(defvar csv-lens-default-column-state nil)


;;;; Initialization code

(defun csv-lens-column-initialize-defaults ()
  "Setup the buffer local column properties."
  (dolist (format-pair  csv-lens-default-column-state)
    (dolist (key (-list-guaranteed (car format-pair)))
      (let ((values (cdr format-pair)))
	(while values
	  (csv-lens-set-column-state key (car values) (cadr values))
	  (setq values (cddr values)))))))




;;;; Manipulation functions
(defun csv-lens-set-column-state (column state &optional value)
  "Set the state of COLUMN to STATE.
Optionally the state can have a VALUE.
See also `csv-lens-column-state'"
  (let ((assoc-pair (assoc column csv-lens-column-state)))
    (if assoc-pair
	(let ((value-pair (assoc state (cdr assoc-pair))))
	  (if value-pair
	      (setcdr value-pair value)
	    (push (cons state value) (cdr assoc-pair))))
      (push (cons column (list (cons state value))) csv-lens-column-state))))


(defun csv-lens-column-state (column key)
  "Return the state of the COLUMN for KEY.
If the value for KEY at COLUMN is nil or does not exist, fallback
to looking up the KEY in the default section."
  (or (assoc-default key (assoc-default column csv-lens-column-state))
      (assoc-default key (assoc-default t csv-lens-column-state))))

(defun csv-lens-column-state-toggle (column key)
  "Toggle for COLUMN the value of KEY.
Assumes we are only interested in generalized boolean value of the key."
  (csv-lens-set-column-state column key
			     (not (csv-lens-column-state column key))))

(defun csv-lens-column-state-indicator (column)
  "Return a string indicating the COLUMN state."
  (cond 
   ((csv-lens-column-state column :key) "K")
   ((csv-lens-column-state column :hidden) "H")
   ((csv-lens-column-state column :constant) "C")
   (t " ")))


;;;; Format functions

(defun csv-lens-cell-format-function-for-column (column)
  "Return the format function for COLUMN."
  (or
   (csv-lens-column-state column :format-function)
   #'identity))


;;; sparkline related functions, diff


(defun csv-lens-diff-number (num1 num2)
  "Given two strings NUM1 and NUM2 containing arbitrary numbers,
returns a string representing the difference.
Think of it as num1 - num2."
  (let ((num1 (math-read-number num1))
	(num2 (math-read-number num2)))
    (if (and num1 num2)
	(math-format-number (math-sub num1 num2))
      "")))


(defun csv-lens-diff-always-nil (a b)
  "Constant nil.  Usefull for columns which do not have a meaningfull diff.
The arguments A and B are ignored."
  nil)

(defun csv-lens-cell-diff-function-for-column (column)
  "Return a function for calculating the diff for COLUMN.
This function is looked up in the column configuration data, and
defaults to `csv-lens-diff-number' if it is not present in the
configuraiton data."
  (or 
   (csv-lens-column-state column :diff-function) 
   #'csv-lens-diff-number))

(defvar csv-lens-spark-line-incremental)
(make-variable-buffer-local 'csv-lens-spark-line-incremental)

(defun csv-lens-spark-line-toggle-incremental ()
  "Toggle between using diff's or values for the sparkle lines."
  (interactive)
  (setq csv-lens-spark-line-incremental (not csv-lens-spark-line-incremental))
  (csv-lens-fill-buffer))


(defun csv-lens-column-key-indices ()
  "Return a list of column numbers which are the key columns.
The list is sorted from low to high.
Assumed to be called in the Lens buffer."
  (let ((result nil)
	(columns csv-lens-columns)
	(index 0))
    (while columns
      (when (csv-lens-column-state (car columns) :key)
	(push index result))
      (pop columns)
      (setq index (+ 1 index)))
    (nreverse result)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun csv-lens--field-index-for-column (column)
  "Return the index of COLUMN."
  (position column csv-lens-columns :test #'equal))

(provide 'csv-lens-column)
;;; csv-lens-column.el ends here
