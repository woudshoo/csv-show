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
(defvar csv-lens-configuration-name)
;(defvar csv-lens-default-column-state nil)

(defvar csv-lens-configurations nil
  "XXX Long doc string needed here!")


;;;; Initialization code

(defun csv-lens-map-configuration-key-values (function configuration)
  "Call FUNCTION for each (column, key, value) combination in CONFIGURATION."
  (dolist (format-pair configuration)
    (dolist (key (-list-guaranteed (car format-pair)))
      (let ((values (cdr format-pair)))
	(while values
	  (funcall function key (car values) (cadr values))
	  (setq values (cddr values)))))))


(defun csv-lens-column-set-column-state-from-configuration (configuration)
  "Updates the column states from CONFIGURATION.

This will overwrite any setting with the settings from the CONFIGURATION.
It will not remove settings that are not specified in CONFIGURATION."
  (csv-lens-map-configuration-key-values #'csv-lens-default-column-state
					 configuration))

(defun csv-lens-column-enrich-column-state-from-configuration (configuration)
  "Update the column states from CONFIGURATION.

This will set the column state from the CONFIGURATION, but only
if it is not already set."
  (csv-lens-map-configuration-key-values #'csv-lens-set-column-state-if-not-set
					 configuration))

;;; Code to update state for new columns

(defun csv-lens-column-configure-for-columns (columns)
  "Initialize current CSV-Lens buffer to display COLUMNS."
  (setq csv-lens-columns columns)
  (let ((configuration-entry
	 (csv-lens-column-best-configuration csv-lens-columns
					     csv-lens-configurations)))
    
    (csv-lens-column-enrich-column-state-from-configuration (second configuration-entry))
    (setq csv-lens-configuration-name (first configuration-entry))))

;;;; Code to find the best configuration
;;;;
(defun csv-lens-defined-columns-in-configuration (configuration)
  "Return the columns defined in CONFIGURATION."
  (let (result)
    (dolist (format-pair configuration result)
      (dolist (key (-list-guaranteed (car format-pair)))
	(when (stringp key)
	  (setq result (cl-adjoin key result :test #'string=)))))))


(defun csv-lens-column-best-configuration (columns named-configurations)
  "Return given the COLUMNS the best configuration.
The best configuration is choosen from the list NAMED-CONFIGURATIONS.
Each entry in NAMED-CONFIGURATIONS is a list of two or three elements:

- (name configuration) or
- (name configuration compare-function).

Name is string which is used for identification by the user.
The configuration is a configuration as defined in XXXX.
The compare-function is used to find the best match between configurations
and has the following signature:

   (compare-function COLUMNS ENTRY-1 ENTRY-2) => ENTRY

The columns argument is passed through from this function, 
ENTRY-1 is the entry under consideration, and ENTRY-2 is the best
match up to know (which can be nil of no best configuration is found yet.)
The return value should be an ENTRY or nil."

  (let (best-entry)
    (dolist (entry named-configurations best-entry)
      (let ((compare-function (or (third entry) #'csv-lens-column-default-compare)))
	(setq best-entry (funcall compare-function columns entry best-entry)))))) 

(defun csv-lens-score-configuration (columns configuration)
  "Calculate how well COLUMNS are described by CONFIGURATION.

The return value is a cons (a . b), with A and B numbers.  A
score (A . B) is better than (X . Y) if (A . B) is lexically
smaller than (X . Y)."

  (let ((configuration-columns (csv-lens-defined-columns-in-configuration configuration)))
    (cons (length (cl-set-difference columns configuration-columns :test #'string=))
	  (length (cl-set-difference configuration-columns columns :test #'string=)))))

(defun csv-lens-score-lexical-compare (score-a score-b)
  "Return a number describing how SCORE-A compares to SCORE-B.
The return value is 
- negative, if SCORE-A comes before SCORE-B
- zero if SCORE-A and SCORE-B are the same.
- positive if SCORE-A comes after SCORE-B."
  (cond
   ((< (car score-a) (car score-b)) -1)
   ((> (car score-a) (car score-b))  1)
   (t (- (cdr score-a) (cdr score-b)))))

(defun csv-lens-column-default-compare (columns current-entry best-entry)
  "Return for COLUMNS the best entry of CURRENT-ENTRY or BEST-ENTRY."
  (let ((current-score (csv-lens-score-configuration columns (second current-entry)))
	(best-score (csv-lens-score-configuration columns (second best-entry))))
    (if (> 0 (csv-lens-score-lexical-compare current-score best-score))
	current-entry
      best-entry)))



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

(defun csv-lens-set-column-state-if-not-set (column state &optional value)
  "Set the state of COLUMN to STATE, but only if not set yet.

Optionally the state can have a VALUE.
See also `csv-lens-column-state' and `csv-lens-set-column-state'"
  (let ((assoc-pair (assoc column csv-lens-column-state)))
    (if assoc-pair
	(unless (assoc state (cdr assoc-pair))
	  (push (cons state value) (cdr assoc-pair)))
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
