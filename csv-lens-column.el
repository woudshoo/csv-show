;;; csv-lens-column.el --- Data structure to store column information

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

;; (lens-column-info 
;;  ((:value-type cumulative :format-function 'csv-lens-cell-format-huge-number))
;;  ((("StatisticTime" "IM_OriginalStatisticTime") (:format-function 'csv-lens-cell-format-statistictime :value-type point :value-function smis-time-to-float :plot nil))
;;   ("UsageRestriction" (:format-function 'csv-lens-cell-format-usagerestriction :plot nil :value-type nil))
;;   (("PermanentAddress" "SwitchWWPN" "DeviceID" "ElementName") (:format-function 'csv-lens-cell-format-wwn))
;;   ("Speed" (:value-type point))))


;;; Code:

(require 'csv-lens-smis-time)

(defvar csv-lens-column-state)
(make-variable-buffer-local 'csv-lens-column-state)

;; Declare again so the functions here will not give a warning.
(defvar csv-lens-columns)

(defvar csv-lens-default-column-state
  `(("InstanceID" :key t :diff-function csv-lens-diff-always-nil)
    ("ElementType" :diff-function csv-lens-diff-always-nil)

    ("StatisticTime" :diff-function csv-lens-diff-statistictime)
    (("StatisticTime" "PeriodStartTime" "PeriodEndTime" "IM_OriginalStatisticTime") 
     :format-function csv-lens-cell-format-statistictime)
    
    ("UsageRestriction" :format-function csv-lens-cell-format-usagerestriction)

    ("Consumed" :format-function csv-lens-cell-format-huge-number)

    (("NumberOfBlocks" "ConsumableBlocks") 
     :format-function csv-lens-cell-format-big-number-of-blocks)

    (("EMCKBytesSPBWritten" "EMCKBytesSPAWritten" 
      "EMCKBytesSPBRead" "EMCKBytesSPARead" 
      "KBytesWritten" "KBytesTransferred" "KBytesRead") 
     :format-function csv-lens-cell-format-big-number-of-kilobytes)

    (("RequestedSpeed" "Speed" "MaxSpeed") 
     :format-function csv-lens-cell-format-big-number-of-bytes)

    (("OtherIdentifyingInfo" "EMCWWN" 
      "AntecedentFCPortWWN" "AntecedentElementWWN" 
      "DependentFCPortWWN" "DependentElementWWN" 
      "ElementName" "DeviceID" 
      "SwitchWWPN" "PermanentAddress") 
     :format-function csv-lens-cell-format-wwn)))


;;;; Initialization code

(defun csv-lens-column-initialize-defaults ()
  (dolist (format-pair  csv-lens-default-column-state)
    (dolist (key (-list-guaranteed (car format-pair)))
      (let ((values (cdr format-pair)))
	(while values
	  (csv-lens-set-column-state key (car values) (cadr values))
	  (setq values (cddr values)))))))




;;;; Manipulation functions
(defun csv-lens-set-column-state (column state &optional value)
  "Sets the state of `column' to `state'.  
Optionally the state can have a value.
See also `csv-lens-column-state'"
  (let ((assoc-pair (assoc column csv-lens-column-state)))
    (if assoc-pair
	(let ((value-pair (assoc state (cdr assoc-pair))))
	  (if value-pair
	      (setcdr value-pair value)
	    (push (cons state value) (cdr assoc-pair))))
      (push (cons column (list (cons state value))) csv-lens-column-state))))


(defun csv-lens-column-state (column &optional key)
  "Return the state of the COLUMN.
The return value is eithe an alist of keys to values,
or the value of KEY."
  (let ((all-keys (assoc-default column csv-lens-column-state)))
    (if key
      (assoc-default key all-keys)
      all-keys)))

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
  "Given two strings num1 and num2 containing arbitrary numbers,
returns a string representing the difference.
Think of it as num1 - num2."
  (let ((num1 (math-read-number num1))
	(num2 (math-read-number num2)))
    (if (and num1 num2)
	(math-format-number (math-sub num1 num2))
      "")))


(defun csv-lens-diff-always-nil (a b)
  nil)

(defun csv-lens-cell-diff-function-for-column (column)
  (or 
   (csv-lens-column-state column :diff-function) 
   #'csv-lens-diff-number))

(defvar csv-lens-spark-line-incremental)
(make-variable-buffer-local 'csv-lens-spark-line-incremental)

(defun csv-lens-spark-line-toggle-incremental ()
  "Toggle between using diff's of values for the sparkle lines"
  (interactive)
  (setq csv-lens-spark-line-incremental (not csv-lens-spark-line-incremental))
  (csv-lens-fill-buffer))


(defun csv-lens-column-key-indices ()
  "Returns a list of column numbers which are the key columns.
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
