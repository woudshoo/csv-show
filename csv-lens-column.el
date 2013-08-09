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

;;; The data structure used during rendering looks like this:
;;;
;;; - array of column information property lists
;;; - list of key column indices
;;;
;;; packed as a cons pair:
;;;   (column-information . key-columns)


;; (defun csv-lens-key-columns (column-info)
;;   "Returns the list of key columns based upon COLUMN-INFO."
;;   (cdr column-info))


;; ;;; Creating column data

;; (defun csv-lens-column-info-for-name (column-name default-column-info)
;;   "Returns the column info for COLUMN-NAME from DEFAULT-COLUMN-INFO.
;; At the moment, single lookup, needs to be extended so multiple keys can 
;; be specified in the DEFAULT-COLUMN-INFO."
;;   (assoc-default column-name default-column-info))

;; (defun csv-lens-select-columns (column-names default-column-info)
;;   "Return a list of column properties for the COLUMN-NAMES based upon DEFAULT-COLUMN-INFO."
;;   (mapcar (lambda (column-name)

;; 	    (csv-lens-column-info-for-name column-name default-column-info))
;; 	  column-names))

;; (defun csv-lens-make-column-data (column-names default-column-info)
;;   "Create a column info for for COLUMN-NAMES based upon DEFAULT-COLUMN-INFO."
;;   (let* ((column-infos (csv-lens-select-columns column-names default-column-info)))
    
;;     (-select)
;;     (mapcar
;;      (lambda (column-name)
;;        (let ((column-info (csv-lens-column-info-for-name column-name default-column-info)))
;; 	 (if (csv-lens-is-key-column column-info)
;; 	     ())))
;;      ))
;;   ())
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Display state

(defvar csv-lens-column-state)
(make-variable-buffer-local 'csv-lens-column-state)


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
  - hidden -- hides the column in CSV Detail buffer, 
              but see also `csv-lens-column-state-toggle'
  - constant -- hides the column in CSV Detail buffer"
  (assoc-default column csv-lens-column-state))


;;;; Format functions
(defvar csv-lens-cell-column-format-functions nil)

(defun csv-lens-cell-format-function-for-column (column)
  "Return the format function for COLUMN."
  (or
   (assoc-default column csv-lens-cell-column-format-functions )
   #'identity))

;;; sparkline related functions, diff

(defvar csv-lens-spark-line-incremental)
(make-variable-buffer-local 'csv-lens-spark-line-incremental)

(defun csv-lens-spark-line-toggle-incremental ()
  "Toggle between using diff's of values for the sparkle lines"
  (interactive)
  (setq csv-lens-spark-line-incremental (not csv-lens-spark-line-incremental))
  (csv-lens-fill-buffer))

;;; Key column info
;;
;;
;; Proposed interface:
;;
;;   FUNCTION
;;   -------
;;   key-column-indices --> Returns a list of key column indices sorted
;;   key-column-names   --> Returns a list of key column names [MIGHT NOT BE NEEDED]
;; Variables

(defvar csv-lens-key-column-name "InstanceID"
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

(make-variable-buffer-local 'csv-lens-key-column-name)
(make-variable-buffer-local 'csv-lens--key-column-field-index)


(defun set-key-column-field-index ()
  "Hack, to update the key column index from the name."
  (setq csv-lens--key-column-field-index (csv-lens--field-index-for-column csv-lens-key-column-name)))


(defun csv-lens-column-key-indices ()
  "Returns a list of column numbers which are the key columns.
The list is sorted from low to high."
  (list csv-lens--key-column-field-index))

(defun csv-lens-column-key-names ()
  "Returns the list of the key column headers.
FIXME: THis might not be needed."
  (list csv-lens-key-column-name))
(provide 'csv-lens-column)
;;; csv-lens-column.el ends her
e
