;;; csv-lens-cell.el --- parse and format cells for csv-lens



;;; Code:

(require 'vendor-from-wwn)
(require 'format-human-readable-big-number)



(defvar csv-lens-cell-column-format-functions nil)

(setq csv-lens-cell-column-format-functions
  `(("StatisticTime" . csv-lens-cell-format-statistictime)
    ("PeriodStartTime" . csv-lens-cell-format-statistictime)
    ("PeriodEndTime" . csv-lens-cell-format-statistictime)
    ("IM_OriginalStatisticTime" . csv-lens-cell-format-statistictime)
    ("IM_CollectorTime" . csv-lens-cell-format-statistictime)
    ("IM_TimeLastSampled" . csv-lens-cell-format-statistictime)
    ("UsageRestriction" . csv-lens-cell-format-usagerestriction)
    ("Usage" . csv-lens-cell-format-usage)
    ("Consumed" . csv-lens-cell-format-huge-number)
    ("ConsumableBlocks" . csv-lens-cell-format-big-number-of-blocks)
    ("NumberOfBlocks" . csv-lens-cell-format-big-number-of-blocks)
    ("KBytesRead" . csv-lens-cell-format-big-number-of-kilobytes)
    ("KBytesTransferred" . csv-lens-cell-format-big-number-of-kilobytes)
    ("KBytesWritten" . csv-lens-cell-format-big-number-of-kilobytes)
    ("MaxSpeed" . csv-lens-cell-format-big-number-of-bytes)
    ("Capacity" . csv-lens-cell-format-big-number-of-bytes)
    ("RequestedSpeed" . csv-lens-cell-format-big-number-of-bytes)
    ("EMCKBytesSPARead" . csv-lens-cell-format-big-number-of-kilobytes)
    ("EMCKBytesSPBRead" . csv-lens-cell-format-big-number-of-kilobytes)
    ("EMCKBytesSPAWritten" . csv-lens-cell-format-big-number-of-kilobytes)
    ("EMCKBytesSPBWritten" . csv-lens-cell-format-big-number-of-kilobytes)
    ("PermanentAddress" . csv-lens-cell-format-wwn)
    ("SwitchWWPN" . csv-lens-cell-format-wwn)
    ("DeviceID" . csv-lens-cell-format-wwn)
    ("ElementName" . csv-lens-cell-format-wwn)
    ("Name" . csv-lens-cell-format-wwn)
    ("NameFormat" . csv-lens-cell-format-nameformat)
    ("DependentElementWWN" . csv-lens-cell-format-wwn)
    ("DependentFCPortWWN" . csv-lens-cell-format-wwn)
    ("AntecedentElementWWN" . csv-lens-cell-format-wwn)
    ("AntecedentFCPortWWN" . csv-lens-cell-format-wwn)
    ("EMCWWN" . csv-lens-cell-format-wwn)
    ("OtherIdentifyingInfo" . csv-lens-cell-format-wwn)
    ("IM_WWNOfExternalVolume" . csv-lens-cell-format-wwn)
    ("PreferredWWPN" . csv-lens-cell-format-wwn)
    ("ActiveWWPN" . csv-lens-cell-format-wwn)
    ("Speed" . csv-lens-cell-format-big-number-of-bytes)))

(defun csv-lens-cell-format-function-for-column (column)
  "Return the format function for COLUMN."
  (or
   (assoc-default column csv-lens-cell-column-format-functions )
   #'identity))


;;; Default format functions

(defun csv-lens-cell-format-nameformat (nameformat)
  (cond ((s-equals? nameformat "1") "Other*")
        ((s-equals? nameformat "2") "VPD83NAA6 (deprecated)*")
        ((s-equals? nameformat "3") "VPD83NAA5 (deprecated)*")
        ((s-equals? nameformat "4") "BPD83Type2 (deprecated)*")
        ((s-equals? nameformat "5") "BPD83Type1 (deprecated)*")
        ((s-equals? nameformat "6") "BPD83Type0 (deprecated)*")
        ((s-equals? nameformat "7") "SNVM*")
        ((s-equals? nameformat "8") "NodeWWN (deprecated)*")
        ((s-equals? nameformat "9") "NAA*")
        ((s-equals? nameformat "10") "EUI64*")
        ((s-equals? nameformat "11") "T10VID*")
        (t nameformat)))

(defun csv-lens-cell-format-wwn (wwn)
  "Returns a nicely formatted WWN."
  (interactive)
  (if (and (vendor-from-wwn/valid-wwn wwn)
           (vendor-from-wwn wwn))
      (concat (vendor-from-wwn/vendor-specific-nice-wwn wwn) " ("  (vendor-from-wwn wwn) ")*" )
    wwn))


(defun csv-lens-cell-format-huge-number (hugenumber)
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


(defun csv-lens-cell-format-big-number-of-bytes (big-number-of-bytes)
 ""
 (interactive)
 (concat
  (format-human-readable-big-number (string-to-number big-number-of-bytes) "%0.1f" *exceptional-format* "B" t :binary )
 "*"))

(defun csv-lens-cell-format-big-number-of-kilobytes (big-number-of-kilobytes)
 ""
 (concat
  (format-human-readable-big-number (* (string-to-number big-number-of-kilobytes) 1024.0) "%0.1f" *exceptional-format* "B" t :binary )
  "*"))

(defun csv-lens-cell-format-big-number-of-blocks (big-number-of-blocks)
 ""
 (concat
  (format-human-readable-big-number (* (string-to-number big-number-of-blocks) 512.0) "%0.1f" *exceptional-format* "B" t :binary )
  "*"))


(defun csv-lens-cell-format-usagerestriction (usagerestriction)
  "Returns a nicely formatted USAGERESTRICTION."
  (interactive)
  (or
   (assoc-default usagerestriction
		  '(("0" .    "Unknown*")
		    ("2" .    "Front-end only*")
		    ("3" .    "Back-end only*")
		    ("4" .    "Not restricted*")))
   usagerestriction))

(defun csv-lens-cell-format-usage (usage)
  "Returns a nicely formatted USAGE."
  (interactive)
  (or
   (assoc-default usage
                  '(("1" . "Other*")
                    ("2" . "Unrestricted*")
                    ("3" . "Reserved for ComputerSystem (the block server)*")
                    ("4" . "Reserved by Replication Services*")
                    ("5" . "Reserved by Migration Services*")
                    ("6" . "Local Replica Source*")
                    ("7" . "Remote Replica Source*")
                    ("8" . "Local Replica Target*")
                    ("9" . "Remote Replica Target*")
                    ("10" . "Local Replica Source or Target*")
                    ("11" . "Remote Replica Source or Target*")
                    ("12" . "Delta Replica Target*")
                    ("13" . "Element Component*")
                    ("14" . "Reserved as Pool Contributor*")
                    ("15" . "Composite Volume Member*")
                    ("16" . "Composite LogicalDisk Member*")
                    ("17" . "Reserved for Sparing*")))
   usage))

(defun csv-lens-cell-format-statistictime (statistictime)
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

(provide 'csv-lens-cell)
;;; csv-lens-cell.el ends here

