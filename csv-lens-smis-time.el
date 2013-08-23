;;; csv-lens-smis-time.el --- parse and format time strings

;;; Commentary:

;; Code to parse and reformat SMIS time stamps.

;;; Code:

(defun smis-time-to-time-string (smis-time)
  "Given a SMIS-TIME return the corresponding time string."
  (format "%s-%s-%s %s:%s:%s"
          (substring smis-time 0 4)
          (substring smis-time 4 6)
          (substring smis-time 6 8)
          (substring smis-time 8 10)
          (substring smis-time 10 12)
          (substring smis-time 12 14)))

(defun parse-smis-time-string (smis-time)
  "Convert SMIS-TIME to a time."
  (date-to-time (smis-time-to-time-string smis-time)))


(defun float-smis-time (smis-time)
  "Return a float representing the epoch for SMIS-TIME."
  (float-time (parse-smis-time-string smis-time)))


(defun diff-smis-times (smis-time1 smis-time2)
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


(defun csv-lens-diff-statistictime (time1 time2)
  "Return a nice string representation of TIME1 - TIME2."
  (seconds-to-string (diff-smis-times time1 time2)))

(provide 'csv-lens-smis-time)
;;; csv-lens-smis-time.el ends here
