;;; csv-lens-sparkline.el -- Support for sparklines
;;



;; Code:
(require 'sparkline)


(defun csv-lens-diff-values (list)
  (let ((first-value (first list))
	result)
    (dolist (element (rest list))
      (push (- element first-value) result)
      (setq first-value element))
    (nreverse result)))

(defun csv-lens-spark-line ()
  (interactive)

  (let* ((column (csv-lens-column-name))
	 (key-indices (csv-lens-column-key-indices))
	 (value-index (csv-lens--field-index-for-column column))
	 (result (list)))
    (message (concat "Spark line for " column ))
    (csv-lens--in-source-buffer
	nil
      (let ((key-values (csv-lens--get-cells-fast key-indices)))
	(goto-char (point-min))
	(while (and (forward-line) (not (eobp)))
	  (when (equal (csv-lens--get-cells-fast key-indices) key-values)
	    (let* ((value-string (car (csv-lens--get-cells-fast (list value-index))))
		   (value (and value-string (string-to-number value-string))))
	      (when value
		(push value result)))))))
    
    (setq result (nreverse result))
    (when csv-lens-spark-line-incremental
      (setq result (csv-lens-diff-values result)))
    (csv-lens-set-column-state column 'sparkline (sparkline-make-sparkline 80 11 result))
    (csv-lens-fontify-detail-buffer)))

(defun csv-lens-spark-line-for-all-visible-columns ()
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (forward-line 3)
    (let ((list-of-non-sparkling-column-names (list "InstanceID" "StatisticTime" "ElementType")))
      (while (csv-lens-column-name)
        (let ((column-name (csv-lens-column-name)))
          (when (and (not (csv-lens-column-state column-name 'hidden))
		     (not (-contains? list-of-non-sparkling-column-names column-name)))
	      (csv-lens-spark-line))
	  (forward-line))))))

(provide 'csv-lens-sparkline)
;;; csv-lens-sparkline.el ends here
