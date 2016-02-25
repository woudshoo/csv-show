;;; csv-lens-cell.el --- parse and format cells for csv-lens



;;; Code:



;;; Default format functions

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


(provide 'csv-lens-cell)
;;; csv-lens-cell.el ends here

