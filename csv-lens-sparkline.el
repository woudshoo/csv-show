;;; csv-lens-sparkline.el -- Support for sparklines

;; Copyright (C) 2013 Willem Rein Oudshoorn <woudshoo@xs4all.nl>
;;
;; Author: Willem Rein Oudshoorn <woudshoo@xs4all.nl>
;; Maintainer: ???
;; Created: 2013
;; Homepage: http://github.com/woudshoo/csv-show
;; Package-Requires: ((emacs "25.0") (sparkline "0.3"))
;;
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
          (when (and (not (csv-lens-column-state column-name :hidden))
		     (not (-contains? list-of-non-sparkling-column-names column-name)))
	      (csv-lens-spark-line))
	  (forward-line))))))

(provide 'csv-lens-sparkline)
;;; csv-lens-sparkline.el ends here
