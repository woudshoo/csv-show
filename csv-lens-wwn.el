;;; csv-lens-wwn.el --- add wwn formatting to csv-lens

;; Copyright (C) 2023  Willem Rein Oudshoorn <woudshoo@xs4all.nl> and Tom Koelman <tkoelman@xs4all.nl>
;;
;; Author:
;;     Willem Rein Oudshoorn <woudshoo@xs4all.nl>
;;     Tom Koelman <tkoelman@xs4all.nl>
;; Maintainer: ???
;; Created: 2023
;; Version: 0.1
;; Keywords: csv wwn
;; Homepage: http://github.com/woudshoo/csv-show
;; Package-Requires: ((vendor-from-wwn))
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

;; `csv-lens-wwn' adds wwn formatting to the `csv-lens-mode' minor mode.

;;; Code:

(require 'csv-lens)
(require 'vendor-from-wwn)

(defun csv-lens-cell-format-wwn (wwn)
  "Return a nicely formatted WWN."
  (if (and (vendor-from-wwn/valid-wwn wwn)
           (vendor-from-wwn wwn))
      (concat (vendor-from-wwn/vendor-specific-nice-wwn wwn) " ("  (vendor-from-wwn wwn) ")*" )
    wwn))

(add-to-list 'csv-lens-formatters
             '("WWN" csv-lens-cell-format-wwn))

(provide 'csv-lens-wwn)
