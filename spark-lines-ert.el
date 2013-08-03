;;; spark-lines-ert.el -- Tests for the spark lines package
;;
;; Copyright (C) 2013 Willem Rein Oudshoorn
;; Created: 3 August 2013
;; Version: 0.1
;;
;; Ths file is not part of GNU Emacs
;; Standard GPL v3 or higher license applies.
;; 
;;
;;; Code:
(require 'ert)
(require 'spark-lines)


(ert-deftest wo-draw-case ()
  (should (eq :1 (wo-draw-case 100 0)))
  (should (eq :4 (wo-draw-case -100 0)))
  (should (eq :2 (wo-draw-case 0 100)))
  (should (eq :8 (wo-draw-case 100 -10)))
  (should (eq :7 (wo-draw-case 0 -100)))
  (should (eq :5 (wo-draw-case -100 -10)))
  (should (eq :6 (wo-draw-case -10 -100)))
  (should (eq :3 (wo-draw-case -10 100))))

(defun spark-lines-ert-coordinates-to-dir (coordinates)
  (list
   (- (nth 2 coordinates) (nth 0 coordinates))
   (- (nth 3 coordinates) (nth 1 coordinates))))

(ert-deftest wo-transform-1 ()
  (should (eq :1 (apply 'wo-draw-case (spark-lines-ert-coordinates-to-dir (wo-transformed-coordinates 0 0 100 0 :1)))))
  (should (eq :1 (apply 'wo-draw-case (spark-lines-ert-coordinates-to-dir (wo-transformed-coordinates 0 0 -100 0 :4)))))
  (should (eq :1 (apply 'wo-draw-case (spark-lines-ert-coordinates-to-dir (wo-transformed-coordinates 0 0 0 100 :2)))))
  (should (eq :1 (apply 'wo-draw-case (spark-lines-ert-coordinates-to-dir (wo-transformed-coordinates 0 0 100 -10 :8)))))
  (should (eq :1 (apply 'wo-draw-case (spark-lines-ert-coordinates-to-dir (wo-transformed-coordinates 0 0 0 -100 :7)))))
  (should (eq :1 (apply 'wo-draw-case (spark-lines-ert-coordinates-to-dir (wo-transformed-coordinates 0 0 -100 -10 :5)))))
  (should (eq :1 (apply 'wo-draw-case (spark-lines-ert-coordinates-to-dir (wo-transformed-coordinates 0 0 -10 -100 :6)))))
  (should (eq :1 (apply 'wo-draw-case (spark-lines-ert-coordinates-to-dir (wo-transformed-coordinates 0 0 -10 100 :3))))))

(provide 'spark-lines-ert)

;;; spark-lines-ert.el ends here
