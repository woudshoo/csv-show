;;; csv-lens-test.el --- Tests for the csv-lens and related code

;; Copyright (C) 2013, 2015, 2023 Willem Rein Oudshoorn <woudshoo@xs4all.nl>
;; Copyright (C) 2013, 2023 Tom Koelman
;;
;; Author: Willem Rein Oudshoorn <woudshoo@xs4all.nl>
;;         Tom Koelman
;; Created: 2013
;; Homepage: http://github.com/woudshoo/csv-show
;; Package-Requires: ((emacs "25.0") csv-lens csv-lens-column ert)
;; 
;;; Commentary:
;;
;;  Some simple tests used during development.
;;
;;; Code:

(require 'csv-lens)
(require 'csv-lens-column)
(require 'ert)

(ert-deftest csv-lens--field-index-for-column-test ()
  (let ((csv-lens-columns (list "Header1" "Header2" "Header3")))
    (assert (equal (csv-lens--field-index-for-column "Header2") 1))))

(ert-deftest csv-lens-diff-values-test ()
  (should (equal '(1 1 1) (csv-lens-diff-values '(10 11 12 13)))))


(ert-deftest csv-lens--diff-number-test ()
  (should (equal (csv-lens-diff-number "5" "3") "2"))
  (should (equal (csv-lens-diff-number "55555555555555555555" "33333333333333333333") "22222222222222222222"))
  (should (equal (csv-lens-diff-number "3" "5") "-2"))
  (should (equal (csv-lens-diff-number "0.936340455076744" "0.920434747227233") "0.01590570785"))
  (should (equal (csv-lens-diff-number "0sdfsaf44" "0.920434747227233") "")))



(ert-deftest csv-lens--column-state-test ()
  (let ((csv-lens-column-state nil))
    (csv-lens-set-column-state "hallo" :hidden)
    (should (equal (csv-lens-column-state "hallo" :hidden) nil))
    (csv-lens-set-column-state "hallo" :hidden t)
    (should (equal (csv-lens-column-state "hallo" :hidden) t))
    (should (equal (csv-lens-column-state "unknown" :hidden) nil))
    (csv-lens-set-column-state "hallo" 'image '(+ 1 2))
    (should (equal (csv-lens-column-state "hallo" :hidden) t))
    (should (equal (csv-lens-column-state "hallo" 'image) '(+ 1 2)))))


(ert-deftest csv-lens--column-state-test-2 ()
  (let ((csv-lens-column-state nil))
    (should (equal csv-lens-column-state nil))
    (csv-lens-set-column-state "hallo" :hidden t)
    (csv-lens-set-column-state "hallo" 'xx t)
    (should (equal csv-lens-column-state
		   '(("hallo" (xx . t) (:hidden . t)))))))

;;;;;;;;;;;;;;;;;;;;

(ert-deftest csv-lens--defined-columns-in-configuration-test ()
  (should-not (cl-set-exclusive-or
	       (csv-lens-defined-columns-in-configuration
		
		'(("hallo" :hidden t)))
	       '("hallo")
	       
		:test #'string=))
  
  (should-not (cl-set-exclusive-or
	       (csv-lens-defined-columns-in-configuration
		
		'(("hallo" :hidden t) ("hallo" :hidden nil)))
	       '("hallo")
	       
	       :test #'string=))
  
  (should-not (cl-set-exclusive-or
	       (csv-lens-defined-columns-in-configuration
		
		'(("hallo" :hidden t)
		  (("hallo" "daar") :hidden nil)))
	       '("hallo" "daar")

	       :test #'string=))
  
  (should-not (cl-set-exclusive-or
	       (csv-lens-defined-columns-in-configuration
		
		'((t :hidden t)))
	       nil

	       :test #'string=)))

(ert-deftest csv-lens--best-configuration-test ()
  (let ((configurations
	 '(("first" (("hallo" :hidden t)))
	   ("second" (("hallo" :hidden t)
		       ("daar" :hidden t)))
	   ("third" (("daar" :hidden t))))))
    (should
     (string= "first"
	      (car (csv-lens-column-best-configuration '("hallo") configurations))))
    (should
     (string= "third"
	      (car (csv-lens-column-best-configuration '("daar") configurations))))
    (should-not
     (car (csv-lens-column-best-configuration '("xxx" "yyy") configurations)))
    (should
     (string= "third"
	      (car (csv-lens-column-best-configuration '("xxx" "daar") configurations))))
    (should
     (string= "second"
	      (car (csv-lens-column-best-configuration '("hallo" "daar") configurations))))))


(ert-deftest csv-lens--score-test ()
  (should (> 0 (csv-lens-score-lexical-compare '(1 . 2) '(2 . 3))))
  (should (> 0 (csv-lens-score-lexical-compare '(1 . 2) '(1 . 3))))
  (should (> 0 (csv-lens-score-lexical-compare '(1 . 2) '(2 . 1))))
  (should (= 0 (csv-lens-score-lexical-compare '(1 . 2) '(1 . 2))))
  (should (= 0 (csv-lens-score-lexical-compare '(2 . 1) '(2 . 1))))
  (should (< 0 (csv-lens-score-lexical-compare '(2 . 1) '(1 . 2))))
  (should (< 0 (csv-lens-score-lexical-compare '(2 . 1) '(2 . 0))))
  (should (< 0 (csv-lens-score-lexical-compare '(2 . 1) '(1 . 0)))))


(provide 'csv-lens-test)
;;; csv-lens-test.el ends here
