;;; csv-lens-test.el --- Tests for the csv-lens and related code

;;; Commentary:
;;;
;;;  Some simple tests used during development.
;;;
;;; Code:

(require 'csv-lens)
(require 'ert)

(ert-deftest csv-lens--field-index-for-column-test ()
  (let ((csv-lens--get-columns-cache (list "Header1" "Header2" "Header3")))
    (assert (equal (csv-lens--field-index-for-column "Header2") 1))))

(ert-deftest csv-lens-diff-values-test ()
  (should (equal '(1 1 1) (csv-lens-diff-values '(10 11 12 13)))))


(ert-deftest csv-lens--diff-number-test ()
  (should (equal (csv-lens--diff-number "5" "3") "2"))
  (should (equal (csv-lens--diff-number "55555555555555555555" "33333333333333333333") "22222222222222222222"))
  (should (equal (csv-lens--diff-number "3" "5") "-2"))
  (should (equal (csv-lens--diff-number "0.936340455076744" "0.920434747227233") "0.01590570785"))
  (should (equal (csv-lens--diff-number "0sdfsaf44" "0.920434747227233") "")))



(ert-deftest csv-lens--column-state-test ()
  (let ((csv-lens-column-state nil))
    (csv-lens-set-column-state "hallo" :hidden)
    (should (equal (csv-lens-column-state "hallo")
    		   '((hidden))))
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
		   '(("hallo" (xx . t) (hidden . t)))))))

(provide 'csv-lens-test)
;;; csv-lens-test.el ends here
