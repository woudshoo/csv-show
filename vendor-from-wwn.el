;;; vendor-from-wwn.el --- 

(defvar vendor-from-wwn/oui-list nil)

; TODO: Dynamically determine oui.txt
; TODO: regexp matchin in buffer, not in string

(defun vendor-from-wwn/oui-filename()
  "Returns the filename that's used as datasource."
  (concat (file-name-directory (symbol-file 'vendor-from-wwn/oui-list-from-file)) "oui.txt"))

(defun vendor-from-wwn/oui-list-from-file ()
  "Parses the oui.txt file and retusn an assoc list from vendor id to vendor string."
  (interactive)
  (let (id-to-vendor)
    (with-temp-buffer
      (insert-file-contents (vendor-from-wwn/oui-filename))
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
          (when (string-match "^\\(..\\)-\\(..\\)-\\(..\\) +([^)]*)\\(.*\\)" line)
            (let ((id (downcase (concat (match-string 1 line)
                                        (match-string 2 line)
                                        (match-string 3 line))))
                  (vendor (substring (match-string 4 line) 2)))
              (push (cons id vendor) id-to-vendor))))
        (forward-line))
      id-to-vendor)))

(defun vendor-from-wwn/oui-list ()
  "Returns an assoc list of vendor id to vendor string. Does caching on first call."
  (interactive)
  (if vendor-from-wwn/oui-list vendor-from-wwn/oui-list
    (progn
      (setq vendor-from-wwn/oui-list (vendor-from-wwn/oui-list-from-file))
      vendor-from-wwn/oui-list)))

(defun vendor-from-wwn/normalize-wwn (wwn)
  "Returns the normalized form of WWN."
  (interactive)
  (mapconcat 'identity (split-string (downcase wwn) ":") ""))

(defun vendor-from-wwn/nice-wwn (wwn)
  "Return a nicely formatted version of WWN."
  (interactive)
  (let ((reststring (vendor-from-wwn/normalize-wwn wwn))
        resultstring)
    (while (>= (length reststring) 2)
      (setq resultstring (concat resultstring (substring reststring 0 2) ":")
            reststring (substring reststring 2)))
    (if (length resultstring)
        (setq resultstring (substring resultstring 0 (- (length resultstring) 1))))
    resultstring))

(defun vendor-from-wwn/valid-wwn (wwn)
  "Checks the validity of a WWN. Retuns nil when invalid."
  (interactive)
  (let ((wwn (vendor-from-wwn/normalize-wwn wwn)))
    (and (or (= (length wwn) 16)
             (= (length wwn) 32))
         (not (equal "0000000000000000" wwn))
         (string-match "^[[:xdigit:]]+$" wwn))))

(defun vendor-sequence-from-wwn (wwn)
  "Returns the vendor sequence or serial number from WWN."
  (interactive)
  (let ((wwn (vendor-from-wwn/normalize-wwn wwn))
        (naa (network-address-authority-from-wwn wwn)))
    (cond ((or (equal naa "1")
               (equal naa "2"))
           (substring wwn 10))
          ((equal naa "5")
           (substring wwn 7))
          ((equal naa "6")
           (substring wwn 7 16))
)))
    
(defun oui-from-wwn (wwn)
  "Returns the Organizationally Unique Identifier or OUI from WWN."
  (interactive)
  (let ((wwn (vendor-from-wwn/normalize-wwn wwn))
        (naa (network-address-authority-from-wwn wwn)))
    (cond ((or (equal naa "1")
               (equal naa "2"))
           (substring wwn 4 10))
          ((or (equal naa "5")
               (equal naa "6"))
           (substring wwn 1 7))
)))

(require 'ert)
(ert-deftest oui-from-wwn--test()
  (assert (equal (oui-from-wwn "10:00:00:00:C9:3d:a5:46") "0000c9"))
  (assert (equal (oui-from-wwn "10:00:00:60:69:90:72:bc") "006069"))
  (assert (equal (oui-from-wwn "22:00:00:20:37:13:12:1a") "002037"))
  (assert (equal (oui-from-wwn "21:00:00:20:37:13:12:1a") "002037"))
  (assert (equal (oui-from-wwn "20:00:00:20:37:13:12:1a") "002037"))
  (assert (equal (oui-from-wwn "50:06:04:85:c5:ed:aa:4c") "006048"))
  (assert (equal (oui-from-wwn "60:02:0f:20:00:00:ca:93:3f:2e:b2:d4:00:01:8a:54") "0020f2"))
  (assert (equal (oui-from-wwn "60020F200000CA933D3D19CA000886A5") "0020f2"))
)

(ert-deftest vendor-sequence-from-wwn--test()
  (assert (equal (vendor-sequence-from-wwn "10:00:00:00:C9:3d:a5:46") "3da546"))
  (assert (equal (vendor-sequence-from-wwn "10:00:00:60:69:90:72:bc") "9072bc"))
  (assert (equal (vendor-sequence-from-wwn "22:00:00:20:37:13:12:1a") "13121a"))
  (assert (equal (vendor-sequence-from-wwn "21:00:00:20:37:13:12:1a") "13121a"))
  (assert (equal (vendor-sequence-from-wwn "20:00:00:20:37:13:12:1a") "13121a"))
  (assert (equal (vendor-sequence-from-wwn "50:06:04:85:c5:ed:aa:4c") "5c5edaa4c"))
  (assert (equal (vendor-sequence-from-wwn "60:02:0f:20:00:00:ca:93:3f:2e:b2:d4:00:01:8a:54") "00000ca93"))
  (assert (equal (vendor-sequence-from-wwn "60020F200000CA933D3D19CA000886A5") "00000ca93"))
)

(ert-deftest csv-show--field-index-for-column-test ()
  (let ((csv-show--get-columns-cache (list "Header1" "Header2" "Header3")))
    (assert (equal (csv-show--field-index-for-column "Header2") 1))))

(defun network-address-authority-from-wwn (wwn)
  "Returns the Network Address Authority or NAA from WWN."
  (interactive)
  (let ((wwn (vendor-from-wwn/normalize-wwn wwn)))
    (cond ((equal (substring wwn 0 1) "1")
           "1")
          ((equal (substring wwn 0 1) "2")
           "2")
          ((equal (substring wwn 0 1) "5")
           "5")
          ((equal (substring wwn 0 1) "6")
           "6")
)))

(ert-deftest network-address-authority-from-wwn--test()
  (assert (equal (network-address-authority-from-wwn "10:00:00:00:C9:3d:a5:46") "1"))
  (assert (equal (network-address-authority-from-wwn "10:00:00:60:69:90:72:bc") "1"))
  (assert (equal (network-address-authority-from-wwn "22:00:00:20:37:13:12:1a") "2"))
  (assert (equal (network-address-authority-from-wwn "21:00:00:20:37:13:12:1a") "2"))
  (assert (equal (network-address-authority-from-wwn "20:00:00:20:37:13:12:1a") "2"))
  (assert (equal (network-address-authority-from-wwn "50:06:04:85:c5:ed:aa:4c") "5"))
  (assert (equal (network-address-authority-from-wwn "60:02:0f:20:00:00:ca:93:3f:2e:b2:d4:00:01:8a:54") "6"))
  (assert (equal (network-address-authority-from-wwn "60020F200000CA933D3D19CA000886A5") "6"))
)

(defun vendor-from-wwn (wwn)
  "Returns the vendor for WWN."
  (interactive)
  (let ((oui-list (vendor-from-wwn/oui-list))
        (oui (oui-from-wwn wwn)))
    (assoc-default oui oui-list)))

(ert-deftest vendor-from-wwn--test()
  (assert (equal (vendor-from-wwn "10:00:00:60:69:90:72:bc") "Brocade Communications Systems, Inc."))
  (assert (equal (vendor-from-wwn "50:06:04:85:c5:ed:aa:4c") "EMC CORPORATION"))
)

(provide 'vendor-from-wwn)
;;; vendor-from-wwn.el ends here
