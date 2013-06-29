;;; vendor-from-wwn.el --- 

(defvar vendor-from-wwn/oui-list nil)

(defun vendor-from-wwn/oui-filename()
  "Returns the filename that's used as datasource."
  (concat (file-name-directory (symbol-file 'vendor-from-wwn/oui-list-from-file)) "oui.txt"))

(defun vendor-from-wwn/oui-url()
  "Returns the url that's used as a datasource."
  "http://standards.ieee.org/develop/regauth/oui/oui.txt")

(defun vendor-from-wwn/oui-buffer()
  "Returns a buffer containing oui, gotten from internet. Returns nil if that fails."
  (url-retrieve-synchronously (vendor-from-wwn/oui-url)))

(defun vendor-from-wwn/oui-list-from-buffer ()
  "Parses the current buffer and returns an assoc list from vendor id to vendor string."
  (interactive)
  (let (id-to-vendor)
    (goto-char (point-min))
    (while (re-search-forward "\\(..\\)-\\(..\\)-\\(..\\) +([^)]*)\\(.*\\)" nil t)
      (let ((id (downcase (concat (match-string 1)
                                  (match-string 2)
                                  (match-string 3))))
            (vendor (substring (match-string 4) 2)))
        (push (cons id vendor) id-to-vendor)))
    id-to-vendor))

(defun vendor-from-wwn/oui-list-from-file ()
  "Parses the oui.txt file and returns an assoc list from vendor id to vendor string. Returns nil on some fail."
  (interactive)
  (when (file-exists-p (vendor-from-wwn/oui-filename))
    (with-temp-buffer
      (insert-file-contents (vendor-from-wwn/oui-filename))
      (vendor-from-wwn/oui-list-from-buffer)
      )))

(defun vendor-from-wwn/oui-list-from-url ()
  "Retrieves the oui.txt file and returns an assoc list from vendor id to vendor string."
  (interactive)
  (let ((buffer (vendor-from-wwn/oui-buffer))
        oui-list)
    (when buffer
      (save-excursion
        (pop-to-buffer buffer)
        (write-region (point-min) (point-max) (vendor-from-wwn/oui-filename))
        (setq oui-list (vendor-from-wwn/oui-list-from-buffer)))
      (kill-buffer buffer)
      )
    oui-list)
  )

(defun vendor-from-wwn/oui-list ()
  "Returns an assoc list of vendor id to vendor string. Does caching on first call."
  (interactive)
  (setq vendor-from-wwn/oui-list (or vendor-from-wwn/oui-list
                                     (vendor-from-wwn/oui-list-from-file)
                                     (vendor-from-wwn/oui-list-from-url)
                                     ))
  vendor-from-wwn/oui-list)

(defun vendor-from-wwn/normalize-wwn (wwn)
  "Returns the normalized form of WWN."
  (interactive)
  (mapconcat 'identity (split-string (downcase wwn) ":") ""))

(defun vendor-from-wwn/pairs (str)
  "Returns a list with pairs. This description should be better."
  (interactive)
  (let ((reststring str)
        result)
    (while (>= (length reststring) 2)
      (push (substring reststring 0 2) result)
      (setq reststring (substring reststring 2)))
    (when (> (length reststring) 0)
      (push reststring result))
    (reverse result)))

(defun vendor-from-wwn/colon-separated-pairs (str)
  ""
  (interactive)
  (mapconcat 'identity (vendor-from-wwn/pairs (vendor-from-wwn/normalize-wwn str)) ":"))

(defun vendor-from-wwn/vendor-specific-nice-wwn (wwn)
  ""
  (interactive)
  (let ((vendor-specific-extension (vendor-specific-extension-from-wwn wwn)))
    (concat "[" (vendor-from-wwn/colon-separated-pairs (vendor-sequence-from-wwn wwn)) "]"
            (when vendor-specific-extension
              (concat "[" (vendor-from-wwn/colon-separated-pairs vendor-specific-extension) "]")))))
  
(defun vendor-from-wwn/nice-wwn (wwn)
  "Return a nicely formatted version of WWN."
  (interactive)
  (let ((vendor-specific-extension (vendor-specific-extension-from-wwn wwn)))
    (concat "[" (network-address-authority-from-wwn wwn) "]" 
            "[" (vendor-from-wwn/colon-separated-pairs (oui-from-wwn wwn)) "]"
            (vendor-from-wwn/vendor-specific-nice-wwn wwn))))
  
(defun vendor-from-wwn/valid-wwn (wwn)
  "Checks the validity of a WWN. Retuns nil when invalid."
  (interactive)
  (let ((wwn (vendor-from-wwn/normalize-wwn wwn)))
    (and (or (= (length wwn) 16)
             (= (length wwn) 32))
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

(defun vendor-specific-extension-from-wwn (wwn)
  "Returns the vendor specific extension from WWN. Not every WWN has one, returns nil when noit."
  (interactive)
  (when (equal (network-address-authority-from-wwn wwn) "6")
    (substring (vendor-from-wwn/normalize-wwn wwn) 16)))

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
(ert-deftest vendor-from-wwn/nice-wwn--test()
  (assert (equal (vendor-from-wwn/nice-wwn "10:00:00:00:C9:3d:a5:46") "[1][00:00:c9][3d:a5:46]"))
  (assert (equal (vendor-from-wwn/nice-wwn "22:00:00:20:37:13:12:1a") "[2][00:20:37][13:12:1a]"))
  (assert (equal (vendor-from-wwn/nice-wwn "50:06:04:85:c5:ed:aa:4c") "[5][00:60:48][5c:5e:da:a4:c]"))
  (assert (equal (vendor-from-wwn/nice-wwn "60020F200000CA933D3D19CA000886A5")
                 "[6][00:20:f2][00:00:0c:a9:3][3d:3d:19:ca:00:08:86:a5]"))
  )

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
