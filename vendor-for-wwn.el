;;; vendor-for-wwn.el --- 

(defvar vendor-for-wwn/oui-list nil)

; TODO: Comment strings
; TODO: Dynamically determine oui.txt
; TODO: regexp matchin in buffer, not in string

(defun vendor-for-wwn/oui-filename()
  ""
  (concat (file-name-directory (symbol-file 'vendor-for-wwn/oui-list-from-file)) "oui.txt"))

(defun vendor-for-wwn/oui-list-from-file ()
  ""
  (interactive)
  (let (id-to-vendor)
    (with-temp-buffer
      (insert-file-contents (vendor-for-wwn/oui-filename))
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

(defun vendor-for-wwn/oui-list ()
  ""
  (interactive)
  (if vendor-for-wwn/oui-list vendor-for-wwn/oui-list
    (progn
      (setq vendor-for-wwn/oui-list (vendor-for-wwn/oui-list-from-file))
      vendor-for-wwn/oui-list)))

(defun vendor-for-wwn/normalize-wwn (wwn)
  ""
  (interactive)
  (mapconcat 'identity (split-string (downcase wwn) ":") ""))

(defun vendor-for-wwn/nice-wwn (wwn)
  ""
  (interactive)
  (let ((reststring (vendor-for-wwn/normalize-wwn wwn))
        resultstring)
    (while (>= (length reststring) 2)
      (setq resultstring (concat resultstring (substring reststring 0 2) ":")
            reststring (substring reststring 2)))
    (if (length resultstring)
        (setq resultstring (substring resultstring 0 (- (length resultstring) 1))))
    resultstring))

(defun vendor-for-wwn/valid-wwn (wwn)
  "Checks the validity of a WWN."
  (interactive)
  (let ((wwn (vendor-for-wwn/normalize-wwn wwn)))
    (and (or (= (length wwn) 16)
             (= (length wwn) 32))
         (not (equal "0000000000000000" wwn))
         (string-match "^[[:xdigit:]]+$" wwn))))

; 600601605A3528009EB43EF277A7E211
; 500601623CE40703
(defun vendor-for-wwn (wwn)
  ""
  (interactive)
  (let ((oui (vendor-for-wwn/oui-list))
        (normalized-wwn (vendor-for-wwn/normalize-wwn wwn)))

    (cond ((not (equal (substring normalized-wwn 0 1) "5" ))
           (assoc-default (substring normalized-wwn 4 10) oui))
          (t
             (assoc-default (substring normalized-wwn 1 7) oui))))
  )

(provide 'vendor-for-wwn)
;;; vendor-for-wwn.el ends here
