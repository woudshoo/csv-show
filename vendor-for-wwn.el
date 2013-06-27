;;; vendor-for-wwn.el --- 

(defvar oui-list nil)

; TODO: Comment strings
; TODO: Dynamically determine oui.txt
; TODO: nice-wwn implementeren

(defun oui-live ()
  ""
  (interactive)
  (let (id-to-vendor)
    (with-temp-buffer
      (insert-file-contents (concat emacs-package-directory "/csv-show/oui.txt"))
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

(defun oui ()
  ""
  (interactive)
  (if oui-list oui-list
    (progn
      (setq oui-list (oui-live))
      oui-list)))

(defun normalize-wwn (wwn)
  ""
  (interactive)
  (mapconcat 'identity (split-string (downcase wwn) ":") ""))

(defun nice-wwn (wwn)
  ""
  (interactive)
  wwn)

(defun valid-wwn (wwn)
  "Checks the validity of a WWN."
  (interactive)
  (let ((wwn (normalize-wwn wwn)))
    (and (or (= (length wwn) 16)
             (= (length wwn) 32))
         (not (equal "0000000000000000" wwn))
         (string-match "^[[:xdigit:]]+$" wwn))))

; 600601605A3528009EB43EF277A7E211
; 500601623CE40703
(defun vendor-for-wwn (wwn)
  ""
  (interactive)
  (let ((oui (oui))
        (normalized-wwn (normalize-wwn wwn)))

    (cond ((not (equal (substring normalized-wwn 0 1) "5" ))
           (assoc-default (substring normalized-wwn 4 10) oui))
          (t
             (assoc-default (substring normalized-wwn 1 7) oui))))
  )

(provide 'vendor-for-wwn)
;;; vendor-for-wwn.el ends here
