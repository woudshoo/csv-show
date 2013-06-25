;;; The following code is Copyright Pascal Bourguignon 1995 - 2011
;;; and distributed under the GPL 2 or later license.

(require 'cl)


(defun dichotomy (vector value compare &optional start end key)
  "
PRE:    entry is the element to be searched in the table.
        (<= start end)
RETURN: (list found index order)
POST:   (<= start index end)
        +-------------------+----------+-------+----------+----------------+
        | Case              |  found   | index |  order   |     Error      |
        +-------------------+----------+-------+----------+----------------+
        | x < a[min]        |   FALSE  |  min  |  less    |      0         |
        | a[i] < x < a[i+1] |   FALSE  |   i   |  greater |      0         |
        | x = a[i]          |   TRUE   |   i   |  equal   |      0         |
        | a[max] < x        |   FALSE  |  max  |  greater |      0         |
        +-------------------+----------+-------+----------+----------------+
"
  (setf start (or start 0)
        end   (or end (length vector))
        key   (or key (function identity)))
  (let* ((curmin start)
         (curmax end)
         (index  (truncate (+ curmin curmax) 2))
         (order  (funcall compare value (funcall key (aref vector index)))) )
    (loop while (and (/= 0 order) (/= curmin index)) do
         (if (< order 0)
             (setf curmax index)
             (setf curmin index))
         (setf index (truncate (+ curmin curmax) 2))
         (setf order (funcall compare value (funcall key (aref vector index)))))
    (when (and (< start index) (< order 0))
      (setf order 1)
      (decf index))
    (assert
     (or (< (funcall compare value (funcall key (aref vector start))) 0)
         (and (< (funcall compare (funcall key (aref vector index)) value) 0)
              (or (>= (1+ index) end)
                  (< (funcall compare value (funcall key (aref vector (1+ 
index)))) 0)))
         (= (funcall compare value (funcall key (aref vector index))) 0)))
    (list (= order 0) index order)))


(defun filter-prefixes (prefixes value)
  "
PREFIXES is a list of (long short (expt base exponent))
VALUE    is either an integer or a floating point value.

DO:      Filters out prefixes that are out of range for type of the
         given value, and compute the values of the symbolic exponents
         in prefixes.

RETURN:  a list of (long short base^exponent-value)
"
  (etypecase value
    (float
     (mapcar (lambda (prefix)
               (destructuring-bind (long short (expt base exponent)) prefix
                 (list long short (expt (float base) exponent))))
             prefixes))
    (integer
     (mapcan (lambda (prefix)
               (destructuring-bind (long short (expt base exponent)) prefix
                 (when (< (expt (float base) exponent) most-positive-fixnum)
                   (list (list long short (expt (float base) exponent))))))
             prefixes))))


(defun compute-prefixes ()
  "
RETURN: A hash-table mapping lists of prefix-code and type to a
        filtered and sorted vector of prefixes.
PREFIX-CODE either :si or :binary
TYPE        either float or integer
"
  (let ((table (make-hash-table :test (function equal))))
    (loop
       for value in '(0 0.0) 
       for type in '(integer float)
       do (loop
             for prefix-code in '(:si :binary)
             for prefixes     in (list *si-prefixes* *binary-prefixes*)
             do (setf (gethash (list prefix-code type) table)
                      (coerce (sort  (filter-prefixes prefixes value)
                                     (lambda (a b) (< (third a) (third b))))
                              'vector))))
    table))


(defvar *si-prefixes*
  '(("yotta" "Y" (expt 10 24))
    ("zetta" "Z" (expt 10 21))
    ("exa"   "E" (expt 10 18))
    ("peta"  "P" (expt 10 15))
    ("tera"  "T" (expt 10 12))
    ("giga"  "G" (expt 10 9))
    ("mega"  "M" (expt 10 6))
    ("kilo"  "k" (expt 10 3))
    (""      ""  (expt 10 0))
    ("milli" "m" (expt 10 -3))
    ("micro" "µ" (expt 10 -6))
    ("nano"  "n" (expt 10 -9))
    ("pico"  "p" (expt 10 -12))
    ("femto" "f" (expt 10 -15))
    ("atto"  "a" (expt 10 -18))
    ("zepto" "z" (expt 10 -21))
    ("yocto" "y" (expt 10 -24))))


(defvar *binary-prefixes*
  '(("yobi"  "Yi" (expt 2 80))
    ("zebi"  "Zi" (expt 2 70))
    ("exbi"  "Ei" (expt 2 60))
    ("pebi"  "Pi" (expt 2 50))
    ("tebi"  "Ti" (expt 2 40))
    ("gibi"  "Gi" (expt 2 30))
    ("mebi"  "Mi" (expt 2 20))
    ("kibi"  "Ki" (expt 2 10))
    (""      ""   (expt 2 0))))

(defvar *prefixes* (compute-prefixes))

(defun find-scale (num prefix-code)
  "
Find from the *prefixes* the scale of the number NUM with the given
PREFIX-CODE.
"
  (let ((prefixes  (gethash (list prefix-code (etypecase num
                                                (integer 'integer)
                                                (float   'float)))
                            *prefixes*)))
    (destructuring-bind (foundp index order)
        (dichotomy prefixes num (lambda (a b)
                                  (cond ((< a b) -1)
                                        ((< b a) +1)
                                        (t        0)))
                   0 (length prefixes) (function third))
      (cond
        ((minusp order) ; too small
         '("" "" 1))
        ((< (/ num 1000.0) (third (aref prefixes index))) ; ok
         (aref prefixes index))
        (t ; too big
         '("" "" 1))))))

(defun format-human-readable-big-number (num format exceptional-format
                                         base-unit short-form prefixes)
  (destructuring-bind (long short scale) (find-scale num prefixes)

    (format "%s %s%s" (format (if (and (= 1 scale)
                                       (or (and (< 0 (abs num)) (< (abs num) 1))
                                           (<= 1000 (abs num))))
                                  exceptional-format
                                  format)
                              (/ num scale))
            (if short-form short long)
            base-unit)))

(defvar *normal-format*       "%9.3f")
(defvar *exceptional-format*  "%13.3e")

(defun test/format-human-readable-big-number ()
  (dolist (prefixes '(:si :binary))
    (dolist (short-form '(nil t))
      (dolist (num '(4 45 456 4567 45678 456789 467890 45678901
                     456789012 4567890123 45678901234 456789012345
                     4567890123456 45678901234567 456789012345678
                     4567890123456789 45678901234567890
                     456789012345678901
                     0.04333
                     0.4333
                     4.333 45.333 456.333 4567.333
                     45678.333 456789.333 467890.333 45678901.333
                     456789012.333 4567890123.333 45678901234.333
                     456789012345.333 4567890123456.333
                     45678901234567.333 456789012345678.333
                     4567890123456789.333 45678901234567890.333
                     456789012345678901.333 4567890123456789012.333
                     45678901234567890123.333 456789012345678901234.333
                     4567890123456789012345.333
                     45678901234567890123456.333
                     456789012345678901234567.333
                     4567890123456789012345678.333
                     45678901234567890123456789.333
                     456789012345678901234567890.333
                     4567890123456789012345678901.333
                     45678901234567890123456789012.333
                     456789012345678901234567890123.333
                     ))
        (princ (format-human-readable-big-number num
                                                 *normal-format*
                                                 *exceptional-format*
                                                 (if short-form
                                                     "B"
                                                     "byte")
                                                 short-form prefixes))
        (terpri)))))

(provide 'format-human-readable-big-number)
