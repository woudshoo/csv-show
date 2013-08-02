;;; spark-lines.el --- Make spark line images from lists of numbers

;; Copyright (C) 2013 Willem Rein Oudshoorn

;; Author: Willem Rein Oudshoorn <woudshoo@xs4all.nl>
;; Created: July 2013
;; Version: 0.1
;; Keywords: extensions


;; This file is not part of GNU Emacs
;; Standard GPL v3 or higher license applies.

;;; Commentary:

;; This package provide a function to create a sparkline graph.
;; Sparkline graphs were introduced by Edward Tufte in ??
;; They are charts containing a single line without any labels or indication
;; of scale.
;;
;; They are meant to be used inline in text, without changing the line hight and
;; provide a quick overview of the trend and pattern of the underlying data.
;;
;; Creating sparkline graphs is done for example by
;;
;;   (wo-make-spark-line 80 11 '(10 20 4 23.3 22.1 3.3 4.5 6.7))
;;
;; which creates in image which can be inserted in a buffer with the standard
;; image functions such as:
;;
;;   (insert-image   (wo-make-spark-line 80 11 '(10 20 4 23.3 22.1 3.3 4.5 6.7)))
;;
;;
;;; Code:

(defun wo-image-data (image)
  "Return the underlying bool-vector containing the bitmap data of IMAGE."
  (getf (cdr image) :data))

(defun wo-image-index (image x y)
  "Return the index in the bitmap vector of IMAGE for location (X Y).
Returns nil if the coordinates are outside the image."
  (let ((width (getf (cdr image) :width))
	(height (getf (cdr image) :height)))
    (when (and (>= x 0)
	       (>= y 0)
	       (> width x)
	       (> height y))
      (+ x (* (getf (cdr image) :width) y)))))

(defun wo-set-pixel (image x y value)
  "Set the pixel in IMAGE at location (X Y) to VALUE.
Value should be either nil or t, where t means foreground and nil
indicates the background.

This updates the image in place.

Note that if the coordinates are outside the image the image is
not updated and no error is throw."
  (let ((index (wo-image-index image x y)))
    (when index
      (aset (wo-image-data image) index value))))


(defun wo-make-image (width height &optional foreground background)
  "Create a bitmap image of given `WIDTH' and `HEIGHT'.
The optional `FOREGROUND' and `BACKGROUND' parameters indicate
the colors for the foreground (t) and background (nil) pixes."
  (let ((data (make-bool-vector (* width height) nil)))
    `(image :type xbm
	    :data
	    ,data
	    :height ,height
	    :width ,width
	    :foreground ,foreground
	    :background ,background
	    :ascent 100)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Line drawing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Assumption:
;;;
;;;
;;;
;;;                     |   /               |
;;;                 \ 6 | 7/                |
;;;                  \  | /                 | y increasing
;;;               5   \ |/   8		    |
;;;           ----------+---------	    |
;;;               4    /|\   1		    |
;;;                   / | \		    |
;;;                  /  |  \		    |
;;;                 / 3 | 2		    v
;;;
;;;     ------ x increasing --------->
;;;
;;;  1 - is normal
;;;  2 -
;;;         (swap x0 y0)
;;;         (swap x1 y1)
;;;  5 -
;;;         (swap x0 x1)
;;;         (swap y0 y1)
;;;
;;;  6 -    (swap x0 y1)
;;;         (swap y0 x1)
;;;
;;;  8 -    (y0 <--> -y0)
;;;         (y1 <--> -y1)
;;;
;;;  7 -    (x* <--> y*)
;;;         (y* <--> -x*)
;;;
;;;  4 -    (x* <--> -x*)
;;;
;;;  3 -    (x* <--> -y*)


(defun wo-draw-case (dx dy)
  "Return the quadrant for the directional vector (DX DY).
The return value is one of :1, ..., :8.

If the vector is on a quadrant boundary it is undefined which quadrant is returned."
  (cond
   ((and (>= dx 0)
	 (>= dy 0)
	 (>= dx dy)) :1)
   ((and (>= dx 0)
	 (>= dy 0)
	 (< dx dy)) :2)
   ((and (>= dx 0)
	 (< dy 0)
	 (>= dx (- dy))) :8)
   ((and (>= dx 0)
	 (< dy 0)
	 (< dx (- dy))) :7)
   ((and (< dx 0)
	 (>= dy 0)
	 (>= (- dx) dy)) :4)
   ((and (< dx 0)
	 (>= dy 0)
	 (< (- dx) dy)) :3)
   ((and (< dx 0)
	 (< dy 0)
	 (>= (- dx) (- dy))) :5)
   ((and (< dx 0)
	 (< dy 0)
	 (< (- dx) (- dy))) :6)
   (t (error "SHOULD NOT HAPPEN, IMPOSSIBLE OCTANT"))))

(defun wo-transformed-coordinates (x0 y0 x1 y1 octant)
  "Helper function for `wo-draw-line'.
This transforms the coordinates for (X0 Y0) (X1 Y1) in such a way that the
resulting directional vector is in quadrant :1 if the original
directional vector is in octant OCTANT.

This is usefull for drawing algorithms because it can be used
to ensure that the line from (X0 Y0) to (X1 Y1) have increasing x values
and increasing y values, where the total increase in y is less or equal than
the total increase in x.

For example the Bresenham line drawing algorithm needs this.

To be able to draw the points afterwards in the correct location,
use `wo-draw-pixel-case' which will undo the transformation
before drawing the pixel."
  (cond
   ((eq octant :1) (list x0 y0 x1 y1))
   ((eq octant :2) (list y0 x0 y1 x1))
   ((eq octant :3) (list y0 (- x0) y1 (- x1)))
   ((eq octant :4) (list (- x0) y0 (- x1) y1))
   ((eq octant :5) (list x1 y1 x0 y0))
   ((eq octant :6) (list y1 x1 y0 x0))
   ((eq octant :7) (list (- y0) x0 (- y1) x1))
   ((eq octant :8) (list x0 (- y0) x1 (- y1)))))

(defun wo-draw-pixel-case (image x y value octant)
  "Helper function for `wo-draw-line'.
Draws in IMAGE at location X Y a point with VALUE.  However
X and Y are not used directly but transformed into another octant depending on
OCTANT.  This inverts the transformation used in `wo-transformed-coordinates'.

The parameter OCTANT indicates the transformation.  It will tranform a point in
octant 1 to the octant OCTANT."
  (cond
   ((eq octant :1) (wo-set-pixel image x y value))
   ((eq octant :2) (wo-set-pixel image y x value))
   ((eq octant :3) (wo-set-pixel image (- y) x value))
   ((eq octant :4) (wo-set-pixel image (- x) y value))
   ((eq octant :5) (wo-set-pixel image x y value))
   ((eq octant :6) (wo-set-pixel image y x value))
   ((eq octant :7) (wo-set-pixel image y (- x) value))
   ((eq octant :8) (wo-set-pixel image x (- y) value))))




(defun wo-draw-line (image x0 y0 x1 y1 value)
  "Draw a line in the IMAGE from (X0 Y0) to (X1 Y1).
The color of the line is indicated by VALUE which should be either
nil or t."
  (let* ((octant (wo-draw-case (- x1 x0) (- y1 y0)))
	 (transformed (wo-transformed-coordinates x0 y0 x1 y1 octant))
	 (x0* (nth 0 transformed))
	 (y0* (nth 1 transformed))
	 (x1* (nth 2 transformed))
	 (y1* (nth 3 transformed)))
    (let* ((dx (- x1* x0*))
	   (dy (- y1* y0*))
	   (D (- (* 2 dy) dx)))
      (wo-draw-pixel-case image x0* y0* value octant)
      (while (and
	      (incf x0*)
	      (<= x0* x1*))
	(if (> D 0)
	    (progn
	      (incf y0*)
	      (wo-draw-pixel-case image x0* y0* value octant)
	      (incf D (- (* 2 dy) (* 2 dx))))
	  (progn
	    (wo-draw-pixel-case image x0* y0* value octant)
	    (incf D (* 2 dy)))))))
  image)



(defun wo-make-spark-line (width height data)
  "Create a bitmap of size WIDTH x HEIGHT containg a sparkline chart of DATA."
  (let* ((min (reduce 'min data))
	(max (reduce 'max data))
	(length (length data))
	(index 0)
	(image (wo-make-image width height (when (= min max) "gray")))
	prev-x prev-y)
    (when (= min max)
      (decf min)
      (incf max))
    (dolist (value data)
      (let ((x (/ (* width index) length))
	    (y (floor (/ (* (- height 1) (- max value)) (- max min)))))
	(when (and prev-x prev-y)
	  (wo-draw-line image prev-x prev-y x y t))
	(setq prev-x x)
	(setq prev-y y)
	(incf index)))
    image))


(provide 'spark-lines)

;;; spark-lines.el ends here
