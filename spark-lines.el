(require 'ert)

(defun wo-image-data (image)
  (getf (cdr image) :data))

(defun wo-image-index (image x y)
  (let ((width (getf (cdr image) :width))
	(height (getf (cdr image) :height)))
    (when (and (>= x 0)
	       (>= y 0)
	       (> width x)
	       (> height y))
      (+ x (* (getf (cdr image) :width) y)))))

(defun wo-set-pixel (image x y value)
  (let ((index (wo-image-index image x y)))
    (when index
      (aset (wo-image-data image) index value))))


(defun wo-make-image (width height &optional foreground background)
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


(defun wo-draw-case (x0 y0 x1 y1)
  (let ((dx (- x1 x0))
	(dy (- y1 y0)))
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
     (t (error "SHOULD NOT HAPPEN, IMPOSSIBLE OCTANT")))))

(defun wo-transformed-coordinates (x0 y0 x1 y1)
  (let ((octant (wo-draw-case x0 y0 x1 y1)))
    (cond
     ((eq octant :1) (list x0 y0 x1 y1))
     ((eq octant :2) (list y0 x0 y1 x1))
     ((eq octant :3) (list y0 (- x0) y1 (- x1)))
     ((eq octant :4) (list (- x0) y0 (- x1) y1))
     ((eq octant :5) (list x1 y1 x0 y0))
     ((eq octant :6) (list y1 x1 y0 x0))
     ((eq octant :7) (list (- y0) x0 (- y1) x1))
     ((eq octant :8) (list x0 (- y0) x1 (- y1))))))

(defun wo-draw-pixel-case (image x y value octant)
  (cond
   ((eq octant :1) (wo-set-pixel image x y value))
   ((eq octant :2) (wo-set-pixel image y x value))
   ((eq octant :3) (wo-set-pixel image (- y) x value))
   ((eq octant :4) (wo-set-pixel image (- x) y value))
   ((eq octant :5) (wo-set-pixel image x y value))
   ((eq octant :6) (wo-set-pixel image y x value))
   ((eq octant :7) (wo-set-pixel image y (- x) value))
   ((eq octant :8) (wo-set-pixel image x (- y) value))))




(ert-deftest wo-draw-case ()
  (should (eq :1 (wo-draw-case 0 0 100 0)))
  (should (eq :4 (wo-draw-case 0 0 -100 0)))
  (should (eq :2 (wo-draw-case 0 0 0 100)))
  (should (eq :8 (wo-draw-case 0 0 100 -10)))
  (should (eq :7 (wo-draw-case 0 0 0 -100)))
  (should (eq :5 (wo-draw-case 0 0 -100 -10)))
  (should (eq :6 (wo-draw-case 0 0 -10 -100)))
  (should (eq :3 (wo-draw-case 0 0 -10 100))))

(ert-deftest wo-transform-1 ()
  (should (eq :1 (apply 'wo-draw-case (wo-transformed-coordinates 0 0 100 0))))
  (should (eq :1 (apply 'wo-draw-case (wo-transformed-coordinates 0 0 -100 0))))
  (should (eq :1 (apply 'wo-draw-case (wo-transformed-coordinates 0 0 0 100))))
  (should (eq :1 (apply 'wo-draw-case (wo-transformed-coordinates 0 0 100 -10))))
  (should (eq :1 (apply 'wo-draw-case (wo-transformed-coordinates 0 0 0 -100))))
  (should (eq :1 (apply 'wo-draw-case (wo-transformed-coordinates 0 0 -100 -10))))
  (should (eq :1 (apply 'wo-draw-case (wo-transformed-coordinates 0 0 -10 -100))))
  (should (eq :1 (apply 'wo-draw-case (wo-transformed-coordinates 0 0 -10 100)))))


(defun wo-draw-line (image x0 y0 x1 y1 value)
  (let* ((octant (wo-draw-case x0 y0 x1 y1))
	 (transformed (wo-transformed-coordinates x0 y0 x1 y1))
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
  "Creates a spark-line image based upon data"
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

