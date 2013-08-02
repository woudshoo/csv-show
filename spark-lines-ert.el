(require 'ert)
(require 'spark-lines)


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

