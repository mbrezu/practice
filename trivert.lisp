;;;; This problem is from a 1991 ACM contest.
;;;; http://www.karrels.org/Ed/ACM/91/prob_b.html

;;;; To see it in action:
;;;; (in-package :acm-1991-finals-trivert)
;;;; (solver *sample-input*)

(in-package :acm-1991-finals-trivert)

(proclaim '(optimize (speed 0) (debug 3)))

(defun get-point-set (line)
  (read-from-string (concatenate 'string "(" line ")")))

(defparameter *sample-input*
  "1 2 3
11 13 29 31
26 11 13 24
4 5 9 13 12 7
1 2 3 4 5
47
11 13 23 25 ")

(defun intersperse (elm lst)
  (if (null lst)
      (list (list elm))
      (cons (cons elm lst)
            (mapcar #'(lambda (ilst)
                        (cons (car lst) ilst))
                    (intersperse elm (cdr lst))))))

(defun permutations (points)
  (if (null points)
      (list nil)
      (mapcan #'(lambda (lst)
                  (intersperse (car points) lst))
              (permutations (cdr points)))))

(defun getxy (point-no)
  (labels ((iter (prev-sum current-sum iterator)
             (if (> current-sum point-no)
                 (list (+ 1 (- point-no prev-sum))
                       (- iterator 1))
                 (iter current-sum
                       (+ current-sum iterator)
                       (+ 1 iterator)))))
    (iter 1 1 1)))

(memoize-symbol 'getxy)

(defun same-line (point1 point2)
  (destructuring-bind (x1 y1) (getxy point1)
    (destructuring-bind (x2 y2) (getxy point2)
      (cond
        ((= x1 x2) t)
        ((= y1 y2) t)
        ((= (abs (- x2 x1)) (abs (- y2 y1))) t)
        (t nil)))))

(memoize-symbol 'same-line)

(defun distance (point1 point2)
  (destructuring-bind (x1 y1) (getxy point1)
    (destructuring-bind (x2 y2) (getxy point2)
      (max (abs (- x2 x1))
           (abs (- y2 y1))))))

(memoize-symbol 'distance)

(defun is-triangle (p1 p2 p3)
  (and (same-line p1 p2)
       (same-line p2 p3)
       (same-line p3 p3)
       (let ((dist (distance p1 p2)))
         (and (= dist (distance p2 p3))
              (= dist (distance p3 p1))))))

(defun get-pairs (lst)
  (mapcar #'(lambda (p1 p2)
              (list p1 p2))
          lst
          (append (cdr lst)
                  (list (car lst)))))

(defun get-triples (lst)
  (mapcar #'(lambda (p1 p2 p3)
              (list p1 p2 p3))
          lst
          (append (cdr lst)
                  (list (car lst)))
          (append (cddr lst)
                  (list (car lst)
                        (cadr lst)))))

;;;; The following functions test if a sequence of points defines a
;;;; convex polygon.  The test used is to count the number of times
;;;; the difference between x coordinates of successive vertices
;;;; changes signs.  If the difference changes signs more than twice,
;;;; we have a concave polygon.  The same test is applied for the y
;;;; coordinate.  The coordinate system used until now becomes
;;;; unappropriate: we need a better emulation of the layout from the
;;;; problem page (we need better accuracy when computing x coordinate
;;;; differences - or we lose some sign changes).  To do that, we
;;;; subtract from each x coordonate a quantity that depends on the y
;;;; coordinate (see the is-convex function).
(defun sign-changes (vect)
  (labels ((first-non-zero (lst)
             (if (null lst)
                 0
                 (if (= 0 (car lst))
                     (first-non-zero (cdr lst))
                     (car lst)))))
    (let ((sign (signum (first-non-zero vect))))
      (second (reduce #'(lambda (sgn+change-count value)
                          (destructuring-bind (sgn change-count) sgn+change-count
                            (if (= 0 value)
                                (list sgn change-count)
                                (let ((new-sgn (signum value)))
                                  (if (= new-sgn sgn)
                                      (list sgn change-count)
                                      (list new-sgn (+ 1 change-count)))))))
                      vect
                      :initial-value (list sign 0))))))

(defun is-convex (points)
  (let* ((coords (mapcar #'getxy points))
         (edges (get-pairs coords))
         (diffxy (mapcar #'(lambda (p1p2)
                             (destructuring-bind ((x1 y1) (x2 y2)) p1p2
                               (let ((adjusted-x1 (- x1 (* 0.5 (- y1 1))))
                                     (adjusted-x2 (- x2 (* 0.5 (- y2 1)))))
                                 (list (- adjusted-x2 adjusted-x1) (- y2 y1)))))
                         edges))
         (x-sign-changes (sign-changes (mapcar #'(lambda (pd)
                                                   (car pd))
                                               diffxy)))
         (y-sign-changes (sign-changes (mapcar #'(lambda (pd)
                                                   (cadr pd))
                                               diffxy))))
    (and (<= x-sign-changes 2)
         (<= y-sign-changes 2))))

(defun is-regular-polygon (points)
  (let* ((same-line-conds (mapcar #'(lambda (p1p2)
                                      (apply #'same-line p1p2))
                                  (get-pairs points)))
         (dist (distance (nth 0 points)
                         (nth 1 points)))
         (same-dist-conds (mapcar #'(lambda (p1p2)
                                      (= dist (apply #'distance p1p2)))
                                  (get-pairs points))))
    (and (every #'(lambda (x)
                    x)
                (append same-line-conds same-dist-conds))
         (is-convex points))))

(defun is-regular-polygon-perm (points)
  (some #'is-regular-polygon (permutations points)))

(defun classify (figure)
  (cond
    ((and (= 3 (length figure))
          (is-regular-polygon-perm figure))
     (format t "~{~d ~}are the vertices of a triangle~%" figure))
    ((and (= 4 (length figure))
          (is-regular-polygon-perm figure))
     (format t "~{~d ~}are the vertices of a parallelogram~%" figure))
    ((and (= 6 (length figure))
          (is-regular-polygon-perm figure))
     (format t "~{~d ~}are the vertices of a hexagon~%" figure))
    (t (format t "~{~d ~}are not the vertices of an acceptable figure~%" figure))))

(defun solver (input)
  (with-input-from-string (str input)
    (loop
       for line in (read-input-lines str)
       do (let ((figure (read-from-string (concatenate 'string "(" line ")"))))
            (classify figure)))))
