;;;; This problem is from a 1991 ACM contest.
;;;; http://www.karrels.org/Ed/ACM/91/prob_d.html

;;;; To see it in action:
;;;; (in-package :acm-1991-finals-domino)
;;;; (solver *sample-input*)

(in-package :acm-1991-finals-domino)

(proclaim '(optimize (speed 0) (debug 3)))

(defparameter *sample-input* "5 4 3 6 5 3 4 6
0 6 0 1 2 3 1 1
3 2 6 5 0 4 2 0
5 3 6 2 3 2 0 6
4 0 4 1 0 0 4 1
5 2 2 4 4 1 6 5
5 5 3 6 1 2 3 1
4 2 5 2 6 3 5 4
5 0 4 3 1 4 1 1
1 2 3 0 2 2 2 2
1 4 0 1 3 5 6 5
4 0 6 0 3 6 6 5
4 0 1 6 4 0 3 0
6 5 3 6 2 1 5 3
")

(defun calculate-bone-pips ()
  (let ((result (make-array 28)))
    (let ((pips-on-bones
           '((1 0 0) (2 0 1) (3 0 2)
             (4 0 3) (5 0 4) (6 0 5) (7 0 6) (8 1 1)
             (9 1 2) (10 1 3) (11 1 4) (12 1 5) (13 1 6)
             (14 2 2) (15 2 3) (16 2 4) (17 2 5) (18 2 6)
             (19 3 3) (20 3 4) (21 3 5) (22 3 6) (23 4 4)
             (24 4 5) (25 4 6) (26 5 5) (27 5 6) (28 6 6))))
      (loop
         for pob in pips-on-bones
         for i from 0
         do
           (setf (aref result i)
                 (cdr pob)))
      result)))

(defparameter *bones* (calculate-bone-pips))

(defclass domino-board ()
  ((input :accessor db-input
          :initform (make-array '(7 8))
          :initarg :input)
   (allocated :accessor db-allocated
              :initform (make-array '(7 8) :initial-element nil)
              :initarg :allocated)))

(defmethod print-object ((db domino-board)
                         stream)
  (format stream
          "board: ~A~%allocated bones: ~A~%~%"
          (db-input db)
          (db-allocated db)))

(defun read-input (str)
  (labels ((parse-lines (lines)
             (mapcar #'(lambda (line)
                         (read-from-string (concatenate 'string
                                                        "("
                                                        line
                                                        ")")))
                     lines))
           (iter (lines acc)
             (if (null lines)
                 (nreverse acc)
                 (let ((layout (parse-lines (subseq lines 0 7))))
                   (iter (subseq lines 7)
                         (cons (make-instance
                                'domino-board
                                :input (make-array '(7 8)
                                                   :initial-contents layout))
                               acc))))))
    (iter (read-input-lines str)
          nil)))

(defun next-pos (pos)
  (destructuring-bind ((x1 y1) (x2 y2)) pos
    (cond
      ((and (= x1 7)
            (= y1 6))
       nil)
      ((= x1 7) `((0 ,(+ y1 1))
                  (0 ,(+ y2 1))))
      ((= x1 x2) `((,x1 ,y1) (,(+ 1 x1) ,y1)))
      ((= y1 y2) `((,(+ 1 x1) ,y1) (,(+ 1 x1) ,(+ 1 y1))))
      (t nil))))

(defun in-range (pos)
  (destructuring-bind ((x1 y1) (x2 y2)) pos
    (and (>= x1 0)
         (>= y1 0)
         (>= x2 0)
         (>= y2 0)
         (<= x1 7)
         (<= x2 7)
         (<= y1 6)
         (<= y2 6))))

(defun all-positions ()
  (loop
     for pos = '((0 0) (0 1)) then (next-pos pos)
     while pos
     when (in-range pos)
     collect pos))

(defparameter *all-positions* (all-positions))

(defun suitable-bone (db bone-index pos-to-allocate)
  (destructuring-bind (pip1 pip2) (aref *bones* bone-index)
    ;; (format t "suitable bone pips: ~a ~a~%" pip1 pip2)
    (destructuring-bind ((x1 y1) (x2 y2)) pos-to-allocate
      (or (and (= pip1 (aref (db-input db) y1 x1))
               (= pip2 (aref (db-input db) y2 x2)))
          (and (= pip2 (aref (db-input db) y1 x1))
               (= pip1 (aref (db-input db) y2 x2)))))))

(defun free-pos (db pos)
  (destructuring-bind ((x1 y1) (x2 y2)) pos
    (and (not (aref (db-allocated db) y1 x1))
         (not (aref (db-allocated db) y2 x2)))))

(defun find-positions-for-bone (db bone-index)
  (loop
     for pos in *all-positions*
     when (and (suitable-bone db bone-index pos)
               (free-pos db pos))
     collect pos))

(defun allocate-bone (db bone-idx pos)
  (destructuring-bind ((x1 y1) (x2 y2)) pos
    (setf (aref (db-allocated db) y1 x1) (1+ bone-idx))
    (setf (aref (db-allocated db) y2 x2) (1+ bone-idx))))

(defun deallocate-bone (db pos)
  (destructuring-bind ((x1 y1) (x2 y2)) pos
    (setf (aref (db-allocated db) y1 x1) nil)
    (setf (aref (db-allocated db) y2 x2) nil)))

(defun copy-array (array)
  (read-from-string (format nil "~a" array)))

(defun allocate (db bone-index allocate-count)
  (if (= bone-index allocate-count)
      (list (copy-array (db-allocated db)))
      (apply #'append (loop
                         for pos in (find-positions-for-bone db bone-index)
                         do
                           (allocate-bone db bone-index pos)
                         collect
                           (allocate db (1+ bone-index) allocate-count)
                         do
                           (deallocate-bone db pos)))))

(defparameter *psi*
  (with-input-from-string (str *sample-input*)
    (read-input str)))

(defun print-array (array)
  (loop for i from 0 to 6 do
       (loop for k from 0 to 7 do
            (format t "~4d" (aref array i k)))
       (format t "~%")))

(defun solve-input (input index)
  (let ((solutions (allocate input 0 28)))
    (format t "~&Layout #~d:~%~%" index)
    (print-array (db-input input))
    (format t "~%Maps resulting from layout #~d are:~%~%" index)
    (loop for solution in solutions do
         (print-array solution)
         (format t "~%"))
    (format t
            "There are ~d solution(s) for layout #~d.~%~%~%"
            (length solutions)
            index)))

(defun solver (input)
  (with-input-from-string (str input)
    (loop
       for input in (read-input str)
       for i from 1
       do (solve-input input i))))

