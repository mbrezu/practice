
(defpackage #:acm-1991-finals-trivert
  (:use :common-lisp)
  (:use :mbrezu-utils))

(in-package :acm-1991-finals-trivert)

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

