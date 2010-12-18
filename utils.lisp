
(defpackage #:mbrezu-utils
  (:use :common-lisp))

(in-package :mbrezu-utils)

(defun print-hash-table (hash-table &optional (stream t))
  (maphash #'(lambda (key value)
               (format stream "~a => ~a~%" key value))
           hash-table))

(defun memo (fn)
  (let ((hash-table (make-hash-table :test #'equal)))
    #'(lambda (&rest args)
        (unless (gethash args hash-table)
          (setf (gethash args hash-table)
                (apply fn args)))
        (gethash args hash-table))))

(defun memoize-symbol (symbol)
  (setf (symbol-function symbol)
        (memo (symbol-function symbol))))
