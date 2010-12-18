;;;; This problem is from a 1991 ACM contest.
;;;; http://www.karrels.org/Ed/ACM/91/prob_a.html

;;;; To see it in action:
;;;; (in-package :acm-1991-finals-firetrucks)
;;;; (solver *sample-input*)

(proclaim '(optimize (speed 0) (debug 3)))

(in-package :acm-1991-finals-firetrucks)

(defparameter *sample-input* "6
1 2
1 3
3 4
3 5
4 6
5 6
2 3
2 4
0 0
4
2 3
3 4
5 1
1 6
7 8
8 9
2 5
5 7
3 1
1 8
4 6
6 9
0 0")

(defclass input ()
  ((target :initarg :target :accessor input-target)
   (links :initarg :links :accessor input-links)))

(defmethod print-object ((i input)
                         str)
  (format str "#<input target: ~a links: ~a>" (input-target i)
          (input-links i)))

(defun fix-input (input)
  (setf (input-links input)
        (reverse (input-links input)))
  input)

(defun add-link (input source destination)
  (setf (input-links input)
        (cons (list source destination)
              (input-links input)))
  input)

(defmacro read-string-bind (string vars &body body)
  (let ((gstream (gensym)))
    `(with-input-from-string (,gstream ,string)
       (let ,(mapcar #'(lambda (var)
                         `(,var (read ,gstream)))
                     vars)
         ,@body))))

(defun split-input (stream)
  (labels ((iter (line-list inputs current-input state)
             (if (null line-list)
                 (if (not (null current-input))
                     (nreverse (cons (fix-input current-input) inputs))
                     (nreverse inputs))
                 (cond
                   ((eql state :out-of-input)
                    (iter (cdr line-list)
                          inputs
                          (make-instance 'input
                                         :target (read-from-string (car line-list))
                                         :links nil)
                          :in-input))
                   ((eql state :in-input)
                    (if (string= (car line-list) "0 0")
                        (iter (cdr line-list)
                              (cons (fix-input current-input) inputs)
                              nil
                              :out-of-input)
                        (read-string-bind (car line-list) (source target)
                          (iter (cdr line-list)
                                inputs
                                (add-link current-input source target)
                                :in-input))))))))
    (iter (read-input-lines stream)
          nil
          nil
          :out-of-input)))

(defun make-graph (links)
  (labels ((add-link (hash-table source target)
             (setf (gethash source hash-table)
                   (cons target (gethash source hash-table)))))
    (let ((result (make-hash-table)))
      (loop
         for (start target) in links
         do (progn
              (add-link result start target)
              (add-link result target start)))
      (maphash #'(lambda (key value)
                   (setf (gethash key result)
                         (sort value #'<)))
               result)
      result)))

(defun depth-first (graph start-node target-node)
  (let ((result))
    (labels ((iter (node visited)
               (unless (member node visited)
                 (if (= node target-node)
                     (setf result (cons (reverse (cons target-node visited))
                                        result))
                     (let ((new-visited (cons node visited)))
                       (loop
                          for neighbor in (gethash node graph)
                          do (iter neighbor new-visited)))))))
      (iter start-node nil)
      (nreverse result))))

(defun solver (input)
  (let ((inputs (with-input-from-string (str input)
                  (split-input str))))
    (loop
       for input in inputs
       for i from 1
       do (progn
            (format t "CASE ~A:~%" i)
            (let* ((graph (make-graph (input-links input)))
                   (paths (depth-first graph 1 (input-target input))))
              (loop
                 for path in paths
                 do (format t "~{~d~1,4@T~}~%" path))
              (format t
                      "There are ~d routes from the firestation to streetcorner ~d.~%"
                      (length paths)
                      (input-target input)))))))

