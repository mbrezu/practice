
;; http://streamtech.nl/problemset/105.html
;; (solver *sample-input*)
;; in this package to see the example from the page solved

(defpackage :streamtech-skyline
  (:use :cl)
  (:use :mbrezu-utils))

(in-package :streamtech-skyline)

(declaim (optimize debug))

(defclass building ()
  ((start :accessor building-start :initarg :start)
   (end :accessor building-end :initarg :end)
   (height :accessor building-height :initarg :height)))

(defmethod print-object ((building building) stream)
  (format stream
          "#<building start:~a end:~a height:~a>"
          (building-start building)
          (building-end building)
          (building-height building)))

(defparameter *sample-input*
  "1 11 5
2 6 7
3 13 9
12 7 16
14 3 25
19 18 22
23 13 29
24 4 28")

(defun read-input (input)
  (labels ((building-from-string (str)
             (let ((numbers (read-from-string (concatenate 'string "(" str ")"))))
               (make-instance 'building
                              :start (first numbers)
                              :end (third numbers)
                              :height (second numbers)))))
    (with-input-from-string (str input)
      (let* ((lines (read-input-lines str))
             (filtered (remove-if #'(lambda (str)
                                      (= (length (string-trim " " str))
                                         0))
                                  lines)))
        (mapcar #'building-from-string filtered)))))

(defclass skyline-event ()
  ((location :accessor skyline-event-location :initarg :location)
   (operation :accessor skyline-event-operation :initarg :operation)
   (height :accessor skyline-event-height :initarg :height)))

(defmethod print-object ((event skyline-event)
                         stream)
  (format stream "#<skyline-event location:~a operation:~a height:~a>"
          (skyline-event-location event)
          (skyline-event-operation event)
          (skyline-event-height event)))

(defun buildings-to-events (buildings)
  (labels ((extract-building-events (building)
             (list (make-instance 'skyline-event
                                  :location (building-start building)
                                  :operation :add
                                  :height (building-height building))
                   (make-instance 'skyline-event
                                  :location (building-end building)
                                  :operation :remove
                                  :height (building-height building)))))
    (let ((unsorted-events (mapcan #'extract-building-events buildings)))
      (sort unsorted-events #'< :key #'skyline-event-location))))

(defclass build-state ()
  ((skyline :accessor bs-skyline :initarg :skyline)
   (heights :accessor bs-heights :initarg :heights)))

(defun build-scanline (events)
  (labels ((update-state (state event)
             (let* ((skyline (bs-skyline state))
                    (current-heights (bs-heights state))
                    (new-heights (ecase (skyline-event-operation event)
                                   (:add (cons (skyline-event-height event)
                                               current-heights))
                                   (:remove (remove (skyline-event-height event)
                                                    current-heights
                                                    :count 1))))
                    (new-skyline-segment (list (skyline-event-location event)
                                               (if (null new-heights)
                                                   0
                                                   (apply #'max new-heights))))
                    (new-skyline (if (or (null skyline)
                                         (not (= (second new-skyline-segment)
                                                 (second (first skyline)))))
                                     (cons new-skyline-segment skyline)
                                     skyline)))
               (make-instance 'build-state :skyline new-skyline :heights new-heights))))
    (let* ((final-state (reduce #'update-state events :initial-value (make-instance 'build-state
                                                                                    :skyline '()
                                                                                    :heights '())))
           (final-reversed-skyline (bs-skyline final-state)))
      (apply #'append (nreverse final-reversed-skyline)))))

(defun solver (input)
  (let* ((skyline (build-scanline (buildings-to-events (read-input input))))
         (str (format nil "~{~a~^ ~}" skyline)))
    str))
