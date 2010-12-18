
(defpackage #:practice-asd
  (:use :common-lisp :asdf))

(in-package :practice-asd)

(defsystem practice
  :name "practice"
  :version "0.0.0"
  :maintainer "Miron Brezuleanu"
  :author "Miron Brezuleanu"
  :licence "Public domain"
  :description "Learning Common Lisp exercises."
  :components ((:file "firetrucks" :depends-on ("utils"))
               (:file "trivert" :depends-on ("utils"))
               (:file "utils")))


