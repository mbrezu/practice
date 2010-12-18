
(defpackage :mbrezu-utils
  (:use :common-lisp)
  (:export :print-hash-table
           :read-input-lines
           :memoize-symbol
           :memo))

(defpackage :acm-1991-finals-firetrucks
  (:use :common-lisp)
  (:use :mbrezu-utils))

(defpackage :acm-1991-finals-trivert
  (:use :common-lisp)
  (:use :mbrezu-utils))

