;;;; data-sift.asd
;;;;
;;;; This file is part of the data-sift library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(defsystem #:data-sift
  :depends-on (#:cl-ppcre #:parse-number #:alexandria #:puri)
  :components
  ((:module "src"
            :components
            ((:file "packages")
             (:file "conditions" :depends-on ("packages"))
             (:file "sift" :depends-on ("conditions"))))))

(defsystem #:data-sift-test
  :depends-on (#:data-sift #:lift)
  :components
  ((:module "t"
            :components
            ((:file "suite")))))

(defmethod perform ((o test-op) (c (eql (find-system '#:data-sift))))
  (operate 'load-op '#:data-sift)
  (operate 'test-op '#:data-sift-test))