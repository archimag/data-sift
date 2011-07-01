;;;; procrustes.asd
;;;;
;;;; This file is part of the Procrustes library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(defsystem #:procrustes
  :depends-on (#:cl-ppcre #:parse-number #:alexandria #:puri)
  :components
  ((:module "src"
            :components
            ((:file "packages")
             (:file "conditions" :depends-on ("packages"))
             (:file "validation" :depends-on ("conditions"))))))

(defsystem #:procrustes-test
  :depends-on (#:procrustes #:lift)
  :components
  ((:module "t"
            :components
            ((:file "suite")))))

(defmethod perform ((o test-op) (c (eql (find-system '#:procrustes))))
  (operate 'load-op '#:procrustes)
  (operate 'test-op '#:procrustes-test))