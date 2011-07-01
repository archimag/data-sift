;;;; conditions.lisp
;;;;
;;;; This file is part of the Procrustes library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:procrustes)

(define-condition validation-fail (condition)
  ((message :initform nil :reader validation-fail-message))
  (:report (lambda (condition stream)
             (format stream
                     "Validation fail : ~A"
                     (validation-fail-message condition)))))

(define-condition stop-validation (condition)
  ((value :initform nil :reader stop-validation-value)
   (message :initform nil :reader stop-validation-message)))
