;;;; conditions.lisp
;;;;
;;;; This file is part of the DATA-SIFT library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:data-sift)

(define-condition invalid-rule (error)
  ((source :initform nil :initarg :source :reader invalid-rule-source)
   (message :initform nil :initarg :message :reader validation-fail-message)))

(define-condition validation-fail (error)
  ((message :initform nil :initarg :message :reader validation-fail-message))
  (:report (lambda (condition stream)
             (format stream
                     "Validation fail : ~A"
                     (validation-fail-message condition)))))

(define-condition stop-validation (condition)
  ((value :initform nil :reader stop-validation-value)
   (message :initform nil :reader stop-validation-message)))

(defun vfail (messsage &rest args)
  (error 'validation-fail
         :message 
         (or messsage
             (apply #'format nil args))))
