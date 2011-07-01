;;;; packages.lisp
;;;;
;;;; This file is part of the Procrustes library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(defpackage #:procrustes
  (:use #:cl #:alexandria)
  (:export #:validate
           #:make-validator
           ;; conditions
           #:validation-fail
           #:stop-validation))