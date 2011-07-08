;;;; suite.lisp
;;;;
;;;; This file is part of the DATA-SIFT library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(defpackage #:data-sift.test
  (:use #:cl #:lift #:data-sift)
  (:export #:run-data-sift-tests))

(in-package #:data-sift.test)

(deftestsuite data-sift-test () ())

(defun run-data-sift-tests ()
  (run-tests :suite 'data-sift-test :report-pathname nil))

(defmethod asdf:perform ((o asdf:test-op) (c (eql (asdf:find-system 'data-sift-test))))
  (let* ((test-results (run-data-sift-tests))
         (errors (lift:errors test-results))
         (failures (lift:failures test-results)))
    (if (or errors failures)
        (error "test-op failed: ~A"
               (concatenate 'list errors failures))
        (print test-results))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(addtest unknow-rule
  (ensure-condition invalid-rule
    (sift  '(myrule :val "hello world") "Hello world")))

(addtest string-rule
  (ensure-condition validation-fail
    (sift 'string 123))
  (ensure-same
   (sift '(string) "Hello world")
   "Hello world")
  (ensure-same
   (sift '(string :strip t) " Hello world   ")
   "Hello world")
  (ensure-different
   (sift '(string :strip nil) " Hello world   ")
   "Hello world")
  (ensure-same
   (sift '(string :min-length 10) "Hello world")
   "Hello world")
  (ensure-same
   (sift '(string :max-length 40) "Hello world")
   "Hello world")
  (ensure-condition validation-fail
    (sift '(string :min-length 20) "Hello world"))
  (ensure-condition validation-fail
    (sift '(string :max-length 10) "Hello world"))
  (ensure-condition error
    (sift '(string :min-length 20 :max-length 10) "Hello world")))

(addtest integer-rule
  (ensure-condition validation-fail
    (sift 'integer "hello"))
  (ensure-same
   (sift 'integer "123") 123)
  (ensure-different
   (sift 'integer "100") 123)
  (ensure-same
   (sift '(integer :min-value 10) "56") 56)
  (ensure-same
   (sift '(integer :max-value 100) "99") 99)
  (ensure-condition validation-fail
    (sift '(integer :min-value 20) "19"))
  (ensure-condition validation-fail
    (sift '(integer :max-value 95) 96)))

(addtest number-rule
  (ensure-condition validation-fail
    (sift 'number "hello"))
  (ensure-same
   (sift 'number "123.123") 123.123)
  (ensure-different
   (sift 'number "100") 100.123)
  (ensure-same
   (sift '(number :min-value 10) "56.123") 56.123)
  (ensure-same
   (sift '(number :max-value 100.12) "100") 100)
  (ensure-condition validation-fail
    (sift '(integer :min-value 20.12) "20.1"))
  (ensure-condition validation-fail
    (sift '(integer :max-value 45.23) 45.3)))

(addtest regexp-rule
  (ensure-same
   (sift '(regexp :regex "^a") "abcd")
   "abcd")
  (ensure-same
   (sift '(regexp :regex "^a" :case-insensitive-mode t) "ABcd")
   "ABcd")
  (ensure-condition validation-fail
   (sift '(regexp :regex "^a") "foo")))

(addtest email-rule
  (ensure-same
   (sift 'email "foo@bar.dk")
   "foo@bar.dk")
  (ensure-same
   (sift 'email "123@bar.dk")
   "123@bar.dk")
  (ensure-same
   (sift 'email "foo@456.dk")
   "foo@456.dk")
  (ensure-same
   (sift 'email "foo@bar456.info")
   "foo@bar456.info")
  (ensure-condition validation-fail
    (sift 'email ""))
  (ensure-condition validation-fail
    (sift 'email "foo"))
  (ensure-condition validation-fail
    (sift 'email "bar.dk"))
  (ensure-condition validation-fail
    (sift 'email "foo@"))
  (ensure-condition validation-fail
    (sift 'email "@bar.dk"))
  (ensure-condition validation-fail
    (sift 'email "foo@bar"))
  (ensure-condition validation-fail
    (sift 'email "foo@bar.ab121"))
  (ensure-condition validation-fail
    (sift 'email "foo@.bar.ab")))

(addtest ip-address-rule
  (ensure-same
   (sift 'ip-address "127.0.0.1")
   "127.0.0.1")
  (ensure-condition validation-fail
    (sift 'ip-address "abc.0.0.1"))
  (ensure-condition validation-fail
    (sift 'ip-address "1278.0.0.1"))
  (ensure-condition validation-fail
    (sift 'ip-address "127.0.0.abc")))
