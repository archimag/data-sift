;;;; sift.lisp
;;;;
;;;; This file is part of the DATA-SIFT library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:data-sift)

(defgeneric compile-rule (rule &key &allow-other-keys)
  (:documentation "Make function-validator according to RULE"))

(defun sift (rule value)
  "Validate and maybe transform VALUE according to RULE"
  (funcall (typecase rule
             (function rule)
             (symbol (compile-rule rule))
             (cons (apply #'compile-rule rule))
             (otherwise (error 'invalid-rule
                               :rule rule
                               :message "Unknow as handle rule")))
           value))

(defun vfail (messsage &rest args)
  (error 'validation-fail
         :message 
         (or messsage
             (apply #'format nil args))))

;;; list rule

(defmethod compile-rule ((rule cons) &key)
  (apply #'compile-rule rule))

;;; unknow rule

(defmethod compile-rule ((symbol symbol) &rest args &key &allow-other-keys)
  (error 'invalid-rule
         :rule (cons symbol args)
         :message "Unknow rule"))

;;; functional rule

(defmethod compile-rule ((rule function) &key &allow-other-keys)
  rule)

;;; string rule

(defun strip (str)
  (string-trim #(#\Tab #\Space #\Newline #\Return #\Linefeed) str))

(defmethod compile-rule ((rule (eql 'string))
                           &key strip (min-length 0) max-length message)
  (when min-length
    (check-type min-length integer))
  (when max-length
    (check-type max-length integer))
  (when (and max-length min-length (< max-length min-length))
    (error "MIN-LENGTH cannot be more than MAX-LENGTH"))
  
  (named-lambda string-validator (value)
    (unless (stringp value)
      (vfail "Value must be string"))
    (let* ((str (if strip (strip value) value))
           (len (length str)))
      (when (and min-length (< len min-length))
        (vfail message
               "Value must be at least ~A character long."
               min-length))
      (when (and max-length (> len max-length))
        (vfail message
               "Value cannot be longer than ~A character."
               max-length))
      str)))

;;; integer rule

(defmethod compile-rule ((rule (eql 'integer)) &key min-value max-value message)
  (when min-value
    (check-type min-value number))
  (when max-value
    (check-type max-value number))
  (when (and max-value min-value (< max-value min-value))
    (error "MIN-VALUE cannot be more than MAX-VALUE"))
  (named-lambda number-validator (value)
    (let ((val (typecase value
                 (string (handler-case 
                             (parse-integer value)
                           (error ()
                             (vfail "Invalid integer"))))
                 (number value)
                 (otherwise (vfail "Value must be string or integer")))))
      (when (and min-value (< val min-value))
        (vfail message
               "Value must be greater than ~A"
               min-value))
      (when (and max-value (> val max-value))
        (vfail message
               "Value must be less than ~A"
               max-value))
      val)))

;;; number rule

(defmethod compile-rule ((rule (eql 'number)) &key min-value max-value message)
  (when min-value
    (check-type min-value number))
  (when max-value
    (check-type max-value number))
  (when (and max-value min-value (< max-value min-value))
    (error "MIN-VALUE cannot be more than MAX-VALUE"))
  (named-lambda number-validator (value)
    (let ((val (typecase value
                 (string (handler-case 
                             (parse-number:parse-number value)
                           (parse-number:invalid-number ()
                             (vfail "Invalid number"))))
                 (number value)
                 (otherwise (vfail "Value must be string or number")))))
      (when (and min-value (< val min-value))
        (vfail message
               "Value must be greater than ~A"
               min-value))
      (when (and max-value (> val max-value))
        (vfail message
               "Value must be less than ~A"
               max-value))
      val)))

;;;; regexp rule

(defmethod compile-rule ((rule (eql 'regexp))
                           &key regex message
                           case-insensitive-mode multi-line-mode single-line-mode extended-mode)
  (let ((scanner (ppcre:create-scanner regex
                                       :case-insensitive-mode case-insensitive-mode
                                       :multi-line-mode multi-line-mode
                                       :single-line-mode single-line-mode
                                       :extended-mode extended-mode)))
    (named-lambda regex-validator (value)
      (unless (ppcre:scan scanner value)
        (vfail message
               "Invalid input"))
      value)))

;;;; email rule

(defparameter *re-email-check* 
  "^[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*@(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\\.)+[a-z0-9](?:[a-z]*[a-z])?$")

(defmethod compile-rule ((rule (eql 'email)) &key message)
  (compile-rule 'regexp 
                :regex *re-email-check*
                :message (or message "Invalid email address")))

;;;; ip-address rule

(defmethod compile-rule ((rule (eql 'ip-address)) &key message)
  (compile-rule 'regexp 
                  :regex "^([0-9]{1,3}\.){3}[0-9]{1,3}$"
                  :message (or message "Invalid email address")))


;;;; url rule

;; (defmethod compile-rule ((rule (eql 'url)) &key (require-tld t) message)
;;   (named-lambda url-validator (value)
;;     (let ((url (puri:parse-uri value)))

;;;; any-of rule

;;(defmethod compile-rule ((rule (eql 'any-of)) &key values message)      

;;;; none-of rule

;;(defmethod compile-rule ((rule (eql 'none-of)) &key values message)      



;;;; optional rule

;; (defmethod compile-rule ((rule (eql 'optional)) &key)
;;   (named-lambda optional-validator (value)
;;     (when (or (null value) (emptyp (strip rule)))
;;       (error 'stop-validation))
;;     value))


;; ;;;; required rule

;; (defmethod compile-rule ((rule (eql 'required)) &key message)
;;   (named-lambda required-validator (value)
;;     (unless (and value (not (emptyp (strip rule))))
;;       (vfail message "Value is required."))
;;     value))