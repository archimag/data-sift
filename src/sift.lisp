;;;; sift.lisp
;;;;
;;;; This file is part of the DATA-SIFT library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:data-sift)

(defgeneric compile-parse-rule (rule &key &allow-other-keys)
  (:documentation "Compile a validator according to RULE"))

(defgeneric compile-render-rule (rule &key &allow-other-keys)
  (:documentation "Compile a renderer according to RULE"))

(defun default-renderer (obj)
  (if (stringp obj)
      obj
      (write-to-string obj)))

(defmethod compile-render-rule (rule &key &allow-other-keys)
  #'default-renderer)

;;; list rule

(defmethod compile-parse-rule ((rule cons) &key)
  (apply #'compile-parse-rule rule))

(defmethod compile-format-rule ((rule cons) &key)
  (apply #'compile-format-rule rule))

;;; unknow rule

(defmethod compile-parse-rule ((symbol symbol) &rest args &key &allow-other-keys)
  (error 'invalid-rule
         :rule (cons symbol args)
         :message "Unknow rule"))

(defmethod compile-format-rule ((symbol symbol) &rest args &key &allow-other-keys)
  (error 'invalid-rule
         :rule (cons symbol args)
         :message "Unknow rule"))

;;; functional rule

(defmethod compile-parse-rule ((rule function) &key &allow-other-keys)
  rule)

;;; string rule

(defun strip (str)
  (string-trim #(#\Tab #\Space #\Newline #\Return #\Linefeed) str))

(defmethod compile-parse-rule ((rule (eql 'string))
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

(defmethod compile-parse-rule ((rule (eql 'integer)) &key min-value max-value message)
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

(defmethod compile-parse-rule ((rule (eql 'number)) &key min-value max-value message)
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

(defmethod compile-parse-rule ((rule (eql 'regexp))
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

(defmethod compile-parse-rule ((rule (eql 'email)) &key message)
  (compile-parse-rule 'regexp 
                :regex *re-email-check*
                :message (or message "Doesn't look like a valid email.")))

;;;; ip-address rule

;; (defmethod compile-parse-rule ((rule (eql 'ip-address)) &key message)
;;   (compile-parse-rule 'regexp 
;;                 :regex "^([0-9]{1,3}\.){3}[0-9]{1,3}$"
;;                 :message (or message "Invalid email address")))


;;;; url rule

;; (defmethod compile-parse-rule ((rule (eql 'url)) &key (require-tld t) message)
;;   (named-lambda url-validator (value)
;;     (let ((url (puri:parse-uri value)))

;;;; any-of rule

;;(defmethod compile-parse-rule ((rule (eql 'any-of)) &key values message)      

;;;; none-of rule

;;(defmethod compile-parse-rule ((rule (eql 'none-of)) &key values message)      

;; ;;;; required rule

(defmethod compile-parse-rule ((rule (eql 'required)) &key message)
  (named-lambda required-validator (value)
    (when (or (null value) (emptyp (strip value)))
      (vfail message "A value is required."))
    value))
