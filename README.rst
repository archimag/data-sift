DATA-SIFT is a Common Lisp data validation and transformation library.
Inspired by `cl-data-format-validation`_ and `WTForms validators`_.

Usage
-----

::

  CL-USER> (data-sift:sift '(integer :min-value 0 :max-value 20) "15")
  15
  CL-USER (funcall (data-sift:compile-rule '(number :min-value 0)) "123.456")
  123.456

Built-in validators
-------------------

* **string** (&key strip (min-length 0) max-length message)
* **integer** (&key min-value max-value message)
* **number** (&key min-value max-value message)
* **regexp** (&key regex message case-insensitive-mode multi-line-mode single-line-mode extended-mode)
* **email** (&key message)
* **ip-address** (&key message)

.. _WTForms validators: http://wtforms.simplecodes.com/docs/0.6.2/validators.html
.. _cl-data-format-validation: http://www.jarw.org.uk/lisp/cl-data-format-validation.html
