;;;; macos-security.lisp — Macos-Security Steward for El Cid

;;;; El Cid (https://github.com/melusina-org/cid)
;;;; This file is part of El Cid.
;;;;
;;;; Copyright © 2015–2024 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(in-package #:org.melusina.cid)

(clsql:def-view-class macos-security (steward)
  ((pathname
    :initform "macos-security"
    :type string)
   (steward-class
    :allocation :class
    :initform 'macos-security
    :type symbol)
   (description
    :allocation :class
    :initform "A steward that do not actually own resources."
    :type string))
  (:documentation
   "A steward for that do not actuall own resources.
For macos-security resources, every step of the lifecycle is a no-operation."))

(defun make-macos-security (&rest initargs &key tenant project)
  "Make a macos-security steward."
  (declare (ignore tenant project))
  (apply #'make-instance 'macos-security initargs))

;;;; End of file `macos-security.lisp'
