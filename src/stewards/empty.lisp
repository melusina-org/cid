;;;; empty.lisp — Empty Steward for El Cid

;;;; El Cid (https://github.com/melusina-org/cid)
;;;; This file is part of El Cid.
;;;;
;;;; Copyright © 2015–2023 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(in-package #:org.melusina.cid)

(clsql:def-view-class empty (steward)
  ((pathname
    :initform "empty"
    :type string)
   (steward-class
    :allocation :class
    :initform 'empty
    :type symbol)
   (description
    :allocation :class
    :initform "A steward that do not actually own resources."
    :type string))
  (:documentation
   "A steward for that do not actuall own resources.
For empty resources, every step of the lifecycle is a no-operation."))

(defun make-empty (&rest initargs &key tenant project)
  "Make a empty steward."
  (declare (ignore tenant project))
  (apply #'make-instance 'empty initargs))

;;;; End of file `empty.lisp'
