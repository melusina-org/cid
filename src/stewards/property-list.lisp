;;;; property-list.lisp — Property-List Steward for El Cid

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

(clsql:def-view-class property nil
  ((property-list-id
    :db-kind :key
    :db-constraints :not-null
    :type integer)
   (name
    :type keyword
    :db-kind :key
    :db-constraints :not-null
    :initarg :name
    :accessor property-name)
   (value
    :type string
    :initarg :value
    :accessor property-value)))

(clsql:def-view-class property-list (steward)
  ((pathname
    :initform "property-list"
    :type string)
   (steward-class
    :allocation :class
    :initform 'property-list
    :type symbol)
   (description
    :allocation :class
    :initform "A steward that do not actually own resources."
    :type string)
   (properties
    :db-kind :join
    :db-info (:join-class property
	      :home-key stewardid
	      :foreign-key property-list-id
	      :set t)))
  (:documentation
   "A steward for that do not actuall own resources.
For property-list resources, every step of the lifecycle is a no-operation."))

(defun make-property-list (&rest initargs &key tenant project)
  "Make a property-list steward."
  (declare (ignore tenant project))
  (apply #'make-instance 'property-list initargs))

;;;; End of file `property-list.lisp'
