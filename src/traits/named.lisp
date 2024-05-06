;;;; named.lisp — Named Trait

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

(defclass named-trait ()
  ((name
    :type string
    :initarg :name
    :reader name
    :documentation "The NAME of the instance.
It must consist of characters in the portable filename character set.")
   (displayname
    :accessor displayname
    :type string
    :initarg :displayname
    :documentation "The DISPLAYNAME is used in informational screens to denote the instance."))
  (:documentation "The NAMED-TRAIT provides methods and attributes for identifying
and describing an instance."))

(defmethod initialize-instance :after ((instance named-trait) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (flet ((finalize-name-slot ()
	   (when (slot-boundp instance 'name)
	     (with-slots (name) instance
	       (check-that-string-is-in-the-portable-filename-character-set name)))))
    (finalize-name-slot)))

;;;; End of file `named.lisp'
