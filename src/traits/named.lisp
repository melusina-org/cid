;;;; named.lisp — Named Trait

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

(clsql:def-view-class named-trait ()
  ((name
    :db-kind :key
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

(defun check-that-string-is-in-the-portable-filename-character-set (string)
  "Check that STRING is in the portable filename character set."
  (labels ((portable-char-p (char)
	     (or (alpha-char-p char)
		 (digit-char-p char)
		 (position char "-_"))))
    (unless (every #'portable-char-p string)
      (error "The string ~A does not consist of characters form the portable set." string))))

(defmethod initialize-instance :after ((instance named-trait) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (flet ((finalize-name-slot ()
	   (when (slot-boundp instance 'name)
	     (with-slots (name) instance
	       (check-that-string-is-in-the-portable-filename-character-set name)))))
    (finalize-name-slot)))

(defmethod print-object ((instance named-trait) stream)
  (flet ((name-slots-boundp ()
	   (and (slot-boundp instance 'name)
		(slot-boundp instance 'displayname)))
	 (print-name-slots ()
	   (with-slots (name displayname) instance
	     (format stream "~A ~A" name displayname)))
	 (print-unavailable-name ()
	   (write-string "No name available" stream)))
    (print-unreadable-object (instance stream :type t :identity t)
      (if (name-slots-boundp) (print-name-slots) (print-unavailable-name)))))

;;;; End of file `named.lisp'
