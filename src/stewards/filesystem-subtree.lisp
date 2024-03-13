;;;; filesystem-subtree.lisp — Filesystem Subtree Steward for El Cid

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

;;;;
;;;; Steward
;;;;

(clsql:def-view-class filesystem-subtree (steward)
  ((pathname
    :initform "filesystem-subtree"
    :type string)
   (steward-class
    :allocation :class
    :initform 'filesystem-subtree
    :type symbol)
   (description
    :allocation :class
    :initform "A steward which owns files in a specific filesystem subtree."
    :type string))
  (:documentation
   "A steward which owns files in a specific filesystem subtree.
In this subtree, text files can be created and updated."))

(defun make-filesystem-subtree (&rest initargs &key tenant project)
  "Make a filesystem-subtree steward."
  (declare (ignore tenant project))
  (apply #'make-instance 'filesystem-subtree initargs))


;;;;
;;;; Resources
;;;;

(clsql:def-view-class local-file (resource)
  ((steward-class
    :type symbol
    :initform 'filesystem-subtree
    :allocation :class)
   (filename
    :type pathname
    :initarg :filename)
   (mode
    :type int
    :initarg :mode
    :initform #o644)
   (content
    :type string
    :initarg :content)
   (checksum
    :type string))
  (:documentation
   "A file on the local filesystem."))

(defun make-local-file (&rest initargs &key filesystem-subtree pathname mode filename content)
  "Make a local file."
  (declare (ignore pathname mode filename content))
  (check-type filesystem-subtree filesystem-subtree)
  (apply #'make-instance 'local-file
	 :steward filesystem-subtree
	 (remove-property initargs :filesystem-subtree)))

;;;; End of file `filesystem-subtree.lisp'
