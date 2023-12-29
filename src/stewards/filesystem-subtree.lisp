;;;; filesystem-subtree.lisp — Filesystem Subtree Steward for El Cid

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
    :initform "A steward that do not actually own resources."
    :type string))
  (:documentation
   "A steward for that do not actuall own resources.
For filesystem-subtree resources, every step of the lifecycle is a no-operation."))

(defun make-filesystem-subtree (&rest initargs &key tenant project)
  "Make a filesystem-subtree steward."
  (declare (ignore tenant project))
  (apply #'make-instance 'filesystem-subtree initargs))

;;;; End of file `filesystem-subtree.lisp'
