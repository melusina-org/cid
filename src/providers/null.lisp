;;;; null.lisp — Null Provider for El Cid

;;;; El Cid (https://github.com/melusina-conseil/cid)
;;;; This file is part of El Cid.
;;;;
;;;; Copyright © 2017–2023 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(in-package #:org.melusina.cid)

(defclass null-provider (provider)
  ((pathname
    :initform "null"
    :documentation "A name for the PROVIDER.
The fully qualified name of the PROVIDER, which should be a safe Unix path.")
   (description
    :initform "A provider for null resources."
    :documentation "A short description of the PROVIDER."))
  (:documentation
   "A provider for null resources.
For null resources, every step of the lifecycle is a no-operation."))

(defun make-null-provider ()
  "Make a null provider."
  (or (find-provider :null)
      (make-instance 'null-provider)))

;;;; End of file `null.lisp'
