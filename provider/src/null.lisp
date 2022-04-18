;;;; null.lisp — The null provider

;;;; El Cid (https://github.com/melusina-conseil/cid)
;;;; This file is part of El Cid.
;;;;
;;;; Copyright © 2017–2022 Michaël Le Barbier
;;;; All rights reserved.

;;;; This software is governed by the CeCILL-B license under French law and
;;;; abiding by the rules of distribution of free software.  You can  use,
;;;; modify and/ or redistribute the software under the terms of the CeCILL-B
;;;; license as circulated by CEA, CNRS and INRIA at the following URL
;;;; "https://cecill.info/licences/Licence_CeCILL-B_V1-en.txt"

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
