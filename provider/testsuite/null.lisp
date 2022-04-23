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

(in-package #:org.melusina.cid/testsuite)

(define-testcase ensure-null-provider-is-a-singleton ()
  "Ensure the NULL-PROVIDER is a singleton."
  (with-empty-providers
    (assert-eq nil (cid:find-provider :null))
    (assert-eq (cid:make-null-provider) (cid:make-null-provider))
    (assert-t* (cid:find-provider :null))
    (assert-eq (cid:make-null-provider) (cid:find-provider :null))))

(define-testcase testsuite-null-provider ()
  (ensure-null-provider-is-a-singleton))

;;;; End of file `null.lisp'
