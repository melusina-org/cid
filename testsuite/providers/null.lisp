;;;; null.lisp — The null provider

;;;; El Cid (https://github.com/melusina-org/cid)
;;;; This file is part of El Cid.
;;;;
;;;; Copyright © 2017–2023 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

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
