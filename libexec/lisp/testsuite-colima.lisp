;;;; testsuite-colima.lisp — Test Colima Support for El Cid

;;;; El Cid (https://github.com/melusina-org/cid)
;;;; This file is part of El Cid.
;;;;
;;;; Copyright © 2015–2023 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(require '#:org.melusina.confidence)
(require '#:org.melusina.cid/colima)

(defpackage #:org.melusina.cid/testsuite-colima
  (:use #:common-lisp)
  (:local-nicknames
   (#:colima #:org.melusina.cid/colima))
  (:import-from
   #:org.melusina.confidence
   #:define-testcase
   #:define-assertion
   #:assert-t
   #:assert-eq
   #:assert-set-equal
   #:assert-string=
   #:assert-type))

(in-package #:org.melusina.cid/testsuite-colima)

(define-testcase testsuite-architecture ()
  (assert-eq
   :aarch64
   (colima::architecture-of-string "aarch64"))
  (assert-string=
   "aarch64"
   (colima::architecture-to-string :aarch64)))

(define-testcase testsuite-lifecycle ()
  (let ((instance
	  (make-instance 'colima:instance
			 :name (string-downcase org.melusina.confidence:*testsuite-id*))))
    (assert-eq (colima:instance-status instance) nil)
    (colima:start-instance instance)
    (assert-eq (colima:instance-status instance) :running)
    (colima:stop-instance instance)
    (assert-eq (colima:instance-status instance) :stopped)
    (colima:delete-instance instance)
    (assert-eq (colima:instance-status instance) nil)))

;;;; End of file `testsuite-colima.lisp'
