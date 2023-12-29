;;;; entry-point.lisp — Entrypoint for El Cid

;;;; El Cid (https://github.com/melusina-org/cid)
;;;; This file is part of El Cid.
;;;;
;;;; Copyright © 2015–2023 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(in-package #:org.melusina.cid/testsuite)

(define-testcase unit-tests ()
  (testsuite-database)
  (testsuite-tenant)
  (testsuite-project))

(define-testcase component-tests ()
  (validate-docker-volume-lifecycle)
  (validate-docker-image-lifecycle)
  (ensure-that-every-image-can-be-built)
  (validate-project-lifecycle))

(define-testcase integration-tests ()
  (assert-t t))

(define-testcase run-all-tests ()
  (unit-tests)
  (component-tests)
  (integration-tests))

;;;; End of file `entry-point.lisp'
