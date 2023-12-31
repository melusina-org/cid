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
  (database-unit-test)
  (tenant-unit-test)
  (project-unit-test)
  (steward-unit-test)
  (property-list-unit-test))

(define-testcase component-tests ()
  (steward-component-test)
  (docker-component-test)
  (build-component-test))

(define-testcase integration-tests ()
  (project-integration-test))

(define-testcase run-all-tests ()
  (unit-tests)
  (component-tests)
  (integration-tests))

;;;; End of file `entry-point.lisp'
