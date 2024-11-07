;;;; entry-point.lisp — Entrypoint for El Cid

;;;; El Cid (https://github.com/melusina-org/cid)
;;;; This file is part of El Cid.
;;;;
;;;; Copyright © 2015–2024 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(in-package #:org.melusina.cid/testsuite)

(define-testcase unit-tests ()
  (utilities-unit-test)
  (initialization-file-unit-test)
  (tenant-unit-test)
  (project-unit-test)
  (steward-unit-test)
  (simulator-unit-test)
  (local-filesystem-subtree-unit-test)
  (colima-unit-test)
  (progn
    (check-that-the-testsuite-runs-on-a-test-docker-context)
    (docker-engine-unit-test))
  (validate-poc))

(define-testcase component-tests ()
  (check-that-the-testsuite-runs-on-a-test-docker-context)
  (docker-component-test)
  (build-component-test)
  (keycloak-component-test))

(define-testcase integration-tests ()
  (check-that-the-testsuite-runs-on-a-test-docker-context)
  (project-integration-test))

(define-testcase run-all-tests ()
  (unit-tests)
  (component-tests)
  (integration-tests))

;;;; End of file `entry-point.lisp'
