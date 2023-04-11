;;;; entrypoint.lisp — Entrypoint for El Cid Testsuite

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

(define-testcase run-unit-tests ()
  (let ((*testsuite-name* "UNIT"))
    (testsuite-utilities)
    (testsuite-database)
    (testsuite-tenant)
    (testsuite-identity)
    (testsuite-project)
    (testsuite-provider)
    (testsuite-resource)))

(define-testcase run-acceptance-tests ()
  (let ((*testsuite-name* "ACCEPTANCE"))
    (trac-acceptance)))

(define-testcase run-integration-tests ()
  (integration-provider))

(define-testcase run-all-tests ()
  (run-unit-tests)
  (run-integration-tests))

;;;; End of file `entrypoint.lisp'
