;;;; package.lisp — Package for El Cid tests

;;;; El Cid (https://github.com/melusina-org/cid)
;;;; This file is part of El Cid.
;;;;
;;;; Copyright © 2015–2024 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(defpackage #:org.melusina.cid/testsuite
  (:use #:common-lisp)
  (:local-nicknames
   (#:cid #:org.melusina.cid)
   (#:poc #:org.melusina.cid/poc)
   (#:console #:org.melusina.cid/console)
   (#:build #:org.melusina.cid/build)
   (#:development #:org.melusina.cid/development)
   (#:operation #:org.melusina.cid/operation))
  (:export
   #:connect-database
   #:disconnect-database
   #:build-component-test
   #:component-tests
   #:database-unit-test
   #:demonstrate-that-infrastructure-errors-can-be-resumed
   #:demonstrate-that-infrastructure-stack-can-be-created-and-destroyed
   #:demonstrate-that-infrastructure-stack-can-be-modified
   #:demonstrate-that-infrastructure-stack-can-be-persisted
   #:demonstrate-that-infrastructure-stacks-can-be-promoted-through-environments
   #:demonstrate-that-infrastructure-stacks-cannot-be-misadvertently-duplicated
   #:demonstrate-that-infrastructure-stacks-modification-can-be-reviewed-before-being-committed
   #:docker-component-test
   #:docker-engine-unit-test
   #:ensure-that-delete-signals-resource-no-longer-exists
   #:ensure-that-every-image-can-be-built
   #:ensure-that-image-can-be-built
   #:ensure-that-image-does-not-exist
   #:ensure-that-image-exists
   #:ensure-that-image-is-valid
   #:ensure-that-infrastructure-stack-can-be-created-and-destroyed
   #:ensure-that-infrastructure-stack-can-be-modified
   #:ensure-that-infrastructure-stack-can-be-persisted
   #:ensure-that-not-created-resources-cannot-be-deleted
   #:ensure-that-resources-are-created-only-once
   #:ensure-that-trac-instance-is-available
   #:ensure-that-volume-does-not-exist
   #:ensure-that-volume-exists
   #:integration-tests
   #:local-filesystem-subtree-unit-test
   #:project-integration-test
   #:project-unit-test
   #:resource-unit-test
   #:run-all-tests
   #:simulator-unit-test
   #:steward-component-test
   #:steward-unit-test
   #:tenant-unit-test
   #:unit-tests
   #:validate-docker-image-lifecycle
   #:validate-docker-volume-lifecycle
   #:validate-poc
   #:validate-project-lifecycle
   #:verify-resource-lifecycle-invariants
   #:verify-steward-resource-relationships)
  (:import-from
   #:org.melusina.confidence
   #:define-testcase
   #:define-assertion
   #:assert<
   #:assert-t
   #:assert-t*
   #:assert-nil
   #:assert-eq
   #:assert-equal
   #:assert-equalp
   #:assert-set-equal
   #:assert-string=
   #:assert-string-match
   #:assert-type
   #:assert-condition
   #:*testsuite-id*))

(in-package #:org.melusina.cid/testsuite)

;;;; End of file `package.lisp'
