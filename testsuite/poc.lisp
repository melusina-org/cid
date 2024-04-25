;;;; poc.lisp — Validation for the proof of Concept

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

(define-testcase demonstrate-that-infrastructure-stack-can-be-created-and-destroyed ()
  "Demonstrate that an infrastructure stack can be created and destroyed.
This testcase prepares an infrastructure stack value, then creates
the corresponding resources and deletes them.

This is the smallest possible testcase for an infrastructure stack."
  (assert-t nil))

(define-testcase demonstrate-that-infrastructure-stack-can-be-persisted ()
  "Demonstrate that an infrastructure stack can be persisted.
This testcase prepares an infrastructure stack value, then persist it
to a CLSQL database and read it back. This ensures that the state of
an infrastrcuture stack can be persisted, the lifespan of infrastructure
resources is usually longer than those of Common Lisp sessions."
  (assert-t nil))

(define-testcase demonstrate-that-infrastructure-stack-can-be-modified ()
  "Demonstrate that an infrastructure stack can be modified.
This testcase prepares an infrastructure stack value, then creates
the corresponding resources, modify an aspect of the stack and requires
the update of the stack."
  (assert-t nil))

(define-testcase demonstrate-that-infrastructure-errors-can-be-resumed ()
  "Demonstrate that an infrastructure errors can be resumed
This testcase prepares an infrastructure stack value, configures
the laboratory so that resource creation then creates
the corresponding resources, modify an aspect of the stack and requires
the update of the stack."
  (assert-t nil))

(define-testcase demonstrate-that-infrastructure-stacks-cannot-be-misadvertently-duplicated ()
  "When we are about to recreate a stack that has already been created,
this is detected early and the condition can be restarted in an appropriate way."
  (assert-t nil))

(define-testcase demonstrate-that-infrastructure-stacks-modification-can-be-reviewed-before-being-committed ()
  "When we are about to update the underlying resources of an infrastructure
stack, the changes about to be made can be reviewed before being performed.
This gives the operator the chance to interrupt the process if unexpected results are to be processed."
  (assert-t nil))

(define-testcase demonstrate-that-infrastructure-stacks-can-be-promoted-through-environments ()
  "Infrastructure stacks are promoted through environments.
An infrastructure stack has several instances and versions,
a given version can be promoted through computational environments
such as develelopment, staging, production."
  (assert-t nil))

(define-testcase validate-poc ()
  "Validate our proof of concept."
  (demonstrate-that-infrastructure-stack-can-be-created-and-destroyed)
  (demonstrate-that-infrastructure-stack-can-be-persisted)
  (demonstrate-that-infrastructure-stack-can-be-modified)
  (demonstrate-that-infrastructure-stacks-cannot-be-misadvertently-duplicated)
  (demonstrate-that-infrastructure-stacks-modification-can-be-reviewed-before-being-committed)
  (demonstrate-that-infrastructure-stacks-can-be-promoted-through-environments))

;;;; End of file `poc.lisp'