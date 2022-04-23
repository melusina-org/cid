;;;; resource.lisp — Projects for El Cid

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

(define-testcase ensure-provider-mismatch-is-discovered ()
  "Ensure that resource mismatch is discovered when a resource is made.
This testcase makes a resource with a provider which is unable to
actually create the resource.  This ensures that the object validation
triggers a resource error."
  (with-empty-providers
    (assert-condition
	(cid:make-memory-text
	 :pathname *testsuite-name*
	 :description "A sample text"
	 :provider (cid:make-null-provider)
	 :text "The actual sample text.")
	error)))

(define-testcase journey-resource-create-delete (resource)
  "This journey creates and delete a RESOURCE with its provider.
This journey coinfigures the provider and then create and delete the RESOURCE."
  (assert-nil (cid:configure-provider (slot-value resource 'cid:provider)))
  (assert-eq resource (cid:create-resource resource))
  (assert-eq resource (cid:delete-resource resource)))

(define-testcase journey-resource-create-read-update-delete (resource slot-name slot-value)
  "This journey creates, updates and deletes a RESOURCE with its provider.
This journey coinfigures the provider and then create and delete the RESOURCE."
  (assert-nil (cid:configure-provider (slot-value resource 'cid:provider)))
  (assert-eq resource (cid:create-resource resource))
  (assert-eq resource (cid:read-resource resource))
  (setf (slot-value resource slot-name) slot-value)
  (assert-eq resource (cid:update-resource resource))
  (assert-eq resource (cid:read-resource resource))
  (assert-eq resource (cid:delete-resource resource)))

(define-testcase testsuite-resource ()
  (ensure-provider-mismatch-is-discovered))

;;;; End of file `resource.lisp'
