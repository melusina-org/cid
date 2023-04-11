;;;; memory.lisp — Memory Provider for El Cid

;;;; El Cid (https://github.com/melusina-conseil/cid)
;;;; This file is part of El Cid.
;;;;
;;;; Copyright © 2017–2023 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(in-package #:org.melusina.cid/testsuite)

(define-testcase ensure-memory-is-a-singleton ()
  "Ensure the MEMORY is a singleton."
  (with-empty-providers
    (assert-eq nil (cid:find-provider :null))
    (assert-eq (cid:make-memory) (cid:make-memory))
    (assert-t* (cid:find-provider :null))
    (assert-eq (cid:make-memory) (cid:find-provider :null))))

(define-testcase testsuite-memory ()
  (ensure-memory-is-a-singleton))

(define-testcase integration-memory ()
  (with-empty-providers
    (let* ((memory-provider
	     (cid:make-memory))
	   (memory-text
	     (cid:make-memory-text :pathname "global.username"
				   :description "The username for global environment."
				   :provider memory-provider
				   :text "USER")))
      (journey-resource-create-delete memory-text)
      (journey-resource-create-read-update-delete memory-text 'cid:text "ALTUSER"))))

;;;; End of file `memory.lisp'
