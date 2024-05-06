;;;; simulator.lisp — Property List Stewards for El Cid

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

(define-testcase simulator-unit-test ()
  (with-test-environment 
    (populate-tenant-table)
    (populate-project-table)
    (let ((simulator
	    (cid:make-simulator :tenant "testsuite"
				:project "testproject"
				:name "simulator")))
      (flet ((make-simulation ()
	       (cid:make-simulation :simulator simulator
				    :displayname "Simulation Resource #1"
				    :description "A simulation resource used in the testsuite.")))
	(resource-unit-test
	 :resource-type 'cid:simulation
	 :make-resource #'make-simulation)))))

;;;; End of file `simulator.lisp'
