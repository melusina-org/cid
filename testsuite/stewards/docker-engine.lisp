;;;; docker-engine.lisp — Docker Engine Steward for El Cid

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

(define-testcase docker-engine-unit-test ()
  (with-test-database
    (populate-tenant-table)
    (populate-project-table)
    (populate-steward-tables)
    (let ((steward
	    (cid:find-steward "local-docker-engine"
			      :tenant "testsuite"
			      :project "testproject"
			      :steward-class 'cid:docker-engine)))
      (flet ((make-docker-volume ()
		 (cid:make-docker-volume
		  :docker-engine steward
		  :name "git"
		  :displayname "Git Repositories"
		  :description "A volume holding git repositories for the project."
		  :volume "git")))
	  (resource-unit-test
	   :resource-type 'cid:docker-volume
	   :make-resource #'make-docker-volume)))))

;;;; End of file `docker-engine.lisp'
