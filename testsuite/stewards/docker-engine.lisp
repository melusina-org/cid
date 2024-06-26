;;;; docker-engine.lisp — Docker Engine Steward for El Cid

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
		  :volume "git"))
	     (make-docker-image ()
	       (let ((tag confidence:*testsuite-id*))
		 (cid:make-docker-image
		  :docker-engine steward
		  :name "linux"
		  :displayname "Linux docker image"
		  :description "A docker image holding a basic Linux system."
		  :repository "cid"
		  :tag tag
		  :dockerfile (system-relative-pathname #p"docker/image/linux/Dockerfile")
		  :context (system-relative-pathname #p"./")
		  :build-time-variables (list (cons "CID_LINUX_REFERENCE" tag))))))
	(resource-unit-test
	 :resource-type 'cid:docker-volume
	 :make-resource #'make-docker-volume)
	#+org.melusina.cid/experimental
	(resource-unit-test
	 :resource-type 'cid:docker-image
	 :make-resource #'make-docker-image)))))

;;;; End of file `docker-engine.lisp'
