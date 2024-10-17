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
  (with-test-environment
    (populate-tenant-table)
    (populate-project-table)
    (let* ((steward
	     (cid:make-docker-engine
	      :tenant "testsuite"
	      :project "testproject"
	      :name "local-docker-engine"))
	   (port-base
	     (+ 16384 (random 32768)))
	   (docker-project-volumes
	     (flet ((volume-name (string)
		      (format nil "cid-~A-~A"
			      (string-downcase *testsuite-id*)
			      string)))
	       (list
		(cid:make-docker-volume
		 :docker-engine steward
		 :name "ssl"
		 :displayname "SSL"
		 :description "Docker volume holding SSL artifacts."
		 :volume (volume-name "ssl"))
		(cid:make-docker-volume
		 :docker-engine steward
		 :name "git"
		 :displayname "GIT"
		 :description "Docker volume holding git repositories."
		 :volume (volume-name "git"))
		(cid:make-docker-volume
		 :docker-engine steward
		 :name "www"
		 :displayname "WWW"
		 :description "Docker volume holding Apache files."
		 :volume (volume-name "www"))
		(cid:make-docker-volume
		 :docker-engine steward
		 :name "trac"
		 :displayname "Trac"
		 :description "Docker volume holding Trac data."
		 :volume (volume-name "trac"))))))
      (flet ((make-docker-volume ()
	       (cid:make-docker-volume
		:docker-engine steward
		:name "git"
		:displayname "Git Repositories"
		:description "A volume holding git repositories for the project."
		:volume "git"))
	     (make-docker-project ()
	       (cid:make-docker-project
		:docker-engine steward
		:name "test"
		:displayname "Test Project"
		:description "A volume holding git repositories for the project."
		:project (string-downcase *testsuite-id*)
		:pathname (system-relative-pathname #p"docker/compose/cid.yml")
		:volumes docker-project-volumes
		:environment
		(list
		 (cons "cid_hostname"
		       (format nil "~A.cid.melusina.local"
			       (string-downcase *testsuite-id*)))
		 (cons "cid_http_port"
		       (write-to-string (+ port-base 80)))
		 (cons "cid_https_port"
		       (write-to-string (+ port-base 443)))
		 (cons "cid_ssh_port"
		       (write-to-string (+ port-base 22)))
		 (cons "cid_image_tag"
		       (string-downcase *testsuite-id*))
		 (cons "cid_project"
		       (string-downcase *testsuite-id*))))))
	(resource-unit-test
	 :resource-type 'cid:docker-volume
	 :make-resource #'make-docker-volume)
	(unwind-protect
	     (progn
	       (development:build
		:tag (string-downcase *testsuite-id*))
	       (dolist (volume docker-project-volumes)
		 (cid:create-resource volume))
	       (resource-unit-test
		:resource-type 'cid:docker-project
		:make-resource #'make-docker-project))
	  (dolist (volume docker-project-volumes)
	    (cid:delete-resource volume)))))))

;;;; End of file `docker-engine.lisp'
