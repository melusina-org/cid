;;;; keycloak.lisp — Keycloak Steward for El Cid

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

(define-testcase keycloak-component-test ()
  (with-test-environment
    (populate-tenant-table)
    (populate-project-table)
    (let* ((administrator-username
	     "Administrator")
	   (administrator-password
	     (ironclad:byte-array-to-hex-string
	      (ironclad:random-data 32)))
	   (port
	     (+ 16384 (random 32768)))
	   (container-name
	     (concatenate 'string *testsuite-id* "-keycloak"))
	   (location
	     (format nil "http://localhost:~A/" port))
	   (keycloak-admin
	     (cid:make-keycloak-admin
	      :tenant "testsuite"
	      :project "testproject"
	      :name "local-keycloak-admin"
	      :location location
	      :username administrator-username
	      :password administrator-password)))
      (flet ((start-keycloak-container ()
	       (flet ((publish (&key local-port container-port)
			(format nil "~A:~A" local-port container-port))
		      (env (&key name value)
			(format nil "~A=~A" name value)))
		 (uiop:run-program
		  (list
		   "docker" "run"
		   "--detach"
		   "--publish" (publish :local-port port :container-port 8080)
		   "--rm"
		   "--env" (env :name "KEYCLOAK_ADMIN"
				:value administrator-username)
		   "--env" (env :name "KEYCLOAK_ADMIN_PASSWORD"
				:value administrator-password)
		   "--name" container-name
		   "quay.io/keycloak/keycloak:25.0.2"
		   "start-dev")
		  :output :string)))
	     (wait-keycloak-container-is-ready ()
	       (flet ((ping ()
			(ignore-errors
			 (drakma:http-request location)
			 (return-from ping t))
			(sleep 2)
			nil))
		 (loop :until (ping))))
	     (stop-keycloak-container ()
		 (uiop:run-program
		  (list
		   "docker" "stop" container-name)
		  :output :string))
	     (make-keycloak-realm ()
	       (cid:make-keycloak-realm
		:keycloak-admin keycloak-admin
		:name "test"
		:displayname "Test Realm"
		:description (format nil "A test realm used by testsuite ~A." *testsuite-id*)
		:realm "test"
		:edit-username :deny)))
      (macrolet ((with-keycloak-container (&body forms)
		   `(progn
		      (start-keycloak-container)
		      (wait-keycloak-container-is-ready)
		      (unwind-protect (progn ,@forms)			
			(stop-keycloak-container)))))
	(with-keycloak-container
	    (cid:configure-steward keycloak-admin)
	    (resource-unit-test
	     :resource-type 'cid:keycloak-realm
	     :make-resource #'make-keycloak-realm
	     :slot-name 'cid:edit-username
	     :new-slot-value :allow)))))))

;;;; End of file `docker-engine.lisp'
