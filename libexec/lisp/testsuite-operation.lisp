;;;; testsuite-operation.lisp — Test Operation Support for El Cid

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

(defun list-docker-volumes (&key docker-engine)
  (check-type docker-engine (or cid:docker-engine null))
  (uiop:run-program
   (append
    '("docker")
    (when docker-engine
      (list "--context" (slot-value docker-engine 'cid::context)))
    (list "volume" "list" "--format" "{{.Name}}"))
   :output :lines))

(define-testcase ensure-that-volume-exists (volume &key docker-engine)
  (assert-t* (member (slot-value volume 'cid::volume)
		     (list-docker-volumes :docker-engine docker-engine)
		     :test #'string=)))

(define-testcase ensure-that-volume-does-not-exist (volume &key docker-engine)
  (assert-nil (member (slot-value volume 'cid::volume)
		      (list-docker-volumes :docker-engine docker-engine)
		      :test #'string=)))

(define-testcase ensure-that-trac-instance-is-available (project instance)
  (let ((hostname
	  (operation:project-hostname project))
	(port
	  (operation:project-https-port project)))
    (labels ((trac-location (&rest location)
	       (apply
		#'concatenate
		'string
		"/trac/"
		instance
		(loop :for location-part :in location
		      :collect "/"
		      :collect location-part)))
	     (validate-location-1 (trac-location http-body http-status)
	       (with-http-reply
		   ((trac-location trac-location)
		    :hostname hostname
		    :port port
		    :verify nil)
		 (assert-http-status http-status)
		 (assert-http-body http-body)))
	     (validate-location (trac-location http-body &optional (http-status 200))
	       (handler-bind ((stream-error
				#'restart-with-http-sleep-and-retry)
			      #+sbcl
			      (usocket:invalid-argument-error 
				#'restart-with-http-sleep-and-retry))
		 (validate-location-1 trac-location http-body http-status))))
      (loop :for (trac-location . http-body)
	    :in '(("wiki" .  "Welcome to Trac")
		  ("timeline" . "Timeline")
		  ("roadmap" . "Roadmap")
		  ("report" . "Available Reports")
		  ("search" . "Search"))
	    :do (validate-location trac-location http-body)))))

(define-testcase validate-project-lifecycle ()
  (let* ((name
	   (string-downcase confidence:*testsuite-id*))
	 (tag
	   (string-downcase confidence:*testsuite-id*))
	 (random-unprivileged-port
	   (let ((first-unprivilieged-port 1024)
		 (first-invalid-port 65536))
	     (+ first-unprivilieged-port
		(random (- first-invalid-port first-unprivilieged-port)))))
	 (project
	   (operation:make-project
	    :name name
	    :tag tag
	    :hostname "localhost"
	    :http-port random-unprivileged-port
	    :https-port (+ 1 random-unprivileged-port)
	    :ssh-port (+ 2 random-unprivileged-port)))
	 (docker-engine
	   (operation:project-docker-engine project)))
    (development:build :tag tag :docker-engine docker-engine)
    (loop :for image :in (enumerate-images :tag tag)
	  :do (progn
		(ensure-that-image-exists image :docker-engine docker-engine)
		(ensure-that-image-is-valid image :docker-engine docker-engine)))
    (operation:create-project :project project)
    (loop :for volume :in (enumerate-volumes :project project)
	  :do (ensure-that-volume-exists volume :docker-engine docker-engine))
    (ensure-that-trac-instance-is-available project "example1")
    (ensure-that-trac-instance-is-available project "example-2-fancy-name")
    (handler-bind
	((cid:resource-no-longer-exists #'continue))
      (operation:delete-project project))
    (loop :for volume :in (enumerate-volumes :project project)
	  :do (ensure-that-volume-does-not-exist volume :docker-engine docker-engine))))

(define-testcase project-integration-test ()
  (validate-project-lifecycle))

;;;; End of file `testsuite-operation.lisp'
