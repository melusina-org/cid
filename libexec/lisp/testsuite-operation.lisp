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

(define-testcase ensure-that-volume-exists (volume)
  (assert-t* (docker:find-volume volume)))

(define-testcase ensure-that-volume-does-not-exist (volume)
  (assert-nil (docker:find-volume (docker:volume-name volume))))

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
	   (operation:create-project
	    :name name
	    :tag tag
	    :hostname "localhost"
	    :http-port random-unprivileged-port
	    :https-port (+ 1 random-unprivileged-port)
	    :ssh-port (+ 2 random-unprivileged-port))))
    (development:build :tag tag)
    (loop :for image :in (enumerate-images :tag tag)
	  :do (progn
		(ensure-that-image-exists image)
		(ensure-that-image-is-valid image)))
    (loop :for volume :in (enumerate-volumes :project name)
	  :do (ensure-that-volume-exists volume))
    (assert-t* (operation:find-project name))
    (setf (operation:project-configuration "project.hostname" project)
	  "localhost")
    (operation:configure-project project)
    (operation:start-project project)
    (ensure-that-trac-instance-is-available project "example1")
    (ensure-that-trac-instance-is-available project "example-2-fancy-name")
    (operation:stop-project project)
    (operation:delete-project project)
    (loop :for volume :in (enumerate-volumes :project name)
	  :do (ensure-that-volume-does-not-exist volume))
    (loop :for image :in (enumerate-images :tag tag)
	  :do (progn
		(docker:delete-image
		 (docker:find-image
		  (build:image-name image)))
		(ensure-that-image-does-not-exist image)))))

(define-testcase project-integration-test ()
  (validate-project-lifecycle))

;;;; End of file `testsuite-operation.lisp'
