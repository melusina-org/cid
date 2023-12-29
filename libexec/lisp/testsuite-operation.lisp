;;;; testsuite-operation.lisp — Test Operation Support for El Cid

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

(define-testcase ensure-that-volume-exists (volume)
  (assert-t* (docker:find-volume volume)))

(define-testcase ensure-that-volume-does-not-exist (volume)
  (assert-nil (docker:find-volume (docker:volume-name volume))))

(define-testcase ensure-that-trac-instance-is-available (project instance)
  (let ((hostname
	  (operation:project-hostname project))
	(port
	  (operation:project-http-port project)))
    (flet ((trac-location (&rest location)
	     (apply
	      #'concatenate
	      'string
	      "/trac/"
	      instance
	      (loop :for location-part :in location
		    :collect "/"
		    :collect location-part))))
      (with-http-reply
	  ((trac-location "wiki")
	   :hostname hostname
	   :port port)
	(assert-http-status 200)
	(assert-http-body "Welcome to Trac"))
      (with-http-reply
	  ((trac-location "timeline")
	   :hostname hostname
	   :port port)
	(assert-http-status 200)
	(assert-http-body "Timeline"))
      (with-http-reply
	  ((trac-location "roadmap")
	   :hostname hostname
	   :port port)
	(assert-http-status 200)
	(assert-http-body "Roadmap"))
      (with-http-reply
	  ((trac-location "report")
	   :hostname hostname
	   :port port)
	(assert-http-status 200)
	(assert-http-body "Available Reports"))
      (with-http-reply
	  ((trac-location "search")
	   :hostname hostname
	   :port port)
	(assert-http-status 200)
	(assert-http-body "Search")))))

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

;;;; End of file `testsuite-operation.lisp'
