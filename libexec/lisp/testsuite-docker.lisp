;;;; testsuite-docker.lisp — Test Docker Support for El Cid

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

(define-testcase validate-docker-volume-lifecycle ()
  (let ((volume
	  nil)
	(name
	  (string-downcase confidence:*testsuite-id*)))
    (assert-nil (docker:find-volume name))
    (setf volume (docker:create-volume :name name))
    (assert-t* (docker:find-volume name))
    (docker:delete-volume volume)
    (assert-nil (docker:find-volume name))))

(define-testcase validate-docker-image-lifecycle ()
  (let ((image
	  nil)
	(tag
	  (string-downcase confidence:*testsuite-id*))
	(repository
	  "cid/validate-docker-image-lifecycle")
	(name
	  nil)
	(context
	  (system-relative-pathname))
	(dockerfile
	  (system-relative-pathname #p"./docker/image/linux/Dockerfile")))
    (setf name (uiop:strcat repository #\: tag))
    (assert-nil (docker:find-image name))
    (setf image (docker:create-image
		 :dockerfile dockerfile
		 :context context
		 :repository repository
		 :tag tag))
    (assert-t* (docker:image-id image))
    (assert-t* (docker:image-name image))
    (assert-t* (docker:find-image name))
    (docker:delete-image image)
    (assert-nil (docker:image-id image))
    (assert-nil (docker:find-image name))))

(define-testcase docker-component-test ()
  (validate-docker-volume-lifecycle)
  (validate-docker-image-lifecycle))

;;;; End of file `testsuite-docker.lisp'
