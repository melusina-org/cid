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

(define-testcase validate-project-lifecycle ()
  (let ((name
	  (string-downcase confidence:*testsuite-id*))
	(tag
	  (string-downcase confidence:*testsuite-id*)))
    (development:build :tag tag)
    (loop :for image :in (enumerate-images :tag tag)
	  :do (progn
		(ensure-that-image-exists image)
		(ensure-that-image-is-valid image)))
    (operation:create-project :name name)
    (loop :for volume :in (enumerate-volumes :project name)
	  :do (ensure-that-volume-exists volume))
    (let ((project
	    (operation:find-project name)))
      (assert-t* project)
      (operation:start-project project)
      (operation:stop-project project)
      (operation:delete-project project))
    (loop :for volume :in (enumerate-volumes :project name)
	  :do (ensure-that-volume-does-not-exist volume))
    (loop :for image :in (enumerate-images :tag tag)
	  :do (progn
		(docker:delete-image
		 (docker:find-image
		  (build:image-name image)))
		(ensure-that-image-does-not-exist image)))))

;;;; End of file `testsuite-operation.lisp'
