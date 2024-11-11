;;;; testsuite-build.lisp — Test Build Support for El Cid

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

(defun list-docker-images (&key docker-engine)
  (check-type docker-engine (or cid:docker-engine null))
  (uiop:run-program
   (append
    '("docker")
    (when docker-engine
      (list "--context" (slot-value docker-engine 'cid::context)))
    (list "image" "list" "--no-trunc" "--format" "{{.Repository}}:{{.Tag}}"))
   :output :lines))

(define-testcase ensure-that-image-exists (image &key docker-engine)
  (check-type docker-engine (or cid:docker-engine null))
  (assert-t* (member (build:image-name image)
		     (list-docker-images :docker-engine docker-engine)
		     :test #'string=)))

(define-testcase ensure-that-image-does-not-exist (image &key docker-engine)
  (check-type docker-engine (or cid:docker-engine null))
  (assert-nil (member (build:image-name image)
		      (list-docker-images :docker-engine docker-engine)
		      :test #'string=)))

(define-testcase ensure-that-image-is-valid (image &key docker-engine)
  (check-type docker-engine (or cid:docker-engine null))
  (assert-t (build:validate-image image :docker-engine docker-engine)))

(define-testcase ensure-that-image-can-be-built (image)
  (let ((docker-engine
	  (cid:make-docker-engine :context "colima-laboratory")))
    (build:build-image image :docker-engine docker-engine)
    (ensure-that-image-exists image :docker-engine docker-engine)
    (ensure-that-image-is-valid image :docker-engine docker-engine)))

(define-testcase ensure-that-every-image-can-be-built ()
  (loop :for image :in (enumerate-images :tag (string-downcase confidence:*testsuite-id*))
	:do (ensure-that-image-can-be-built image)))

(define-testcase build-component-test ()
  (ensure-that-every-image-can-be-built))

;;;; End of file `testsuite-build.lisp'
