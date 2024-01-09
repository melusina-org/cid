;;;; testsuite-build.lisp — Test Build Support for El Cid

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

(define-testcase ensure-that-image-exists (image)
  (let ((docker-image
	  (docker:find-image (build:image-name image))))
    (assert-t* docker-image)
    (assert-string= (build:image-name image)
		    (docker:image-name docker-image))))

(define-testcase ensure-that-image-does-not-exist (image)
  (let ((docker-image
	  (docker:find-image (build:image-name image))))
    (assert-nil docker-image)))

(define-testcase ensure-that-image-is-valid (image)
  (assert-t (build:validate-image image)))

(define-testcase ensure-that-image-can-be-built (image)
  (build:build-image image)
  (ensure-that-image-exists image)
  (ensure-that-image-is-valid image))

(define-testcase ensure-that-every-image-can-be-built ()
  (loop :for image :in (enumerate-images :tag (string-downcase confidence:*testsuite-id*))
	:do (ensure-that-image-can-be-built image)))

(define-testcase build-component-test ()
  (ensure-that-every-image-can-be-built))

;;;; End of file `testsuite-build.lisp'
