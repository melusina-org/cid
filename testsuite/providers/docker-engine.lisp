;;;; docker-engine.lisp — Docker Engine Provider for El Cid

;;;; El Cid (https://github.com/melusina-org/cid)
;;;; This file is part of El Cid.
;;;;
;;;; Copyright © 2017–2023 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(in-package #:org.melusina.cid/testsuite)

(define-testcase ensure-docker-engine-is-a-singleton ()
  "Ensure the argument-free DOCKER-ENGINE is a singleton."
  (with-empty-providers
    (assert-eq nil (cid:find-provider :docker-engine))
    (assert-eq (cid:make-docker-engine :pathname "docker-engine")
	       (cid:make-docker-engine))
    (assert-t* (cid:find-provider :docker-engine))
    (assert-eq (cid:make-docker-engine) (cid:find-provider :docker-engine))))

(define-testcase ensure-docker-engine-is-configurable ()
  "Ensure the DOCKER-ENGINE is configurable."
  (with-empty-providers
    (assert-eq nil (cid:find-provider :docker-engine))
    (let ((docker-engine
	    (cid:make-docker-engine)))
      (assert-nil (slot-value docker-engine 'cid::version))
      (cid:configure-provider docker-engine)
      (assert-t* (slot-value docker-engine 'cid::version)))))

(define-testcase integration-docker-engine ()
  (ensure-docker-engine-is-a-singleton)
  (ensure-docker-engine-is-configurable))

;;;; End of file `docker-engine.lisp'
