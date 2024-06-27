;;;; initialization-file.lisp — Initialization Files

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

(define-testcase verify-initialization-file-read-write-idempotency ()
  (labels ((check-configuration (configuration)
	     (check-structural-equality
	      configuration
	      (cid:read-initialization-file-from-string
	       (cid:write-initialization-file-to-string
		configuration))))
	   (example (name)
	     (system-relative-pathname
	      (make-pathname :name name
			     :type "ini"
			     :directory (list :relative "example" "initialization-file")))))
    (dolist (name '("example-1" "example-2" "example-3"))
      (check-configuration
       (cid:read-initialization-file (example name))))))


(define-testcase initialization-file-unit-test ()
  (verify-initialization-file-read-write-idempotency))

;;;; End of file `initialization-file.lisp'
