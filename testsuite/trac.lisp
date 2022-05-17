;;;; trac.lisp — Trac integration

;;;; El Cid (https://github.com/melusina-conseil/cid)
;;;; This file is part of El Cid.
;;;;
;;;; Copyright © 2017–2022 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(in-package #:org.melusina.cid/testsuite)

(defparameter *trac-environment* nil
  "The current trac environment.")

(defmacro with-test-trac-environment (&body body)
  `(let ((*trac-environment*
	   (concatenate 'string "TRAC" *testsuite-name* *testsuite-id*)))
     (unwind-protect
	  (progn (cid:trac-create-environment *trac-environment*) ,@body)
       (cid:trac-delete-environment *trac-environment*))))

(define-testcase trac-operator-can-create-environment ()
  "Ensure that the trac operator can create a trac environment."
  (with-test-trac-environment
      (assert-t (find *trac-environment* (cid:trac-list-environments)
		      :test #'string-equal))))

(define-testcase trac-operator-can-create-and-delete-user ()
  "Ensure that the trac operator can create a user."
  (let ((username
	  (cid:random-string 4 cid:*alphabet-hexadecimal*))
	(role
	  "admin")
	(password
	  (cid:random-string)))
    (with-test-trac-environment
      (cid:trac-create-user *trac-environment* username role password)
      (assert-t (find username (cid:trac-list-users *trac-environment*)
		      :test #'string-equal)) 
      (cid:trac-delete-user *trac-environment* username)
      (assert-nil (find username (cid:trac-list-users *trac-environment*)
			:test #'string-equal)))))

(define-testcase trac-acceptance ()
  (trac-operator-can-create-environment)
  (trac-operator-can-create-and-delete-user))

;;;; End of file `trac.lisp'
