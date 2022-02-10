;;;; trac.lisp — Trac integration

;;;; El Cid (https://github.com/melusina-conseil/cid)
;;;; This file is part of El Cid.
;;;;
;;;; Copyright © 2017–2022 Michaël Le Barbier
;;;; All rights reserved.

;;;; This software is governed by the CeCILL-B license under French law and
;;;; abiding by the rules of distribution of free software.  You can  use, 
;;;; modify and/ or redistribute the software under the terms of the CeCILL-B
;;;; license as circulated by CEA, CNRS and INRIA at the following URL
;;;; "https://cecill.info/licences/Licence_CeCILL-B_V1-en.txt"

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
    (assert-t (find *trac-environment* (cid:trac-list-environments) :test #'string-equal))))

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
      (assert-t (find username (cid:trac-list-users *trac-environment*) :test #'string-equal)) 
      (cid:trac-delete-user *trac-environment* username)
      (assert-nil (find username (print (cid:trac-list-users *trac-environment*)) :test #'string-equal)))))

(define-testcase trac-acceptance ()
  (trac-operator-can-create-environment)
  (trac-operator-can-create-and-delete-user))

;;;; End of file `trac.lisp'
