;;;; utilities.lisp — Utilities for El Cid tests

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

(defparameter *testsuite-name* "TESTSUITE"
  "The name for the testsuite.

Usually TESTSUITE but common values are ACCEPTANCE, INTEGRATION, PREFLIGHT, etc.")

(defparameter *testsuite-id* (cid:random-string 6)
  "A random identfier for the current testsuite run batch.")

(defun testsuite-database-name ()
  "The name of the testsuite database file."
  (concatenate 'string *testsuite-name* *testsuite-id*))

(defmacro with-test-database (&body body)
  `(let ((cid:*database-type*
	   :sqlite3)
	 (cid:*database-connection-spec*
	   (list (testsuite-database-name))))
     (unwind-protect
	  (progn (cid:connect-database) ,@body)
       (cid:disconnect-database)
       (clsql:destroy-database cid:*database-connection-spec* :database-type cid:*database-type*))))


;;;;
;;;; Test 
;;;;

(define-testcase validate-plist-sort ()
  (assert-list-equal '(:a 4 :b 3 :c 2)
		     (cid::sort-plist '(:c 2 :a 4 :b 3))))

(define-testcase testsuite-utilities ()
  (validate-plist-sort))

;;;; End of file `utilities.lisp'
