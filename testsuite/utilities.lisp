;;;; utilities.lisp — Utilities for El Cid tests

;;;; El Cid (https://github.com/melusina-conseil/cid)
;;;; This file is part of El Cid.
;;;;
;;;; Copyright © 2017–2023 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(in-package #:org.melusina.cid/testsuite)

(defun testsuite-database-name ()
  "The name of the testsuite database file."
  *testsuite-id*)

(defmacro with-test-database (&body body)
  `(let ((cid:*database-type*
	   :sqlite3)
	 (cid:*database-connection-spec*
	   (list (testsuite-database-name))))
     (unwind-protect
	  (progn (cid:connect-database) ,@body)
       (cid:disconnect-database)
       (clsql:destroy-database cid:*database-connection-spec* :database-type cid:*database-type*))))

(defmacro with-empty-providers (&body body)
  `(let ((cid::*providers*
	   (make-hash-table)))
     ,@body))



;;;;
;;;; Test 
;;;;

(define-testcase validate-plist-sort ()
  (assert-list-equal '(:a 4 :b 3 :c 2)
		     (cid::sort-plist '(:c 2 :a 4 :b 3))))

(define-testcase testsuite-utilities ()
  (validate-plist-sort))

;;;; End of file `utilities.lisp'
