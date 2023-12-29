;;;; steward.lisp — Resource Stewards for El Cid

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

(defun example-empty-steward ()
  "Some EMPTY steward that can be used in the testsuite."
  (cid:find-steward "empty"
		    :tenant "testsuite"
		    :project "testproject"
		    :steward-class 'cid:empty))

(defun example-property-list-steward ()
  "Some PROPERTY-LIST steward that can be used in the testsuite."
  (cid:find-steward "plist"
		    :tenant "testsuite"
		    :project "testproject"
		    :steward-class 'cid:property-list))

(defun example-filesystem-subtree-steward ()
  "Some FILESYSTEM-SUBTREE steward that can be used in the testsuite."
  (cid:find-steward "fs"
		    :tenant "testsuite"
		    :project "testproject"
		    :steward-class 'cid:filesystem-subtree))

(defun example-docker-engine-steward ()
  "Some DOCKER-ENGINE steward that can be used in the testsuite."
  (cid:find-steward "docker"
		    :tenant "testsuite"
		    :project "testproject"
		    :steward-class 'cid:docker-engine))

(defun example-macos-security-steward ()
  "Some MACOS-SECURITY steward that can be used in the testsuite."
  (cid:find-steward "secrets"
		    :tenant "testsuite"
		    :project "testproject"
		    :steward-class 'cid:macos-security))

(defparameter *example-steward-definitions*
  (list
   (list
    :key :empty
    :find-steward #'example-empty-steward
    :steward-class 'cid:empty
    :pathname "empty"
    :tenant "testsuite"
    :project "testproject")
   (list
    :key :property-list
    :find-steward #'example-property-list-steward
    :steward-class 'cid:property-list
    :pathname "plist"
    :tenant "testsuite"
    :project "testproject")
   (list
    :key :filesystem-subtree
    :find-steward #'example-filesystem-subtree-steward
    :steward-class 'cid:filesystem-subtree
    :pathname "fs"
    :tenant "testsuite"
    :project "testproject")
   (list
    :key :docker-engine
    :find-steward #'example-docker-engine-steward
    :steward-class 'cid:docker-engine
    :pathname "docker"
    :tenant "testsuite"
    :project "testproject")
   (list
    :key :macos-security
    :find-steward #'example-macos-security-steward
    :steward-class 'cid:macos-security
    :pathname "secrets"
    :tenant "testsuite"
    :project "testproject"))
  "Some steward definitions that can be used in the testsuites.")

(defun select-steward-examples-by-key (&optional key)
  (unless key
    (return-from select-steward-examples-by-key
      *example-steward-definitions*))
  (setf key (alexandria:ensure-list key))
  (flet ((steward-example-is-selected-p (example)
	   (member (getf example :key) key)))
    (remove-if-not
     #'steward-example-is-selected-p
     *example-steward-definitions*)))

(defun populate-steward-tables ()
  "Populate the TENANT table with some test data."
  (flet ((make-steward (&key key find-steward steward-class pathname tenant project)
	   (declare (ignore key find-steward))
	   (make-instance steward-class
			  :pathname pathname
			  :tenant tenant
			  :project project)))
    (loop :for example :in *example-steward-definitions*
	  :for steward = (apply #'make-steward example)
	  :do (clsql:update-records-from-instance steward))))

(define-testcase ensure-that-steward-joined-slots-are-set (instance)
  (assert-t (slot-boundp instance 'cid:tenant-pathname))
  (assert-t (slot-boundp instance 'cid:project-pathname))
  (assert-t* (slot-value instance 'cid:tenant))
  (assert-t* (slot-value instance 'cid:project)))

(define-testcase ensure-that-find-steward-produces-an-acceptable-result (&key key find-steward pathname steward-class tenant project)
  (let ((steward (funcall find-steward)))
    (assert-type steward 'cid:steward)
    (assert-type steward steward-class)
    (ensure-that-steward-joined-slots-are-set steward)))

(define-testcase ensure-that-configure-steward-returns-the-instance (steward)
  (assert-eq steward (cid:configure-steward steward)))

(define-testcase ensure-that-steward-can-be-configured (&key key find-steward pathname steward-class tenant project)
  (let ((steward (funcall find-steward)))
    (assert-type steward 'cid:steward)
    (assert-type steward steward-class)
    (with-slots ((instance-pathname cid::pathname)
		 (instance-project cid::project-pathname)
		 (instance-tenant cid::tenant-pathname))
	steward
      (assert-string= pathname instance-pathname)
      (assert-string= project instance-project)
      (assert-string= tenant instance-tenant)
      (ensure-that-configure-steward-returns-the-instance steward))))

(define-testcase testsuite-steward (&optional key)
  (with-test-database
    (populate-tenant-table)
    (populate-project-table)
    (populate-steward-tables)
    (loop :for example :in (select-steward-examples-by-key key)
	  :do (apply #'ensure-that-find-steward-produces-an-acceptable-result example))))

(define-testcase component-test-steward (&optional key)
  (with-test-database
    (populate-tenant-table)
    (populate-project-table)
    (populate-steward-tables)
    (loop :for example :in (select-steward-examples-by-key key)
	  :do (apply #'ensure-that-steward-can-be-configured example))))

;;;; End of file `steward.lisp'
