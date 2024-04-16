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

(defun example-phony-steward ()
  "Some PHONY steward that can be used in the testsuite."
  (cid:find-steward "phony"
		    :tenant "testsuite"
		    :project "testproject"
		    :steward-class 'cid:phony-steward))

(defparameter *example-steward-definitions*
  (list
   (list
    :key :phony
    :steward-class 'cid:phony-steward
    :name "phony"
    :tenant "testsuite"
    :project "testproject"
    :make-steward nil))
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
  "Populate the STEWARD tables with some test data."
  (flet ((make-steward (&key key steward-class name tenant project make-steward)
	   (declare (ignore key))
	   (apply #'make-instance steward-class
		  :name name
		  :tenant tenant
		  :project project
		  make-steward)))
    (loop :for example :in *example-steward-definitions*
	  :for steward = (apply #'make-steward example)
	  :do (clsql:update-records-from-instance steward))))

(define-testcase steward-unit-test (&optional key)
  (flet ((ensure-that-find-steward-produces-an-acceptable-result (&key key name steward-class tenant project
								       make-steward)
	   (declare (ignore key make-steward))
	   (flet ((ensure-that-steward-has-expected-type (instance)
		    (assert-type instance 'cid:steward)
		    (assert-type instance steward-class))
		  (ensure-that-steward-joined-slots-are-set (instance)
		    (assert-t (slot-boundp instance 'cid::tenant-name))
		    (assert-t (slot-boundp instance 'cid::project-name))
		    (assert-t* (slot-value instance 'cid:tenant))
		    (assert-t* (slot-value instance 'cid:project)))
		  (ensure-that-steward-was-created-with-the-provided-values (instance)
		    (with-slots ((instance-name cid::name)
				 (instance-project cid::project-name)
				 (instance-tenant cid::tenant-name))
			instance
		      (assert-string= name instance-name)
		      (assert-string= project instance-project)
		      (assert-string= tenant instance-tenant))))
	     (let ((steward
		     (cid:find-steward name :tenant tenant :project project :steward-class steward-class)))
	       (ensure-that-steward-has-expected-type steward)
	       (ensure-that-steward-joined-slots-are-set steward)
	       (ensure-that-steward-was-created-with-the-provided-values steward)))))
    (with-test-database
      (populate-tenant-table)
      (populate-project-table)
      (populate-steward-tables)
      (loop :for example :in (select-steward-examples-by-key key)
	    :do (apply #'ensure-that-find-steward-produces-an-acceptable-result example)))))

(define-testcase steward-component-test (&optional key)
  (flet ((ensure-that-steward-can-be-configured (&key key name steward-class tenant project
						      make-steward)
	   (declare (ignore key make-steward))
	   (flet ((ensure-that-configure-steward-returns-the-instance (steward)
		    (assert-eq steward (cid:configure-steward steward)))
		  (ensure-that-steward-has-expected-type (instance)
		    (assert-type instance 'cid:steward)
		    (assert-type instance steward-class)))
	     (let ((steward
		     (cid:find-steward name :tenant tenant :project project :steward-class steward-class)))
	       (ensure-that-steward-has-expected-type steward)
	       (ensure-that-configure-steward-returns-the-instance steward)))))
    (with-test-database
      (populate-tenant-table)
      (populate-project-table)
      (populate-steward-tables)
      (loop :for example :in (select-steward-examples-by-key key)
	    :do (apply #'ensure-that-steward-can-be-configured example)))))

;;;; End of file `steward.lisp'
