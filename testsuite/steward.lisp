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

(defun example-simulator ()
  "Some SIMULATOR steward that can be used in the testsuite."
  (cid:find-steward "simulator"
		    :tenant "testsuite"
		    :project "testproject"
		    :steward-class 'cid:simulator))

(defparameter *example-steward-definitions*
  (list
   (flet ((make-simulator ()
	    (cid:make-simulator :tenant "testsuite"
				:project "testproject"
				:name "simulator")))
     (list
      :key :simulator
      :steward-class 'cid:simulator
      :name "simulator"
      :tenant "testsuite"
      :project "testproject"
      :make-steward #'make-simulator))
   #+org.melusina.cid/poc
   (list
    :key :docker-engine
    :steward-class 'cid:docker-engine
    :name "local-docker-engine"
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

(defun example-stewards (&optional key)
  "Some stewards that can be used in the testsuite."
  (flet ((make-steward (example)
	   (alexandria:when-let ((make-steward
				  (getf example :make-steward)))
	     (funcall make-steward))))
    (loop :for example :in (select-steward-examples-by-key key)
	  :for steward = (make-steward example)
	  :when steward
	  :collect steward)))

(define-testcase steward-unit-test (&optional key)
  (flet ((ensure-that-steward-can-be-configured (&key key name steward-class tenant project
						      make-steward)
	   (declare (ignore key name tenant project))
	   (unless make-steward
	     (return-from ensure-that-steward-can-be-configured))
	   (flet ((ensure-that-configure-steward-returns-the-instance (steward)
		    (assert-eq steward (cid:configure-steward steward)))
		  (ensure-that-steward-has-expected-type (instance)
		    (assert-type instance 'cid:steward)
		    (assert-type instance steward-class)))
	     (let ((steward
		     (funcall make-steward)))
	       (ensure-that-steward-has-expected-type steward)
	       (ensure-that-configure-steward-returns-the-instance steward)))))
    (with-test-environment
      (populate-tenant-table)
      (populate-project-table)
      (loop :for example :in (select-steward-examples-by-key key)
	    :do (apply #'ensure-that-steward-can-be-configured example)))))

;;;; End of file `steward.lisp'
