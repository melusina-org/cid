;;;; local-filesystem-subtree.lisp — Filesystem Subtree Steward Testsuite for El Cid

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

(define-testcase local-filesystem-subtree-unit-test ()
  (rashell:with-temporary-directory (tmpdir)
    (with-test-environment
      (populate-tenant-table)
      (populate-project-table)
      (let ((steward
	      (cid:make-local-filesystem-subtree
	       :tenant "testsuite"
	       :project "testproject"
	       :name "tmpdir"
	       :displayname "Temporary directory for the Test Suite"
	       :pathname tmpdir)))
	(flet ((make-local-text-file ()
		 (cid:make-local-text-file
		  :local-filesystem-subtree steward
		  :name "file-1"
		  :displayname "Temporary File #1"
		  :description "A temporary file resource used in the testsuite."
		  :pathname #p"file-1"
		  :content "Some test content for the temporary file."))
	       (make-local-initialization-file ()
		 (cid:make-local-initialization-file
		  :local-filesystem-subtree steward
		  :name "configuration-1.ini"
		  :displayname "Configuration File #1"
		  :description "A temporary configuration file resource used in the testsuite."
		  :pathname #p"configuration-1.ini"
		  :configuration
		  (cid:read-initialization-file
		   (system-relative-pathname #p"example/initialization-file/example-2.ini")))))
	  (resource-unit-test
	   :resource-type 'cid:local-text-file
	   :make-resource #'make-local-text-file
	   :slot-name 'cid:content
	   :new-slot-value "Some modified test content for the temporary file.")
	  (resource-unit-test
	   :resource-type 'cid:local-initialization-file
	   :make-resource #'make-local-initialization-file
	   :slot-name 'cid:configuration
	   :new-slot-value
	   (cid:read-initialization-file
	    (system-relative-pathname #p"example/initialization-file/example-1.ini"))))))))

;;;; End of file `local-filesystem-subtree.lisp'
