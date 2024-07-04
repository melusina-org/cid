;;;; org.melusina.cid.asd — Load El Cid

;;;; El Cid (https://github.com/melusina-org/cid)
;;;; This file is part of El Cid.
;;;;
;;;; Copyright © 2015–2024 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(asdf:defsystem #:org.melusina.cid
  :description "Continuous Integration and Delivery"
  :author "Michaël Le Barbier"
  :license "MIT License"
  :depends-on (#:alexandria
	       #:cl-ppcre
	       #:flexi-streams
	       #:ironclad
	       #:yason)
  :components
  ((:module "src"
    :components ((:file "package")
		 (:file "utilities")
		 (:file "initialization-file")
		 (:module "traits"
		  :components ((:file "named")))
		 (:file "tenant")
		 (:file "project")
		 (:file "steward")
		 (:file "resource")
		 (:module "stewards"
		  :components ((:file "simulator")
			       (:file "local-filesystem-subtree")
			       (:file "docker-engine")))
		 (:file "entry-point")))))

(asdf:defsystem #:org.melusina.cid/poc
  :description "Proof of Concept for El Cid"
  :author "Michaël Le Barbier"
  :license "MIT License"
  :depends-on (#:org.melusina.cid)
  :components
  ((:module "src"
    :components ((:file "poc")))))

(asdf:defsystem #:org.melusina.cid/console
  :description "Administration Console for El Cid"
  :author "Michaël Le Barbier"
  :license "MIT License"
  :build-operation program-op
  :build-pathname "console"
  :entry-point "org.melusina.cid/console:entry-point"
  :components
  ((:module "src/console"
    :components ((:file "package")
		 (:file "utilities")
		 (:file "entry-point")))))

(asdf:defsystem #:org.melusina.cid/colima
  :description "Colima Support for El Cid"
  :author "Michaël Le Barbier"
  :license "MIT License"
  :depends-on (#:yason)
  :components
  ((:module "libexec/lisp"
    :components ((:file "colima")))))

(asdf:defsystem #:org.melusina.cid/docker
  :description "Docker Support for El Cid"
  :author "Michaël Le Barbier"
  :license "MIT License"
  :depends-on (#:yason
	       #:cl-ppcre)
  :components
  ((:module "libexec/lisp"
    :components ((:file "docker")))))

(asdf:defsystem #:org.melusina.cid/build
  :description "Build tools for El Cid"
  :author "Michaël Le Barbier"
  :license "MIT License"
  :depends-on (#:org.melusina.cid/docker)
  :components
  ((:module "libexec/lisp"
    :components ((:file "build")))))

(asdf:defsystem #:org.melusina.cid/development
  :description "Development tools for El Cid"
  :author "Michaël Le Barbier"
  :license "MIT License"
  :depends-on (#:org.melusina.atelier
	       #:org.melusina.cid/colima
	       #:org.melusina.cid/build)
  :components
  ((:module "libexec/lisp"
    :components ((:file "development")))))

(asdf:defsystem #:org.melusina.cid/operation
  :description "Operation tools for El Cid"
  :author "Michaël Le Barbier"
  :license "MIT License"
  :depends-on (#:cl-ppcre
	       #:org.melusina.atelier
	       #:org.melusina.cid
	       #:org.melusina.cid/docker)
  :components
  ((:module "libexec/lisp"
    :components ((:file "operation")))))

(asdf:defsystem #:org.melusina.cid/testsuite
  :description "Testsuite for El Cid"
  :author "Michaël Le Barbier"
  :license "MIT License"
  :depends-on (#:drakma
	       #:org.melusina.confidence
	       #:org.melusina.rashell
	       #:org.melusina.cid
	       #:org.melusina.cid/poc
	       #:org.melusina.cid/development
	       #:org.melusina.cid/operation
	       #:org.melusina.cid/build
	       #:org.melusina.cid/console
	       #:org.melusina.cid/docker)
  :components
  ((:module "testsuite"
    :components ((:file "package")
		 (:file "utilities")
		 (:file "initialization-file")
		 (:file "tenant")
		 (:file "project")
		 (:file "steward")
		 (:file "resource")
		 (:module "stewards"
		  :components ((:file "simulator")
			       (:file "local-filesystem-subtree")
			       (:file "docker-engine")))
		 (:file "poc")))
   (:module "libexec"
    :components
    ((:module "lisp"
      :components
      ((:file "testsuite-docker")
       (:file "testsuite-build")
       (:file "testsuite-operation")))))
   (:file "testsuite/entry-point")))

(asdf:defsystem #:org.melusina.cid/user
  :description "System for El Cid Users"
  :author "Michaël Le Barbier"
  :license "MIT License"
  :depends-on (#:org.melusina.confidence
	       #:org.melusina.cid
	       #:org.melusina.cid/poc
	       #:org.melusina.cid/colima
	       #:org.melusina.cid/docker
	       #:org.melusina.cid/console
	       #:org.melusina.cid/build
	       #:org.melusina.cid/development
	       #:org.melusina.cid/operation
	       #:org.melusina.cid/testsuite)
  :components
  ((:module "libexec"
    :components
    ((:module "lisp"
      :components
      ((:file "user")))))))

;;;; End of file `org.melusina.cid.asd'
