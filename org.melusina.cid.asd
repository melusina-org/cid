;;;; org.melusina.cid.asd — System definition for El Cid

;;;; El Cid (https://github.com/melusina-conseil/cid)
;;;; This file is part of El Cid.
;;;;
;;;; Copyright © 2017–2022 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(asdf:defsystem org.melusina.cid
  :description "Count of Vivar and Prince of Continuous Integration and Delivery Systems"
  :author "Michaël Le Barbier"
  :license "MIT License"
  :depends-on (:alexandria :clsql :clsql-sqlite3 :hunchentoot :swank :unix-opts
	       :org.melusina.rashell)
  :components
  ((:module "src"
    :components ((:file "package")
                 (:file "utilities")
		 (:file "tenant")
		 (:file "identity")
		 (:file "project")
		 (:file "database")
		 (:file "provider")
		 (:file "resource")
		 ;;; Components
		 (:file "trac")
		 (:file "gocd")
		 ;;; Providers
		 (:module "providers"
		  :components
		  ((:file "null") 
		   (:file "memory")
		   (:file "docker-engine")))
		 ;;; API
		 (:file "health")
		 ;;; Server
		 (:file "server")
		 ;;; Toplevel
		 (:file "entrypoint")))))

(asdf:defsystem org.melusina.cid/testsuite
  :description "Count of Vivar and Prince of Continuous Integration and Delivery Systems"
  :author "Michaël Le Barbier"
  :license "MIT License"
  :depends-on (:alexandria :org.melusina.confidence :org.melusina.cid)
  :components
  ((:module "testsuite"
    :components ((:file "package")
		 (:file "utilities")
		 (:file "tenant")
		 (:file "identity")
		 (:file "project")
		 (:file "database")
		 (:module "providers"
		  :components
		  ((:file "null") 
		   (:file "memory")
		   (:file "docker-engine")))
		 (:file "provider")
		 (:file "resource")
		 (:file "entrypoint")))))

;;;; End of file `org.melusina.cid.asd'
