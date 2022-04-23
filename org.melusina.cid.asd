;;;; org.melusina.cid.asd — System definition for El Cid

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

(asdf:defsystem org.melusina.cid
  :description "Count of Vivar and Prince of Continuous Integration and Delivery Systems"
  :author "Michaël Le Barbier"
  :license "CeCILL-B Free Software License Agreement"
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
		 ;;; API
		 (:file "health")
		 ;;; Server
		 (:file "server")
		 ;;; Toplevel
		 (:file "entrypoint")))
   (:module "provider"
    :components
    ((:module "src"
      :components
      ((:file "null")
       (:file "memory")
       (:file "docker-engine")))))))

(asdf:defsystem org.melusina.cid/testsuite
  :description "Count of Vivar and Prince of Continuous Integration and Delivery Systems"
  :author "Michaël Le Barbier"
  :license "CeCILL-B Free Software License Agreement"
  :depends-on (:alexandria :org.melusina.confidence :org.melusina.cid)
  :components
  ((:module "testsuite"
    :components ((:file "package")
		 (:file "utilities")
		 (:file "tenant")
		 (:file "identity")
		 (:file "project")
		 (:file "database")
		 (:file "provider")
		 (:file "resource")
		 (:file "entrypoint")))
   (:module "provider"
    :components
    ((:module "testsuite"
      :components
      ((:file "null") 
       (:file "memory")
       (:file "docker-engine")))))))

;;;; End of file `org.melusina.cid.asd'
