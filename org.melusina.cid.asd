;;;; org.melusina.cid.asd — Load El Cid

;;;; El Cid (https://github.com/melusina-org/cid)
;;;; This file is part of El Cid.
;;;;
;;;; Copyright © 2015–2023 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(asdf:defsystem #:org.melusina.cid
  :description "Continuous Integration and Delivery"
  :author "Michaël Le Barbier"
  :license "MIT License"
  :depends-on (#:org.melusina.atelier)
  :components
  ((:module "src"
    :components ((:file "package")
		 (:file "utilities")
		 (:file "entrypoint")))))

(asdf:defsystem #:org.melusina.cid/testsuite
  :description "Testsuite for El Cid"
  :author "Michaël Le Barbier"
  :license "MIT License"
  :depends-on (#:org.melusina.confidence)
  :components
  ((:module "testsuite"
    :components ((:file "package")
		 (:file "utilities")
		 (:file "entrypoint")))))

(asdf:defsystem #:org.melusina.cid/development
  :description "Development tools for El Cid"
  :author "Michaël Le Barbier"
  :license "MIT License"
  :depends-on (#:org.melusina.atelier)
  :components
  ((:module "libexec/lisp"
    :components ((:file "development")))))

;;;; End of file `org.melusina.cid.asd'
