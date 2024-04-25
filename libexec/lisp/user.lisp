;;;; user.lisp — System for El Cid Users

;;;; El Cid (https://github.com/melusina-org/cid)
;;;; This file is part of El Cid.
;;;;
;;;; Copyright © 2015–2024 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(defpackage #:org.melusina.cid/user
  (:use #:cl)
  (:local-nicknames
   (#:atelier #:org.melusina.atelier)
   (#:cid #:org.melusina.cid)
   (#:poc #:org.melusina.cid/poc)
   (#:colima #:org.melusina.cid/colima)
   (#:docker #:org.melusina.cid/docker)
   (#:build #:org.melusina.cid/build)
   (#:console #:org.melusina.cid/console)
   (#:development #:org.melusina.cid/development)
   (#:operation #:org.melusina.cid/operation)
   (#:testsuite #:org.melusina.cid/testsuite)))

(in-package #:org.melusina.cid/user)

;;;; End of file `user.lisp'
