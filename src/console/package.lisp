;;;; package.lisp — Package for El Cid Administration Console

;;;; El Cid (https://github.com/melusina-org/cid)
;;;; This file is part of El Cid.
;;;;
;;;; Copyright © 2015–2024 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(defpackage #:org.melusina.cid/console
  (:use #:common-lisp)
  (:local-nicknames
   (#:cid #:org.melusina.cid)
   (#:development #:org.melusina.cid/development)
   (#:operation #:org.melusina.cid/operation))
  (:export
   #:entry-point
   #:configure-console
   #:configure-apache-trac))

(in-package #:org.melusina.cid/console)

;;;; End of file `package.lisp'
