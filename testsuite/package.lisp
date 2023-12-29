;;;; package.lisp — Package for El Cid tests

;;;; El Cid (https://github.com/melusina-org/cid)
;;;; This file is part of El Cid.
;;;;
;;;; Copyright © 2015–2023 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(defpackage #:org.melusina.cid/testsuite
  (:use #:common-lisp)
  (:local-nicknames
   (#:confidence #:org.melusina.confidence)
   (#:cid #:org.melusina.cid)
   (#:console #:org.melusina.cid/console)
   (#:docker #:org.melusina.cid/docker)
   (#:build #:org.melusina.cid/build)
   (#:development #:org.melusina.cid/development)
   (#:operation #:org.melusina.cid/operation))
  (:import-from
   #:org.melusina.confidence
   #:define-testcase
   #:define-assertion
   #:assert-t
   #:assert-t*
   #:assert-nil
   #:assert-eq
   #:assert-set-equal
   #:assert-string=
   #:assert-type
   #:assert-condition))

(in-package #:org.melusina.cid/testsuite)

;;;; End of file `package.lisp'
