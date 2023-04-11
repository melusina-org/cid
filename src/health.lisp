;;;; health.lisp — Health API

;;;; El Cid (https://github.com/melusina-conseil/cid)
;;;; This file is part of El Cid.
;;;;
;;;; Copyright © 2017–2023 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(in-package #:org.melusina.cid)

(hunchentoot:define-easy-handler (api-v1-health :uri "/api/v1/health") ()
  (setf (hunchentoot:content-type*) "application/json")
  "{status:\"up\"}")

;;;; End of file `health.lisp'
