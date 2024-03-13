;;;; build-console.lisp — Build the Administration Console Program

;;;; El Cid (https://github.com/melusina-org/cid)
;;;; This file is part of El Cid.
;;;;
;;;; Copyright © 2015–2024 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(in-package #:cl-user)

(handler-case
    (progn
      (require '#:asdf)
      (require '#:uiop)
      (require '#:org.melusina.cid/console))
  (error (c)
    (format *trace-output* "~&Failure: ~A~&" c)
    (quit :unix-status 1)))

(asdf:operate 'asdf:program-op '#:org.melusina.cid/console)

;;;; End of file `build-console.lisp'
