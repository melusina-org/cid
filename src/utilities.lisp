;;;; utilities.lisp — Utilities for El Cid

;;;; El Cid (https://github.com/melusina-org/cid)
;;;; This file is part of El Cid.
;;;;
;;;; Copyright © 2015–2023 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(in-package #:org.melusina.cid)

(defun user-data-relative-pathname (&rest more)
  "Prepare a PATHNAME relative to the user data of the application."
  (apply #'uiop:xdg-data-home
	 #.(string-downcase (package-name *package*)) more))

;;;; End of file `utilities.lisp'
