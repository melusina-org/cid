;;;; setup.lisp — Atelier Setup for El Cid

;;;; El Cid (https://github.com/melusina-conseil/cid)
;;;; This file is part of El Cid.
;;;;
;;;; Copyright © 2017–2022 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(in-package #:cl-user)

;;;
;;; Atelier
;;;

(ql:quickload "org.melusina.atelier" :silent t)

(atelier:initialise)

(setf atelier:*parameter-bindings*
      '((:copyright-holder . "Michaël Le Barbier")
        (:copyright-year . "2017–2022")
	(:project-filename . "org.melusina.cid")
        (:project-name . "El Cid")
	(:project-description . "Count of Vivar and Prince of Continuous Integration and Delivery Systems")
	(:project-long-description . "The El Cid project aims at providing a complete continuous integration and delivery system that is easy to incrementally improve, to share with team mates and collaborators, and that can be operated trivially either locally, on bare metal or in the cloud.")
        (:homepage . "https://github.com/melusina-conseil/cid")
        (:license . :mit)))

(ql:quickload "org.melusina.cid" :silent t)

(clsql:enable-sql-reader-syntax)

;;;; End of file `setup.lisp'
