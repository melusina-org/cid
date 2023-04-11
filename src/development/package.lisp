;;;; package.lisp — Project Development for El Cid

;;;; El Cid (https://github.com/melusina-conseil/cid)
;;;; This file is part of El Cid.
;;;;
;;;; Copyright © 2017–2022 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(defpackage #:org.melusina.cid/development
  (:use #:cl)
  (:local-nicknames
   (#:atelier #:org.melusina.atelier)
   (#:cid #:org.melusina.cid))
  (:export
   #:lint
   #:reload))

(in-package #:org.melusina.cid/development)

(defparameter *parameter-bindings*
  '((:copyright-holder . "Michaël Le Barbier")
    (:copyright-year . "2017–2022")
    (:project-filename . "org.melusina.cid")
    (:project-name . "El Cid")
    (:project-description .
     "Count of Vivar and Prince of Continuous Integration and Delivery Systems")
    (:project-long-description .
     #.(concatenate 'string
	"The El Cid project aims at providing a complete continuous integration"
	" and delivery system that is easy to incrementally improve, to share"
	" with team mates and collaborators, and that can be operated trivially"
	" either locally, on bare metal or in the cloud."))
    (:homepage . "https://github.com/melusina-conseil/cid")
    (:license . :mit)))

(defun lint ()
  "Lint the project."
  (let ((atelier:*parameter-bindings* *parameter-bindings*))
    (atelier:lint
     #p"src"
     #p"testsuite"
     #p"development"
     #p"operation")))

(defun reload ()
  (ql:quickload '("org.melusina.atelier"
		  "org.melusina.confidence"
		  "org.melusina.cid/testsuite")))


;;;;
;;;; Command Stock
;;;;

#+nil
(org.melusina.cid/development:reload)

;;;; End of file `package.lisp'
