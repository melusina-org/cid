;;;; setup.lisp — Setup for El Cid Atelier Project

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

(in-package #:cl-user)

(ql:quickload "org.melusina.atelier")

(atelier:initialise)

(defparameter *working-copy-directory* #p"~/Melusina/cid/")

(defun working-copy-pathname (&rest path-elements)
  (reduce #'merge-pathnames (append path-elements (list *working-copy-directory*)) :from-end t))

(setf atelier:*parameter-bindings*
      '((:copyright-holder . "Michaël Le Barbier")
        (:copyright-year . "2017–2022")
	(:project-filename . "org.melusina.cid")
        (:project-name . "El Cid")
	(:project-description . "Count of Vivar and Prince of Continuous Integration and Delivery Systems")
	(:project-long-description .
	 #.(concatenate 'string
	    "The El Cid project aims at providing a complete continuous integration and delivery "
	    "system that is easy to incrementally improve, to share with team mates "
	    "and collaborators, and that can be operated trivially either locally, on bare metal "
	    "or in the cloud."))
        (:homepage . "https://github.com/melusina-conseil/cid")
        (:license . :cecill-b)))

#+nil
(atelier:write-template :atelier-lisp-project #p"~/Melusina/cid/")

(defun new-lisp-file (name summary)
  "Create a new file NAME with SUMMARY."
  (let ((source-pathname
	  (working-copy-pathname "src/" name))
	(testsuite-pathname
	  (working-copy-pathname "testsuite/" name))
	(environment
	  (list (cons :summary summary))))
    (atelier:write-template :lisp-source source-pathname environment)
    (atelier:write-template :lisp-source testsuite-pathname environment)))

;;;; End of file `setup.lisp'
