;;;; memory.lisp — Memory Provider for El Cid

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

(in-package #:org.melusina.cid/testsuite)

(define-testcase ensure-memory-is-a-singleton ()
  "Ensure the MEMORY is a singleton."
  (with-empty-providers
    (assert-eq nil (cid:find-provider :null))
    (assert-eq (cid:make-memory) (cid:make-memory))
    (assert-t* (cid:find-provider :null))
    (assert-eq (cid:make-memory) (cid:find-provider :null))))

(define-testcase testsuite-memory ()
  (ensure-memory-is-a-singleton))

(define-testcase integration-memory ()
  (with-empty-providers
    (let* ((memory-provider
	     (cid:make-memory))
	   (memory-text
	     (cid:make-memory-text :pathname "global.username"
				   :description "The username for global environment."
				   :provider memory-provider
				   :text "USER")))
      (journey-resource-create-delete memory-text)
      (journey-resource-create-read-update-delete memory-text 'cid:text "ALTUSER"))))

;;;; End of file `memory.lisp'
