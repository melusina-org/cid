;;;; docker-engine.lisp — Docker Engine Provider for El Cid

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

(define-testcase ensure-docker-engine-is-a-singleton ()
  "Ensure the argument-free DOCKER-ENGINE is a singleton."
  (let ((cid::*providers*
	    (make-hash-table)))
    (assert-eq nil (cid:find-provider :docker-engine))
    (assert-eq (cid:make-docker-engine :pathname "docker-engine")
	       (cid:make-docker-engine))
    (assert-t* (cid:find-provider :docker-engine))
    (assert-eq (cid:make-docker-engine) (cid:find-provider :docker-engine))))

(define-testcase ensure-docker-engine-is-configurable ()
  "Ensure the DOCKER-ENGINE is configurable."
  (let ((cid::*providers*
	    (make-hash-table)))
    (assert-eq nil (cid:find-provider :docker-engine))
    (let ((docker-engine
	    (cid:make-docker-engine)))
      (assert-nil (slot-value docker-engine 'cid::version))
      (cid:configure-provider docker-engine)
      (assert-t* (slot-value docker-engine 'cid::version)))))

(define-testcase testsuite-docker-engine ()
  (ensure-docker-engine-is-a-singleton)
  (ensure-docker-engine-is-configurable))
  
;;;; End of file `docker-engine.lisp'
