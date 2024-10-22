;;;; configuration.lisp — Configuration for El Cid Administration Console

;;;; El Cid (https://github.com/melusina-org/cid)
;;;; This file is part of El Cid.
;;;;
;;;; Copyright © 2015–2024 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT


(in-package #:org.melusina.cid/console)

(defparameter *swank-port* 4005
  "The port of the SWANK listener.")

(defparameter *server-address*
  #.(flet ((docker-p ()
	     (member :linux *features*)))
      (if (docker-p)
	  "0.0.0.0"
	  "127.0.0.1")))

;;;; End of file `configuration.lisp'
