;;;; swank.lisp — Swank Server for El Cid Administration Console

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


;;;;
;;;; Start and Stop the Server
;;;;

(defvar *swank-acceptor* nil
  "The instance of the swank server running.")

(defvar *service-acceptor* nil
  "The instance of the service running.")

(defun start-swank ()
  "Start the SWANK server."
  (unless *swank-acceptor*
    (setf swank::*loopback-interface* *server-address*)
    (setf swank::*swank-debug-p* nil)
    (swank:create-server
     :port *swank-port*
     :dont-close t
     :style swank:*communication-style*)
    (setf *swank-acceptor* t)))

(defun stop-swank ()
  "Stop the SWANK server."
  (when *swank-acceptor*
    (swank:stop-server *swank-port*)
    (setf *swank-acceptor* nil)))

;;;; End of file `entry-point.lisp'
