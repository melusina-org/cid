;;;; server.lisp — Application Forge for Common Lisp

;;;; El Cid (https://github.com/melusina-conseil/cid)
;;;; This file is part of El Cid.
;;;;
;;;; Copyright © 2017–2022 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(in-package #:org.melusina.cid)

(defparameter *swank-port* 4005
  "The port of the swank listener.")

(defparameter *service-port* 8080
  "The port of the service listener.")

(defparameter *service-address* "0.0.0.0")

(defvar *service-acceptor* nil
  "The instance of the service running.")

(defun start-server ()
  "Start the El Cid Server"
  (unless *service-acceptor*
    (setf swank::*loopback-interface* *service-address*)
    (swank-loader:init)
    (swank:create-server :port *swank-port*
                         :dont-close t
                         :style swank:*communication-style*)
    (setf *service-acceptor*
          (hunchentoot:start
           (make-instance 'hunchentoot:easy-acceptor
                          :port *service-port*
                          :address *service-address*)))))

(defun stop-server ()
  (when *service-acceptor*
    (hunchentoot:stop *service-acceptor*)
    (swank:stop-server *swank-port*)
    (setf *service-acceptor* nil)))

;;;; End of file `server.lisp'
