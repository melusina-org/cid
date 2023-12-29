;;;; utilities.lisp — Utilities for El Cid tests

;;;; El Cid (https://github.com/melusina-org/cid)
;;;; This file is part of El Cid.
;;;;
;;;; Copyright © 2015–2023 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(in-package #:org.melusina.cid/testsuite)

(defun system-relative-pathname (&optional pathname)
  (flet ((system-source-directory ()
	   (asdf:system-source-directory
	    #.(string-downcase (package-name *package*)))))
    (if pathname
	(merge-pathnames pathname (system-source-directory))
	(system-source-directory))))

(defun enumerate-images (&key tag)
  (loop :for name :in '("cid/linux" "cid/console" "cid/trac" "cid/reverseproxy")
	:for image = (build:find-image name)
	:when tag
	:do (setf (build:image-tag image) tag)
	:collect image))

(defun enumerate-volumes (&key project)
  (loop :for system :in '("trac" "git" "www")
	:collect (docker:make-volume
		  :name (concatenate 'string "cid-" project "-" system))))

(defun testsuite-database-name (&optional(testsuite-id *testsuite-id*))
  "The name of the testsuite database file."
  (cid:user-data-relative-pathname
   "testsuite"
   (make-pathname :name (or testsuite-id "testsuite") :type "sqlite")))

(defun connect-database ()
  "Connect to a testsuite database.
This alters CID:*DATABASE-TYPE* and CID:*DATABASE-CONNECTION-SPEC*
to reflect the database being used."
  (setf cid:*database-type* :sqlite3
	cid:*database-connection-spec*
	(list (namestring (testsuite-database-name))))
  (ensure-directories-exist (testsuite-database-name))
  (cid:connect-database))

(defun disconnect-database (&key (destroy-database t))
  "Disconnect the testsuite database.
Unless DESTROY-DATABASE is set to NIL, the test database
is also destroyed.

This alters CID:*DATABASE-TYPE* and CID:*DATABASE-CONNECTION-SPEC*
to reflect that the testsuite database should not be used anymore.
It can be reconnected to with CONNECT-DATABASE."
  (cid:disconnect-database)
  (when destroy-database
    (clsql:destroy-database cid:*database-connection-spec*
			    :database-type cid:*database-type*))
  (setf cid:*database-type* nil
	cid:*database-connection-spec* nil))

(defmacro with-test-database (&body body)
  "Run BODY with a connected testsuite database.
The database is reclaimed and destroyed after BODY completes."
  `(let ((cid:*database-type* nil)
	 (cid:*database-connection-spec* nil)
	 (clsql:*default-database* nil))
     (unwind-protect
	  (progn
	    (connect-database)
	    ,@body)
       (disconnect-database))))



;;;
;;; Analyse HTTP Requests and Replies
;;;

(defclass http-reply ()
  ((body :initarg :body)
   (status-code :initarg :status-code)
   (headers :initarg :headers)
   (uri :initarg :uri)
   (stream :initarg :stream)
   (close :initarg :close)
   (reason-phrase :initarg :reason-phrase))
  (:documentation
   "Model HTTP-REPLIES as used by Drakma."))

(defun http-request (uri-path
                     &rest args
                     &key (protocol :http/1.1)
                          (method :get)
			  (hostname "localhost")
			  (port 80)
                          content
                          content-type
                          content-length
                          range
                          cookie-jar
                          basic-authorization
                          parameters
                          external-format-out
                          additional-headers
                          real-host)
  "Send a HTTP request to an ACCEPTOR and returns its reply.
This uses `DRAKMA:HTTP-REQUEST' to perform the request."
  (declare (ignore protocol method content content-type content-length
		   range cookie-jar basic-authorization parameters
		   external-format-out additional-headers real-host))
  (let ((drakma:*text-content-types*
          '(("text" . nil)
            ("application" . "json")))
	(http-reply
	  (make-instance 'http-reply))
	(actual-url
	  (format nil "http://~A:~A~A" hostname port uri-path))
	(actual-args
	  (loop :with filter-out = '(:hostname :port)
		:for args-part = args :then (cddr args-part)
		:while args-part
		:for key = (first args-part)
		:for value = (second args-part)
		:unless (member key filter-out)
		:append (list key value))))
    (with-slots (body status-code headers uri stream close) http-reply
      (setf (values body status-code headers uri stream close)
            (apply 'drakma:http-request actual-url actual-args)))
    (values http-reply)))

(defparameter *http-reply* nil
  "The HTTP-REPLY under scrutiny.")

(defmacro with-http-reply ((uri-path
			    &rest args
			    &key (protocol :http/1.1)
				 (method :get)
				 (hostname "localhost")
				 (port 80)
				 content
				 content-type
				 content-length
				 range
				 cookie-jar
				 basic-authorization
				 parameters
				 external-format-out
				 accept
				 additional-headers
				 real-host)
			   &body body-forms)
  (declare (ignore protocol method hostname port
		   content content-type content-length
		   range cookie-jar basic-authorization parameters
		   external-format-out accept additional-headers real-host))
  `(let ((*http-reply*
	   (http-request ,uri-path ,@args)))
     ,@body-forms))

(defun header-value (header-name reply)
  (cdr (assoc header-name (slot-value reply 'headers) :test #'string-equal)))

(defun header-match (value regexp)
  (when value
    (cl-ppcre:scan regexp value)))

(defun write-header-value (stream header-name reply)
  (let ((header-value (header-value header-name reply)))
    (if header-value
        (format stream "~%The header ~A is set to~%~%  ~S" header-name header-value)
        (format stream "~%The header ~A is not set." header-name))))

(define-assertion assert-http-header-undefined (header-name &optional (reply *http-reply*))
  "The assertion (ASSERT-HTTP-HEADER-UNDEFINED HEADER-NAME) is true iff
the header with the name HEADER-NAME is undefined in the last *HTTP-REPLY*."
  :report (lambda (stream)
            (format stream
"The optional argument REPLY can be used to test the header of another reply than
the last *HTTP-REPLY*.~%")
            (write-header-value stream header-name reply))
  (not (header-value header-name reply)))


(define-assertion assert-http-header-match (header-name regexp &optional (reply *http-reply*))
  "The assertion (ASSERT-HTTP-HEADER-MATCH HEADER-NAME REGEXP) is true iff
the header with the name HEADER-NAME is defined in the last *HTTP-REPLY* and
matches the given REGEXP.

The optional argument REPLY can be used to test the header of another reply than
the last *HTTP-REPLY*."
  :report (lambda (stream)
            (format stream "The regular expression used is

  ~S~%" regexp)
            (write-header-value stream header-name reply))
  (header-match (header-value header-name reply) regexp))

(define-assertion assert-http-header-charset (charset &optional (reply *http-reply*))
  "The assertion (ASSERT-HTTP-HEADER-CHARSET CHARSET) is true iff the
header with the name Content-Type is defined in the last *HTTP-REPLY*
and features a charset declaration validating CHARSET.

Currently, only the :utf-8 charset is supported.

The optional argument REPLY can be used to test the header of another
reply than the last *HTTP-REPLY*."
  :report (lambda (stream)
            (format stream "The expected CHARSET is

  ~S

and the CHARSET received in the last response is~%~%  " charset)
	    (write-header-value stream :content-type reply))
  (let ((regexp (ecase charset
                  (:utf-8 ";\\s*charset=(?i)utf-8"))))
    (header-match (header-value :content-type reply) regexp)))


(define-assertion assert-http-body (regexp &optional (reply *http-reply*))
"The assertion (ASSERT-HTTP-BODY REGEXP) is true iff the body response
in the last *HTTP-REPLY* matches the given REGEXP.

The optional argument REPLY can be used to test the header of another
reply than the last *HTTP-REPLY*."
  :report (lambda (stream)
            (format stream
"~&~%  The regular expression used is

  ~S

  and the last response body is

  ~S~%~%" regexp (slot-value reply 'body)))
  (cl-ppcre:scan regexp (slot-value reply 'body)))

(define-assertion assert-http-status (status &optional (reply *http-reply*))
"The assertion (ASSERT-HTTP-STATUS STATUS) is true iff the status response
in the last *HTTP-REPLY* equals the given STATUS or is in the list STATUS.

The optional argument REPLY can be used to test the header of another
reply than the last *HTTP-REPLY*."
  :report (lambda (stream)
            (format stream
"~&~%  The status used is

  ~S

  and the last response status is

  ~S~%~%" status  (slot-value reply 'status-code)))
  (if (listp status)
      (position (slot-value reply 'status-code) status)
      (= (slot-value reply 'status-code) status)))

;;;; End of file `utilities.lisp'
