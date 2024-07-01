;;;; utilities.lisp — Utilities for El Cid tests

;;;; El Cid (https://github.com/melusina-org/cid)
;;;; This file is part of El Cid.
;;;;
;;;; Copyright © 2015–2024 Michaël Le Barbier
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

(defmacro with-test-environment (&body body)
  "Run BODY with an appropriate test environment."
  `(let* ((cid::*tenant-directory*
	    (make-hash-table :test #'equal))
	  (cid::*project-directory*
	    (make-hash-table :test #'equal))
	  (cid:*tenant*
	    (cid:make-tenant :name "testsuite" :displayname "Test Tenant"))
	  (cid:*project*
	    (cid:make-project :name *testsuite-id*
			      :displayname (concatenate 'string "Projet " *testsuite-id*)))
	  (cid:*encryption-key*
	    (ironclad:random-data 32)))
     ,@body))


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
      (flet ((perform-request-and-store-response ()
	       (setf (values body status-code headers uri stream close)
		     (apply 'drakma:http-request actual-url actual-args))))
	(restart-case (perform-request-and-store-response)
	  (http-retry ()
	    (perform-request-and-store-response))
	  (http-sleep-and-retry ()
	    (sleep 5)
	    (perform-request-and-store-response)))))
    (values http-reply)))

(defun restart-with-http-sleep-and-retry (condition)
  (declare (ignore condition))
  (let ((restart (find-restart 'http-sleep-and-retry)))
    (when restart (invoke-restart restart))))

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


;;;;
;;;; Ensure that the testsuite runs on test docker contexts
;;;;

(defparameter *test-docker-contexts*
  '("colima-laboratory")
  "The list of docker contexts where the testsuite is allowed to run.")

(defun check-that-the-testsuite-runs-on-a-test-docker-context ()
  "Signal an error unless the testsuite runs on a test docker context."
  (let ((docker-context
	  (gethash "Context" (gethash "ClientInfo" (docker:info)))))
    (unless (member docker-context *test-docker-contexts*
		    :test #'string=)
      (error "The testsuite is not allowed to run on the docker context ~S."
	     docker-context))))


;;;;
;;;; Persistence Testcase
;;;;

(define-testcase check-structural-equality (object1 object2)
  (assert-equal (type-of object1) (type-of object2))
  (etypecase object1
    (integer
     (assert-equal object1 object2))
    (string
     (assert-string= object1 object2))
    (symbol
     (assert-eq object1 object2))
    (null
     (assert-eq object1 object2))
    (pathname
     (assert-equal object1 object2))
    (cons
     (check-structural-equality (car object1) (car object2))
     (check-structural-equality (cdr object1) (cdr object2)))
    ((or cid:tenant cid:project)
     ;; TENANT and PROJECT instances are listed in a directory
     ;; and must be physically equal rather than structurally equal.
     (assert-eq object1 object2))
    ((or poc:infrastructure-stack cid:steward cid:resource)
		(assert-eq (cid:persistent-constructor (type-of object1))
			   (cid:persistent-constructor (type-of object2)))
     (loop :for slot-spec :in (cid:persistent-slots object1)
	   :for slot-name = (getf slot-spec :slot-name)
	   :do (check-structural-equality
		(slot-value object1 slot-name)
		(slot-value object2 slot-name))))))

(define-testcase verify-persistence-idempotency (object)
  "Demonstrate that OBJECT persistence is idempotent."
  (labels ((write-then-read (object)
	     (cid:read-persistent-object-from-string
	      (cid:write-persistent-object-to-string object))))
    (check-structural-equality object (write-then-read object))))

(define-testcase verify-formatting-of-persistent-resources ()
  (let ((tenant
	  (cid:make-tenant :name "local" :displayname "Local Tenant"))
	(tenant-line-representation
	  "
[ORG.MELUSINA.CID:TENANT :NAME \"local\" :DISPLAYNAME \"Local Tenant\"]
")
	(tenant-compact-representation
	  "
[ORG.MELUSINA.CID:TENANT
 :NAME \"local\"
 :DISPLAYNAME \"Local Tenant\"]
")
	(tenant-very-compact-representation
	  "
[ORG.MELUSINA.CID:TENANT
 :NAME
 \"local\"
 :DISPLAYNAME
 \"Local Tenant\"]
"))
    (assert-string= tenant-line-representation
		    (cid:write-persistent-object-to-string tenant))
    (assert-string= tenant-compact-representation
		    (let ((*print-right-margin* 32))
		      (cid:write-persistent-object-to-string tenant)))
    (assert-string= tenant-very-compact-representation
		    (let ((*print-right-margin* 8))
		      (cid:write-persistent-object-to-string tenant)))))

(define-testcase utilities-unit-test ()
  #+org.melusina.cid/formatting-of-persistent-resources
  (verify-formatting-of-persistent-resources))

;;;; End of file `utilities.lisp'
