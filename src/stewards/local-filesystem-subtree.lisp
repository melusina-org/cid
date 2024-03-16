;;;; local-filesystem-subtree.lisp — Filesystem Subtree Steward for El Cid

;;;; El Cid (https://github.com/melusina-org/cid)
;;;; This file is part of El Cid.
;;;;
;;;; Copyright © 2015–2024 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(in-package #:org.melusina.cid)

;;;;
;;;; Steward
;;;;

(clsql:def-view-class local-filesystem-subtree (steward)
  ((description
    :allocation :class
    :initform "A steward which owns files in a specific filesystem subtree."
    :type string)
   (pathname
    :initform (error "A LOCAL-FILESYSTEM-SUBTREE requires a PATHNAME.")
    :type pathname
    :initarg :pathname
    :documentation "The root of the filesystem subtree owned by the steward."))
  (:documentation
   "A steward which owns files in a specific filesystem subtree.
In this subtree, text files can be created and updated."))

(defun make-local-filesystem-subtree (&rest initargs &key tenant project name displayname pathname)
  "Make a LOCAL-FILESYSTEM-SUBTREE steward with the given parameters."
  (declare (ignore tenant project name displayname pathname))
  (apply #'make-instance 'local-filesystem-subtree initargs))

(defmethod list-resource-identifiers ((steward local-filesystem-subtree) (resource-class (eql 'local-text-file)))
  (loop :for absolute-pathname :in (uiop:directory-files (slot-value steward 'pathname))
	:for relative-pathname = (namestring (make-pathname :directory nil :defaults absolute-pathname))
	:when (portable-filename-character-set-p relative-pathname)
	:collect relative-pathname))


;;;;
;;;; Resources
;;;;

(clsql:def-view-class local-text-file (resource)
  ((steward-class
    :type symbol
    :initform 'local-filesystem-subtree
    :allocation :class)
   (pathname
    :type pathname
    :initarg :pathname
    :documentation "The pathname of the file relative to its stweward.")
   (mode
    :type int
    :initarg :mode
    :initform #o644)
   (external-format
    :type keyword
    :initarg :external-format
    :initform :default)
   (content
    :type string
    :initarg :content)
   (checksum
    :type string))
  (:documentation
   "A text file on the local filesystem."))

(defun make-local-text-file (&rest initargs &key local-filesystem-subtree name displayname description
					    pathname mode content external-format)
  "Make a local text file."
  (declare (ignore name displayname description
		   mode content external-format))
  (flet ((check-that-pathname-is-relative (pathname)
	   (unless (or (eq nil (pathname-directory pathname))
		       (eq :relative (first (pathname-directory pathname))))
	     (error "The pathname ~A must be relative." pathname))))	   
    (check-type local-filesystem-subtree local-filesystem-subtree)
    (check-type pathname pathname)
    (check-that-pathname-is-relative pathname)
    (when (pathname-directory pathname)
      (error "The pathname ~A must sit directly under the root of the subtree.
Deeper hierarchies are not implemented." pathname))
    (apply #'make-instance 'local-text-file
	   :steward local-filesystem-subtree
	   :identifier (namestring pathname)
	   (remove-property initargs :local-filesystem-subtree))))

(defmethod examine-resource append ((instance local-text-file))
  (with-slots (pathname mode external-format content checksum) instance
    (list
     :pathname pathname
     :mode mode
     :external-format external-format
     :content content
     :checksum checksum)))

(defmethod update-instance-from-resource ((instance local-text-file))
  (let ((absolute-pathname
	  (merge-pathnames
	   (pathname
	    (slot-value instance 'identifier))
	   (slot-value (slot-value instance 'steward) 'pathname))))
    (unless (probe-file absolute-pathname)
      (with-slots (state) instance
	(setf state nil)
	(return-from update-instance-from-resource instance)))
    (flet ((file-mode (pathname)
	     (osicat-posix:stat-mode (osicat-posix:stat pathname)))
	   (file-content (pathname)
	     (with-output-to-string (contents)
	       (with-open-file (input pathname :external-format (slot-value instance 'external-format))
		 (loop :for char = (read-char input nil nil)
		       :while char
		       :do (write-char char contents))))))
      (with-slots (identifier pathname state mode content checksum) instance
	(setf pathname (pathname identifier)
	      mode (file-mode absolute-pathname)
	      content (file-content absolute-pathname)
	      checksum (file-checksum absolute-pathname)
	      state t)))))

;;;; End of file `local-filesystem-subtree.lisp'
