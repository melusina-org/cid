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
    :type integer
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
	   (remove-property initargs :local-filesystem-subtree))))

(defmethod examine-resource append ((instance local-text-file))
  (with-slots (pathname mode external-format content checksum) instance
    (list
     :pathname pathname
     :mode mode
     :external-format external-format
     :content content
     :checksum checksum)))

(defun local-text-file-absolute-pathname (local-text-file)
  (check-type local-text-file local-text-file)
  (merge-pathnames
   (slot-value local-text-file 'pathname)
   (slot-value (slot-value local-text-file 'steward) 'pathname)))

(defmethod update-instance-from-resource ((instance local-text-file))
  (flet ((ensure-pathname-is-set-when-identifier-is-set ()
	   (when (and (slot-boundp instance 'identifier)
		      (slot-value instance 'identifier)
		      (not (slot-boundp instance 'pathname)))
	     (setf (slot-value instance 'pathname)
		   (pathname (slot-value instance 'identifier)))))
	 (return-early-when-file-does-not-exist (absolute-pathname)
	   (unless (probe-file absolute-pathname)
	     (with-slots (state identifier) instance
	       (setf state nil
		     identifier nil)
	       (return-from update-instance-from-resource instance))))
	 (file-mode (pathname)
	   (flet ((stat ()
		    (uiop:run-program
		     (list "stat" "-f" "%Dp" (namestring pathname))
		     :output :string))
		  (clip (mode)
		    (logand mode #o777)))
	     (clip (parse-integer (stat)))))
	 (file-content (pathname)
	   (with-output-to-string (contents)
	     (with-open-file (input pathname :external-format (slot-value instance 'external-format))
	       (loop :for char = (read-char input nil nil)
		     :while char
		     :do (write-char char contents))))))
    (ensure-pathname-is-set-when-identifier-is-set)
    (let ((absolute-pathname
	    (local-text-file-absolute-pathname instance)))
      (return-early-when-file-does-not-exist absolute-pathname)
      (with-slots (pathname state mode content checksum) instance
	(setf mode (file-mode absolute-pathname)
	      content (file-content absolute-pathname)
	      checksum (file-checksum absolute-pathname)
	      state t)))))

(defmethod create-resource ((instance local-text-file))
  (flet ((return-early-when-file-already-exists (pathname)
	   (when (probe-file pathname)
	     (resource-error 'create-resource instance
			     "File already exists."
			     "There is already an existing file under the pathname ~S
therefore the local text file ~A cannot be created." pathname instance)))
	 (create-empty-file (pathname)
	   (let ((octal-mode
		   (write-to-string 
		    (slot-value instance 'mode)
		    :base 8))
		 (path
		   (namestring pathname)))
	     (uiop:run-program (list "install" "-m" octal-mode "/dev/null" path))))
	 (write-content-to-file (pathname)
	   (with-slots (mode external-format content) instance
	     (with-open-file (output pathname :direction :output
					      :external-format external-format
					      :if-does-not-exist :error)
	       (write-string content output)))))
  (let ((absolute-pathname
	  (local-text-file-absolute-pathname instance)))
    (return-early-when-file-already-exists absolute-pathname)
    (create-empty-file absolute-pathname)
    (write-content-to-file absolute-pathname)
    (with-slots (identifier pathname) instance
      (setf identifier (namestring pathname)))
    (update-instance-from-resource instance))))

(defmethod delete-resource ((instance local-text-file))
  (flet ((return-early-when-file-does-not-exist (pathname)
	   (unless (probe-file pathname)
	     (with-slots (state identifier) instance
	       (resource-no-longer-exists
		'delete-resource instance
		"Cannot delete non-existent file."
		"There is no file under the pathname ~S
and therefore the local text file ~A cannot be deleted." pathname instance))))
	 (update-state-and-identifier ()
	   (with-slots (identifier state) instance
	     (setf state nil
		   identifier nil))))
    (let ((absolute-pathname
	    (local-text-file-absolute-pathname instance)))
      (return-early-when-file-does-not-exist absolute-pathname)
      (delete-file absolute-pathname)
      (update-state-and-identifier))))

;;;; End of file `local-filesystem-subtree.lisp'
