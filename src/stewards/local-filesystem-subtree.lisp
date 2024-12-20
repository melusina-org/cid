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

(defclass local-filesystem-subtree (steward)
  ((pathname
    :initform (error "A LOCAL-FILESYSTEM-SUBTREE requires a PATHNAME.")
    :type pathname
    :initarg :pathname
    :documentation "The root of the filesystem subtree owned by the steward."))
  (:default-initargs
   :description "A steward which owns files in a specific filesystem subtree.")
  (:documentation
   "A steward which owns files in a specific filesystem subtree.
In this subtree, text files can be created and updated."))

(defun make-local-filesystem-subtree (&rest initargs &key tenant project name displayname description pathname)
  "Make a LOCAL-FILESYSTEM-SUBTREE steward with the given parameters."
  (declare (ignore tenant project name displayname description pathname))
  (apply #'make-instance 'local-filesystem-subtree initargs))

(defmethod persistent-constructor ((class (eql 'local-filesystem-subtree)))
  'make-local-filesystem-subtree)

(defmethod persistent-slots append ((instance local-filesystem-subtree))
  '((:initarg :pathname
     :slot-name pathname)))

(defmethod list-resource-identifiers ((steward local-filesystem-subtree) (resource-class (eql 'local-text-file)))
  (loop :for absolute-pathname :in (uiop:directory-files (slot-value steward 'pathname))
	:for relative-pathname = (namestring (make-pathname :directory nil :defaults absolute-pathname))
	:when (portable-filename-character-set-p relative-pathname)
	:collect relative-pathname))


;;;;
;;;; Resources
;;;;

(defclass local-text-file (resource)
  ((steward-class
    :type symbol
    :initform 'local-filesystem-subtree
    :allocation :class)
   (pathname
    :type pathname
    :initarg :pathname
    :documentation "The pathname of the file relative to its steward.")
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
    :initarg :content))
  (:documentation
   "A text file on the local filesystem."))

(defun make-local-text-file (&rest initargs &key local-filesystem-subtree name displayname description
						 state identifier parent external
						 pathname mode content external-format)
  "Make a local text file."
  (declare (ignore name displayname description
		   state identifier parent external
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

(defmethod persistent-constructor ((class (eql 'local-text-file)))
  'make-local-text-file)

(defmethod persistent-slots append ((instance local-text-file))
  '((:initarg :local-filesystem-subtree
     :slot-name steward)
    (:initarg :pathname
     :slot-name pathname
     :immutable t)
    (:initarg :mode
     :slot-name mode)
    (:initarg :external-format
     :slot-name external-format)
    (:initarg :content
     :slot-name content)))

(defmethod examine-resource append ((instance local-text-file))
  (with-slots (pathname mode external-format content) instance
    (list
     :pathname pathname
     :mode mode
     :external-format external-format
     :content content)))

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
      (with-slots (state pathname mode content) instance
	(setf mode (file-mode absolute-pathname)
	      content (file-content absolute-pathname)
	      state t)))))

(defmethod create-resource ((instance local-text-file))
  (flet ((return-early-when-file-already-exists (pathname)
	   (when (probe-file pathname)
	     (resource-already-exists
	      'create-resource instance
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

(defmethod update-resource-from-instance ((instance local-text-file))
  (flet ((update-file-permissions (pathname)
	   (let ((octal-mode
		   (write-to-string 
		    (slot-value instance 'mode)
		    :base 8))
		 (path
		   (namestring pathname)))
	     (uiop:run-program (list "chmod" octal-mode path))))
	 (update-file-content (pathname)
	   (with-slots (mode external-format content) instance
	     (with-open-file (output pathname :direction :output
					      :external-format external-format
					      :if-does-not-exist :error)
	       (write-string content output)))))
  (let ((absolute-pathname
	  (local-text-file-absolute-pathname instance)))
    (update-file-permissions absolute-pathname)
    (update-file-content absolute-pathname))))


;;;;
;;;; Initialization File
;;;;

(defclass local-initialization-file (local-text-file)
  ((configuration
    :initarg :configuration
    :accessor configuration))
  (:documentation
   "An initialization file on the local filesystem.
The configuration is written in the INI format as the file content."))

(defun make-local-initialization-file (&rest initargs &key local-filesystem-subtree name displayname description
						 state identifier parent external
						 pathname mode content configuration external-format)
  "Make a local text file."
  (declare (ignore name displayname description
		   state identifier parent external
		   mode external-format))
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
    (apply #'make-instance 'local-initialization-file
	   :steward local-filesystem-subtree
	   :content (or content (write-initialization-file-to-string configuration))
	   (remove-properties initargs :local-filesystem-subtree :content))))

(defmethod persistent-constructor ((class (eql 'local-initialization-file)))
  'make-local-initialization-file)

(defmethod update-instance-from-resource :after ((instance local-initialization-file))
  (with-slots (content configuration) instance
    (setf configuration
      (read-initialization-file-from-string content))))

(defmethod update-resource-from-instance :before ((instance local-initialization-file))
  (with-slots (content configuration) instance
    (setf content
      (write-initialization-file-to-string configuration))))

;;;; End of file `local-filesystem-subtree.lisp'
