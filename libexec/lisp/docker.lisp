;;;; docker.lisp — Docker Support for El Cid

;;;; El Cid (https://github.com/melusina-org/cid)
;;;; This file is part of El Cid.
;;;;
;;;; Copyright © 2015–2024 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(defpackage #:org.melusina.cid/docker
  (:use #:common-lisp)
  (:export
   
   ;; Docker Volume
   #:volume
   #:volume-name
   #:volume-driver
   #:make-volume
   #:list-volumes
   #:find-volume
   #:update-volume
   #:create-volume
   #:delete-volume
   #:reclaim-volumes
   ;; Docker Image
   #:image
   #:image-name
   #:image-short-id
   #:image-id
   #:image-repository
   #:image-tag
   #:image-size
   #:image-created-at
   #:make-image
   #:list-images
   #:find-image
   #:update-image
   #:create-image
   #:delete-image
   #:reclaim-images
   ;; Miscellaneous
   #:info
   ))

(in-package #:org.melusina.cid/docker)


;;;;
;;;; Docker Volume
;;;;

(defclass volume ()
  ((name
    :initarg :name
    :initform (error "A NAME is required.")
    :reader volume-name)
   (driver
    :initarg :driver
    :initform "local"
    :reader volume-driver)))

(defun make-volume (&rest initargs &key name driver)
  (declare (ignore name driver))
  (apply #'make-instance 'volume initargs))

(defmethod print-object ((instance volume) stream)
  (print-unreadable-object (instance stream :type t :identity t)
    (with-slots (name driver) instance
      (format stream ":NAME ~S :DRIVER ~S" name driver))))

(defun volume-of-json (string)
  (let ((object
	  (yason:parse string)))
    (flet ((get-field (field kind)
	     (multiple-value-bind (value present-p) (gethash field object)
	       (unless present-p
		 (error "Cannot read field ~A from JSON description of volume instance." field))
	       (ecase kind
		 (:string
		  (string value))))))
      (make-instance
       'volume
       :name (get-field "Name" :string)
       :driver (get-field "Driver" :string)))))

(defun list-volumes ()
  (with-input-from-string
      (volume-list
       (uiop:run-program
	'("docker" "volume" "list" "--format" "json")
	:output :string))
    (loop :for line = (read-line volume-list nil nil)
	  :while line
	  :unless (string= "" line)
	  :collect (volume-of-json line))))

(defun find-volume (designator)
  "Find volume designated by DESIGNATOR."
  (typecase designator
    (volume     
     designator)
    (string
     (find designator (list-volumes)
	   :key #'volume-name
	   :test #'string=))))

(defun update-volume (volume)
  "Update VOLUME slots from its actual state."
  (let ((actual-state
	  (find-volume (volume-name volume))))
    (unless actual-state
      (return-from update-volume volume))
    (loop :for slot-name :in '(driver)
	  :do (setf (slot-value volume slot-name)
		    (slot-value actual-state slot-name)))
    (values volume)))

(defun create-volume (&key name driver)
  (unless name
    (error "A NAME is required."))
  (unless driver
    (setf driver "local"))
  (let ((existing-volume
	  (find-volume name)))
    (when existing-volume
      (return-from create-volume existing-volume)))
  (let ((created-volume
	  (uiop:run-program
	   (list "docker" "volume" "create" "--driver" driver name)
	   :output :string)))
    (setf created-volume
	  (string-trim '(#\Newline) created-volume))
    (unless (string= created-volume name)
      (error "Cannot create volume ~A" name))
    (make-volume :name name :driver driver)))

(defun delete-volume (volume)
  (with-slots (driver name) volume
    (let ((deleted-volume
	    (uiop:run-program
	     (list "docker" "volume" "rm" name)
	     :output :string)))
      (setf deleted-volume
	    (string-trim '(#\Newline) deleted-volume))
      (unless (string= deleted-volume name)
	(error "Cannot delete volume ~A" name))
      (values volume))))

(defun reclaim-volumes ()
  "Reclaim volumes which are no longer in use."
  (flet ((testsuite-p (volume)
	   (uiop:string-prefix-p "cid-testsuite" (volume-name volume))))
    (mapcar #'delete-volume
	    (loop :for volume :in (list-volumes)
		  :when  (testsuite-p volume)
		  :collect volume))))


;;;;
;;;; Docker Image
;;;;

(defclass image ()
  ((id
    :initarg :id
    :initform (error "An ID is required.")
    :reader image-id)
   (repository
    :initarg :repository
    :initform nil
    :reader image-repository)
   (tag
    :initarg :tag
    :initform nil
    :reader image-tag)
   (size
    :initarg :size
    :initform nil
    :reader image-size)
   (created-at
    :initarg :created-at
    :initform nil
    :reader image-created-at)))

(defun image-name (image)
  (with-slots (repository tag) image
    (when (and repository tag)
      (concatenate 'string repository '(#\:) tag))))

(defun image-short-id (image)
  (with-slots (id) image
    (when id
      (let ((start
	      (1+ (position #\: id)))
	    (length
	      12))
      (subseq id start (+ start length))))))

(defun make-image (&rest initargs &key id repository tag size created-at)
  (declare (ignore id repository tag size created-at))
  (apply #'make-instance 'image initargs))

(defmethod print-object ((instance image) stream)
  (print-unreadable-object (instance stream :type t :identity t)
    (with-slots (id repository tag size created-at) instance
      (format stream "~@[:REPOSITORY ~S ~]~@[:TAG ~S ~]:ID ~S :SIZE ~S :CREATED-AT ~S" repository tag id size created-at))))

(defun image-of-json (string)
  (let ((object
	  (yason:parse string)))
    (flet ((get-field (field kind)
	     (multiple-value-bind (value present-p) (gethash field object)
	       (unless present-p
		 (error "Cannot read field ~A from JSON description of image instance." field))
	       (ecase kind
		 (:string
		  (string value))
		 (:nullable-string
		  (unless (string= "<none>" value)
		    (string value)))))))
      (make-instance
       'image
       :id (get-field "ID" :string)
       :repository (get-field "Repository" :nullable-string)
       :tag (get-field "Tag" :nullable-string)
       :size (get-field "Size" :string)
       :created-at (get-field "CreatedAt" :string)))))

(defun list-images ()
  (with-input-from-string
      (image-list
       (uiop:run-program
	'("docker" "image" "list" "--no-trunc" "--format" "json")
	:output :string))
    (loop :for line = (read-line image-list nil nil)
	  :while line
	  :unless (string= "" line)
	  :collect (image-of-json line))))

(defun find-image (designator)
  "Find image designated by DESIGNATOR."
  (typecase designator
    (image     
     designator)
    (string
     (or
      (find designator (list-images)
	    :key #'image-name
	    :test #'string=)
      (when (= 12 (length designator))
	(find designator (list-images)
	      :key #'image-short-id
	      :test #'string=))
      (when (position #\: designator)
	(find designator (list-images)
	      :key #'image-id
	      :test #'string=))
      (find (concatenate 'string "sha256:" designator) (list-images)
	    :key #'image-id
	    :test #'string=)))))

(defun update-image (image)
  "Update IMAGE slots from its actual state."
  (let ((actual-state
	  (find-image (image-name image))))
    (unless actual-state
      (setf (slot-value image 'id) nil)
      (return-from update-image image))
    (loop :for slot-name :in '(id repository tag size created-at)
	  :do (setf (slot-value image slot-name)
		    (slot-value actual-state slot-name)))
    (values image)))

(defun create-image (&key dockerfile context repository tag (cache t) (build-time-variables))
  (unless tag
    (setf tag "latest"))
  (unless context
    (setf context #p"."))
  (unless dockerfile
    (setf dockerfile (merge-pathnames #p"Dockerfile" context)))
  (let ((image-tag
	  (concatenate 'string repository '(#\:) tag)))
    (uiop:run-program
     (append
      (list "docker" "image" "build")
      (unless cache
	(list "--no-cache"))
      (when build-time-variables
	(loop :for (name . value) :in build-time-variables
	      :append (list "--build-arg" (concatenate 'string name "=" value))))
      (list "--file" (namestring dockerfile))
      (list "--tag" image-tag)
      (list (namestring context)))
     :output t
     :error-output t)
    (find-image image-tag)))

(defun delete-image (image)
  (let ((deleted-image
	  (uiop:run-program
	   (list "docker" "image" "rm"
		 (or (image-name image)
		     (image-id image)))
	   :output :string)))
    (flet ((compare-event (slot-reader regex)
	     (declare (optimize debug))
	     (ppcre:register-groups-bind (value) (regex deleted-image)
	       (unless (string= value (funcall slot-reader image))
		 (error "Cannot delete image ~A" (image-name image))))))
      (compare-event #'image-name "Untagged: (.*)")
      (compare-event #'image-id "Deleted: (.*)"))
    (setf (slot-value image 'id) nil)
    (values image)))

(defun reclaim-images ()
  "Reclaim docker images which are no longer in use."
  (flet ((has-no-name-p (image)
	   (not (image-name image)))
	 (testsuite-p (image)
	   (uiop:string-prefix-p "testsuite" (image-tag image))))
    (mapcar #'delete-image
	    (loop :for image :in (list-images)
		  :when (or
			 (testsuite-p image)
			 (has-no-name-p image))
		  :collect image))))

(defun info ()
  "Retrieve docker info."
  (yason:parse
   (uiop:run-program
    '("docker" "info" "--format" "json")
    :output :string)))
 
;;;; End of file `docker.lisp'
