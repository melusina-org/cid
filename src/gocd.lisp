;;;; gocd.lisp — Gocd integration

;;;; El Cid (https://github.com/melusina-org/cid)
;;;; This file is part of El Cid.
;;;;
;;;; Copyright © 2017–2023 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(in-package #:org.melusina.cid)

(defparameter *gocd-home-directory* #p"/home/go"
  "The directory holding GoCD home data.")

(defparameter *gocd-data-directory* #p"/var/gocd/godata/"
  "The directory holding GoCD data directory.")

(defparameter *gocd-working-directory* #p"/var/gocd/go-working-dir/"
  "The directory holding GoCD working directory.")

(defun gocd-dump (dumpname)
  "Dump GoCD data to a tarball whose names is built from DUMPNAME."
  (let ((tmpdir
	  (merge-pathnames (concatenate 'string (pathname-name dumpname) "/")
			   #p"/var/tmp/gocd/"))
	(tarballname
	  (concatenate 'string (namestring dumpname) ".gocd.txz")))
    (labels
	((dump (srcdir dumpdir &optional files)
	   (let ((script
		   (format nil "find ~a | cpio -dump ~a"
			   (or files ".")
			 (namestring (merge-pathnames dumpdir tmpdir)))))
	     (rashell:run-utility
	      (make-instance 'rashell:command
			     :directory srcdir
			     :program #p"/bin/sh"
			     :argv (list "-c" script))))))
      (bsd-install-directory
       (list
	(merge-pathnames #p"gocd/" tmpdir)
	(merge-pathnames #p"gocd/home/" tmpdir)
	(merge-pathnames #p"gocd/data/" tmpdir)
	(merge-pathnames #p"gocd/working/" tmpdir))
       :owner "go"
       :group "go"
       :mode #o750)
      (dump *gocd-home-directory* "gocd/home")
      (dump *gocd-data-directory* "gocd/data")
      (dump *gocd-working-directory* "gocd/working")
      (rashell:run-utility
       (make-instance 'rashell:command
		      :directory tmpdir
		      :program #p"/bin/tar"
		      :argv (list "cJf" tarballname ".")))
      (rashell:rm tmpdir :recursive t :force t)
      (values nil))))

(defun gocd-restore (dumpname)
  "Restore gocd data and repositories from a tarball DUMPNAME."
  (let ((tarballname
	  (concatenate 'string (namestring dumpname) ".gocd.txz")))
    (rashell:run-utility
     (make-instance 'rashell:command
		    :directory *gocd-home-directory*
		    :program #p"/bin/tar"
		    :argv (list "xJf" tarballname "--strip-components" "3" "./gocd/home")))
    (rashell:run-utility
     (make-instance 'rashell:command
		    :directory *gocd-data-directory*
		    :program #p"/bin/tar"
		    :argv (list "xJf" tarballname "--strip-components" "3" "./gocd/data")))
    (rashell:run-utility
     (make-instance 'rashell:command
		    :directory *gocd-working-directory*
		    :program #p"/bin/tar"
		    :argv (list "xJf" tarballname "--strip-components" "3" "./gocd/working")))
    (values nil)))

;;;; End of file `gocd.lisp'
