;;;; cid.lisp -- Load El Cid

;;;; El Cid (https://github.com/melusina-org/cid)
;;;; This file is part of El Cid
;;;;
;;;; Copyright © 2017 Michael Le Barbier Grünewald
;;;;
;;;; This file must be used under the terms of the MIT license.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at
;;;; https://opensource.org/licenses/MIT

(in-package #:asdf-user)
(defsystem #:cid
  :description "El Cid, Count of Bivar and Prince of Continuous Integration and Deployment Systems."
  :author "Michael Le Barbier Grünewald"
  :license "MIT"
  :depends-on (#:cl-base64)
  :components
  ((:module "src"
    :components ((:file "dockerfile")))))

;;;; End of file `cid.asd'
