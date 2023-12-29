(in-package #:cl-user)

(handler-case
    (progn
      (require '#:asdf)
      (require '#:uiop)
      (require '#:org.melusina.cid/console))
  (error (c)
    (format *trace-output* "~&Failure: ~A~&" c)
    (quit :unix-status 1)))

(asdf:operate 'asdf:program-op '#:org.melusina.cid/console)
