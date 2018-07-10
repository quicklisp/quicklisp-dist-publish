;;;; quicklisp-dist-publish.asd

(asdf:defsystem #:quicklisp-dist-publish
  :serial t
  :depends-on (#:zs3
               #:quicklisp
               #:release-report
               #:cl-ppcre
               #:drakma
               #:ironclad
               #:commando)
  :components ((:file "dist-index")))
