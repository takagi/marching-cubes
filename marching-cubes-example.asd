#|
  This file is a part of marching-cubes project.
  Copyright (c) 2012 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage marching-cubes-example-asd
  (:use :cl :asdf))
(in-package :marching-cubes-example-asd)

(defsystem marching-cubes-example
  :author "Masayuki Takagi"
  :license "LLGPL"
  :depends-on (:marching-cubes)
  :components ((:module "example"
                :serial t
                :components
                ((:file "package")
                 (:file "blob1"))))
  :perform (load-op :after (op c) (asdf:clear-system c)))
