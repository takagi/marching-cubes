#|
  This file is a part of marching-cubes project.
  Copyright (c) 2012 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage marching-cubes-test-asd
  (:use :cl :asdf))
(in-package :marching-cubes-test-asd)

(defsystem marching-cubes-test
  :author "Masayuki Takagi"
  :license "LLGPL"
  :depends-on (:marching-cubes
               :cl-test-more)
  :components ((:module "t"
                :serial t
                :components
                ((:file "package")
                 (:file "marching-cubes"))))
  :description "test for marching-cubes package"
  :perform (load-op :after (op c) (asdf:clear-system c)))
