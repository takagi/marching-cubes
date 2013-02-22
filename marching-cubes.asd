#|
  This file is a part of marching-cubes project.
  Copyright (c) 2012 Masayuki Takagi (kamonama@gmail.com)
|#

#|
  Author: Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage marching-cubes-asd
  (:use :cl :asdf))
(in-package :marching-cubes-asd)

(defsystem marching-cubes
  :version "1.0"
  :author "Masayuki Takagi"
  :license "LLGPL"
  :depends-on ()
  :components ((:module "src"
                :serial t
                :components
                ((:file "package")
                 (:file "marching-cubes"))))
  :description "A marching cubes algorithm implementation in Common Lisp based on Paul Bourke's (http://paulbourke.net/geometry/polygonise/)"
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (load-op marching-cubes-test))))
