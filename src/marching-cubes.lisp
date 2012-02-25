#|
  This file is a part of marching-cubes project.
  Copyright (c) 2012 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :marching-cubes)


;; vec3

(defstruct (vec3 (:constructor make-vec3 (x y z)))
  (x 0 :read-only t)
  (y 0 :read-only t)
  (z 0 :read-only t))

(defun vec3= (a b)
  (and (= (vec3-x a) (vec3-x b))
       (= (vec3-y a) (vec3-y b))
       (= (vec3-z a) (vec3-z b))))


;; triangle

(defstruct (triangle (:conc-name nil)
                     (:constructor make-triangle (vertex-0 vertex-1 vertex-2)))
  (vertex-0 nil :read-only t)
  (vertex-1 nil :read-only t)
  (vertex-2 nil :read-only t))

(defun triangle= (a b)
  (and (vec3= (vertex-0 a) (vertex-0 b))
       (vec3= (vertex-1 a) (vertex-1 b))
       (vec3= (vertex-2 a) (vertex-2 b))))

       
;; grid-cell

(defstruct (grid-cell (:constructor make-grid-cell (vertices% values%)))
  vertices% values%)

(defmacro grid-cell-vertex (grid i)
  `(aref (grid-cell-vertices% ,grid) ,i))

(defmacro grid-cell-value (grid i)
  `(aref (grid-cell-values% ,grid) ,i))


;; grid

(defstruct (grid (:constructor make-raw-grid (min max size-x size-y size-z
                                                  delta values%)))
  min max size-x size-y size-z delta values%)

(defun make-grid (fn min max delta)
  (labels ((size (min max delta)
             (assert (and (< min max) (< 0 delta)))
             (multiple-value-bind (x y) (floor (/ (- max min) delta))
               (if (= y 0) x (1+ x)))))
    (let ((i (size (vec3-x min) (vec3-x max) delta))
          (j (size (vec3-y min) (vec3-y max) delta))
          (k (size (vec3-z min) (vec3-z max) delta)))
      (let ((max (make-vec3 (+ (vec3-x min) (* i delta))
                            (+ (vec3-y min) (* j delta))
                            (+ (vec3-z min) (* k delta))))
            (values% (make-grid-values fn i j k min delta)))
        (make-raw-grid min max i j k delta values%)))))

(defun make-grid-values (fn i j k origin delta)
  (let ((values (make-array (list (1+ i) (1+ j) (1+ k)))))
    (dotimes (x (1+ i))
      (dotimes (y (1+ j))
        (dotimes (z (1+ k))
          (setf (aref values x y z)
                (funcall fn
                         (+ (vec3-x origin) (* x delta))
                         (+ (vec3-y origin) (* y delta))
                         (+ (vec3-z origin) (* z delta)))))))
    values))
  
(defun reduce-grid (grid fn)
  (assert (grid-p grid))
  (let ((triangles nil))
    (dotimes (i (grid-size-x grid))
      (dotimes (j (grid-size-y grid))
        (dotimes (k (grid-size-z grid))
          (setf triangles (append (funcall fn (grid-cell grid i j k))
                                  triangles)))))
    triangles))

(defun grid-cell (grid i j k)
  (let ((vertices (grid-cell-vertices grid i j k))
        (values (grid-cell-values grid i j k)))
    (make-grid-cell vertices values)))

(defvar *vertex-offsets* '((0 0 0) (1 0 0) (1 1 0) (0 1 0)
                           (0 0 1) (1 0 1) (1 1 1) (0 1 1)))

(defun grid-cell-vertices (grid i j k)
  (let ((vertices (make-array '(8))))
    (dotimes (n 8)
      (destructuring-bind (di dj dk) (nth n *vertex-offsets*)
        (setf (aref vertices n)
              (grid-point grid (+ i di) (+ j dj) (+ k dk)))))
    vertices))

(defun grid-cell-values (grid i j k)
  (let ((values (make-array '(8))))
    (dotimes (n 8)
      (destructuring-bind (di dj dk) (nth n *vertex-offsets*)
        (setf (aref values n)
              (grid-value grid (+ i di) (+ j dj) (+ k dk)))))
    values))

(defun grid-point (grid i j k)
  (assert (and (grid-p grid)
               (<= 0 i) (<= i (grid-size-x grid))
               (<= 0 j) (<= j (grid-size-y grid))
               (<= 0 k) (<= k (grid-size-z grid))))
  (let ((min (grid-min grid))
        (delta (grid-delta grid)))
    (make-vec3 (+ (vec3-x min) (* i delta))
               (+ (vec3-y min) (* j delta))
               (+ (vec3-z min) (* k delta)))))

(defun grid-value (grid i j k)
  (assert (and (grid-p grid)
               (<= 0 i) (<= i (grid-size-x grid))
               (<= 0 j) (<= j (grid-size-y grid))
               (<= 0 k) (<= k (grid-size-z grid))))
  (aref (grid-values% grid) i j k))


;; utility

(defmacro inc-logior (x val)
  `(setf ,x (logior ,x ,val)))


;; main

(defun marching-cubes (fn min max delta isolevel)
  (let ((grid (make-grid fn min max delta)))
    (reduce-grid grid (lambda (cell)
                        (polygonise cell isolevel)))))

(defun polygonise (grid isolevel)
  (let ((cube-index 0)
        (vert-array (make-array 12)))
    
    ; Determin the index into the edge table which
    ; tells us which vertices are inside of the surface
    (when (< (grid-cell-value grid 0) isolevel) (inc-logior cube-index 1))
    (when (< (grid-cell-value grid 1) isolevel) (inc-logior cube-index 2))
    (when (< (grid-cell-value grid 2) isolevel) (inc-logior cube-index 4))
    (when (< (grid-cell-value grid 3) isolevel) (inc-logior cube-index 8))
    (when (< (grid-cell-value grid 4) isolevel) (inc-logior cube-index 16))
    (when (< (grid-cell-value grid 5) isolevel) (inc-logior cube-index 32))
    (when (< (grid-cell-value grid 6) isolevel) (inc-logior cube-index 64))
    (when (< (grid-cell-value grid 7) isolevel) (inc-logior cube-index 128))
    
    ; Cube is entirely in/out of the surface
    (when (= (aref *edge-table* cube-index) 0)
      (return-from polygonise nil))
    
    ; Find the vertices where the surface intersects the cube
    (when (logand (aref *edge-table* cube-index) 1)
      (setf (aref vert-array 0)
            (vertex-interop isolevel
                            (grid-cell-vertex grid 0) (grid-cell-vertex grid 1)
                            (grid-cell-value grid 0) (grid-cell-value grid 1))))
    (when (logand (aref *edge-table* cube-index) 2)
      (setf (aref vert-array 1)
            (vertex-interop isolevel
                            (grid-cell-vertex grid 1) (grid-cell-vertex grid 2)
                            (grid-cell-value grid 1) (grid-cell-value grid 2))))
    (when (logand (aref *edge-table* cube-index) 4)
      (setf (aref vert-array 2)
            (vertex-interop isolevel
                            (grid-cell-vertex grid 2) (grid-cell-vertex grid 3)
                            (grid-cell-value grid 2) (grid-cell-value grid 3))))
    (when (logand (aref *edge-table* cube-index) 8)
      (setf (aref vert-array 3)
            (vertex-interop isolevel
                            (grid-cell-vertex grid 3) (grid-cell-vertex grid 0)
                            (grid-cell-value grid 3) (grid-cell-value grid 0))))
    (when (logand (aref *edge-table* cube-index) 16)
      (setf (aref vert-array 4)
            (vertex-interop isolevel
                            (grid-cell-vertex grid 4) (grid-cell-vertex grid 5)
                            (grid-cell-value grid 4) (grid-cell-value grid 5))))
    (when (logand (aref *edge-table* cube-index) 32)
      (setf (aref vert-array 5)
            (vertex-interop isolevel
                            (grid-cell-vertex grid 5) (grid-cell-vertex grid 6)
                            (grid-cell-value grid 5) (grid-cell-value grid 6))))
    (when (logand (aref *edge-table* cube-index) 64)
      (setf (aref vert-array 6)
            (vertex-interop isolevel
                            (grid-cell-vertex grid 6) (grid-cell-vertex grid 7)
                            (grid-cell-value grid 6) (grid-cell-value grid 7))))
    (when (logand (aref *edge-table* cube-index) 128)
      (setf (aref vert-array 7)
            (vertex-interop isolevel
                            (grid-cell-vertex grid 7) (grid-cell-vertex grid 4)
                            (grid-cell-value grid 7) (grid-cell-value grid 4))))
    (when (logand (aref *edge-table* cube-index) 256)
      (setf (aref vert-array 8)
            (vertex-interop isolevel
                            (grid-cell-vertex grid 0) (grid-cell-vertex grid 4)
                            (grid-cell-value grid 0) (grid-cell-value grid 4))))
    (when (logand (aref *edge-table* cube-index) 512)
      (setf (aref vert-array 9)
            (vertex-interop isolevel
                            (grid-cell-vertex grid 1) (grid-cell-vertex grid 5)
                            (grid-cell-value grid 1) (grid-cell-value grid 5))))
    (when (logand (aref *edge-table* cube-index) 1024)
      (setf (aref vert-array 10)
            (vertex-interop isolevel
                            (grid-cell-vertex grid 2) (grid-cell-vertex grid 6)
                            (grid-cell-value grid 2) (grid-cell-value grid 6))))
    (when (logand (aref *edge-table* cube-index) 2048)
      (setf (aref vert-array 11)
            (vertex-interop isolevel
                            (grid-cell-vertex grid 3) (grid-cell-vertex grid 7)
                            (grid-cell-value grid 3) (grid-cell-value grid 7))))
    
    ; Create the triangle
    (loop for i from 0 by 3
       while (/= (aref *tri-table* cube-index i) -1)
       collect (make-triangle
                (aref vert-array (aref *tri-table* cube-index i))
                (aref vert-array (aref *tri-table* cube-index (+ i 1)))
                (aref vert-array (aref *tri-table* cube-index (+ i 2)))))))

(defun vertex-interop (isolevel p1 p2 val1 val2)
  (when (< (abs (- isolevel val1)) 0.00001)
    (return-from vertex-interop p1))
  (when (< (abs (- isolevel val2)) 0.00001)
    (return-from vertex-interop p2))
  (when (< (abs (- val1 val2)) 0.00001)
    (return-from vertex-interop p1))
  (let ((mu (/ (- isolevel val1) (- val2 val1))))
    (make-vec3 (+ (vec3-x p1) (* mu (- (vec3-x p2) (vec3-x p1))))
               (+ (vec3-y p1) (* mu (- (vec3-y p2) (vec3-y p1))))
               (+ (vec3-z p1) (* mu (- (vec3-z p2) (vec3-z p1)))))))


;; tables

(defparameter *edge-table*
  (make-array
    '(256)
    :element-type 'fixnum
    :initial-contents
    '(#x0 #x109 #x203 #x30a #x406 #x50f #x605 #x70c
      #x80c #x905 #xa0f #xb06 #xc0a #xd03 #xe09 #xf00
      #x190 #x99  #x393 #x29a #x596 #x49f #x795 #x69c
      #x99c #x895 #xb9f #xa96 #xd9a #xc93 #xf99 #xe90
      #x230 #x339 #x33  #x13a #x636 #x73f #x435 #x53c
      #xa3c #xb35 #x83f #x936 #xe3a #xf33 #xc39 #xd30
      #x3a0 #x2a9 #x1a3 #xaa  #x7a6 #x6af #x5a5 #x4ac
      #xbac #xaa5 #x9af #x8a6 #xfaa #xea3 #xda9 #xca0
      #x460 #x569 #x663 #x76a #x66  #x16f #x265 #x36c
      #xc6c #xd65 #xe6f #xf66 #x86a #x963 #xa69 #xb60
      #x5f0 #x4f9 #x7f3 #x6fa #x1f6 #xff  #x3f5 #x2fc
      #xdfc #xcf5 #xfff #xef6 #x9fa #x8f3 #xbf9 #xaf0
      #x650 #x759 #x453 #x55a #x256 #x35f #x55  #x15c
      #xe5c #xf55 #xc5f #xd56 #xa5a #xb53 #x859 #x950
      #x7c0 #x6c9 #x5c3 #x4ca #x3c6 #x2cf #x1c5 #xcc 
      #xfcc #xec5 #xdcf #xcc6 #xbca #xac3 #x9c9 #x8c0
      #x8c0 #x9c9 #xac3 #xbca #xcc6 #xdcf #xec5 #xfcc
      #xcc  #x1c5 #x2cf #x3c6 #x4ca #x5c3 #x6c9 #x7c0
      #x950 #x859 #xb53 #xa5a #xd56 #xc5f #xf55 #xe5c
      #x15c #x55  #x35f #x256 #x55a #x453 #x759 #x650
      #xaf0 #xbf9 #x8f3 #x9fa #xef6 #xfff #xcf5 #xdfc
      #x2fc #x3f5 #xff  #x1f6 #x6fa #x7f3 #x4f9 #x5f0
      #xb60 #xa69 #x963 #x86a #xf66 #xe6f #xd65 #xc6c
      #x36c #x265 #x16f #x66  #x76a #x663 #x569 #x460
      #xca0 #xda9 #xea3 #xfaa #x8a6 #x9af #xaa5 #xbac
      #x4ac #x5a5 #x6af #x7a6 #xaa  #x1a3 #x2a9 #x3a0
      #xd30 #xc39 #xf33 #xe3a #x936 #x83f #xb35 #xa3c
      #x53c #x435 #x73f #x636 #x13a #x33  #x339 #x230
      #xe90 #xf99 #xc93 #xd9a #xa96 #xb9f #x895 #x99c
      #x69c #x795 #x49f #x596 #x29a #x393 #x99  #x190
      #xf00 #xe09 #xd03 #xc0a #xb06 #xa0f #x905 #x80c
      #x70c #x605 #x50f #x406 #x30a #x203 #x109 #x0)))

(defparameter *tri-table*
  (make-array
   '(256 16)
   :element-type 'fixnum
   :initial-contents
   '((-1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     (0 8 3 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     (0 1 9 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     (1 8 3 9 8 1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     (1 2 10 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     (0 8 3 1 2 10 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     (9 2 10 0 2 9 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     (2 8 3 2 10 8 10 9 8 -1 -1 -1 -1 -1 -1 -1)
     (3 11 2 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     (0 11 2 8 11 0 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     (1 9 0 2 3 11 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     (1 11 2 1 9 11 9 8 11 -1 -1 -1 -1 -1 -1 -1)
     (3 10 1 11 10 3 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     (0 10 1 0 8 10 8 11 10 -1 -1 -1 -1 -1 -1 -1)
     (3 9 0 3 11 9 11 10 9 -1 -1 -1 -1 -1 -1 -1)
     (9 8 10 10 8 11 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     (4 7 8 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     (4 3 0 7 3 4 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     (0 1 9 8 4 7 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     (4 1 9 4 7 1 7 3 1 -1 -1 -1 -1 -1 -1 -1)
     (1 2 10 8 4 7 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     (3 4 7 3 0 4 1 2 10 -1 -1 -1 -1 -1 -1 -1)
     (9 2 10 9 0 2 8 4 7 -1 -1 -1 -1 -1 -1 -1)
     (2 10 9 2 9 7 2 7 3 7 9 4 -1 -1 -1 -1)
     (8 4 7 3 11 2 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     (11 4 7 11 2 4 2 0 4 -1 -1 -1 -1 -1 -1 -1)
     (9 0 1 8 4 7 2 3 11 -1 -1 -1 -1 -1 -1 -1)
     (4 7 11 9 4 11 9 11 2 9 2 1 -1 -1 -1 -1)
     (3 10 1 3 11 10 7 8 4 -1 -1 -1 -1 -1 -1 -1)
     (1 11 10 1 4 11 1 0 4 7 11 4 -1 -1 -1 -1)
     (4 7 8 9 0 11 9 11 10 11 0 3 -1 -1 -1 -1)
     (4 7 11 4 11 9 9 11 10 -1 -1 -1 -1 -1 -1 -1)
     (9 5 4 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     (9 5 4 0 8 3 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     (0 5 4 1 5 0 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     (8 5 4 8 3 5 3 1 5 -1 -1 -1 -1 -1 -1 -1)
     (1 2 10 9 5 4 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     (3 0 8 1 2 10 4 9 5 -1 -1 -1 -1 -1 -1 -1)
     (5 2 10 5 4 2 4 0 2 -1 -1 -1 -1 -1 -1 -1)
     (2 10 5 3 2 5 3 5 4 3 4 8 -1 -1 -1 -1)
     (9 5 4 2 3 11 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     (0 11 2 0 8 11 4 9 5 -1 -1 -1 -1 -1 -1 -1)
     (0 5 4 0 1 5 2 3 11 -1 -1 -1 -1 -1 -1 -1)
     (2 1 5 2 5 8 2 8 11 4 8 5 -1 -1 -1 -1)
     (10 3 11 10 1 3 9 5 4 -1 -1 -1 -1 -1 -1 -1)
     (4 9 5 0 8 1 8 10 1 8 11 10 -1 -1 -1 -1)
     (5 4 0 5 0 11 5 11 10 11 0 3 -1 -1 -1 -1)
     (5 4 8 5 8 10 10 8 11 -1 -1 -1 -1 -1 -1 -1)
     (9 7 8 5 7 9 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     (9 3 0 9 5 3 5 7 3 -1 -1 -1 -1 -1 -1 -1)
     (0 7 8 0 1 7 1 5 7 -1 -1 -1 -1 -1 -1 -1)
     (1 5 3 3 5 7 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     (9 7 8 9 5 7 10 1 2 -1 -1 -1 -1 -1 -1 -1)
     (10 1 2 9 5 0 5 3 0 5 7 3 -1 -1 -1 -1)
     (8 0 2 8 2 5 8 5 7 10 5 2 -1 -1 -1 -1)
     (2 10 5 2 5 3 3 5 7 -1 -1 -1 -1 -1 -1 -1)
     (7 9 5 7 8 9 3 11 2 -1 -1 -1 -1 -1 -1 -1)
     (9 5 7 9 7 2 9 2 0 2 7 11 -1 -1 -1 -1)
     (2 3 11 0 1 8 1 7 8 1 5 7 -1 -1 -1 -1)
     (11 2 1 11 1 7 7 1 5 -1 -1 -1 -1 -1 -1 -1)
     (9 5 8 8 5 7 10 1 3 10 3 11 -1 -1 -1 -1)
     (5 7 0 5 0 9 7 11 0 1 0 10 11 10 0 -1)
     (11 10 0 11 0 3 10 5 0 8 0 7 5 7 0 -1)
     (11 10 5 7 11 5 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     (10 6 5 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     (0 8 3 5 10 6 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     (9 0 1 5 10 6 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     (1 8 3 1 9 8 5 10 6 -1 -1 -1 -1 -1 -1 -1)
     (1 6 5 2 6 1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     (1 6 5 1 2 6 3 0 8 -1 -1 -1 -1 -1 -1 -1)
     (9 6 5 9 0 6 0 2 6 -1 -1 -1 -1 -1 -1 -1)
     (5 9 8 5 8 2 5 2 6 3 2 8 -1 -1 -1 -1)
     (2 3 11 10 6 5 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     (11 0 8 11 2 0 10 6 5 -1 -1 -1 -1 -1 -1 -1)
     (0 1 9 2 3 11 5 10 6 -1 -1 -1 -1 -1 -1 -1)
     (5 10 6 1 9 2 9 11 2 9 8 11 -1 -1 -1 -1)
     (6 3 11 6 5 3 5 1 3 -1 -1 -1 -1 -1 -1 -1)
     (0 8 11 0 11 5 0 5 1 5 11 6 -1 -1 -1 -1)
     (3 11 6 0 3 6 0 6 5 0 5 9 -1 -1 -1 -1)
     (6 5 9 6 9 11 11 9 8 -1 -1 -1 -1 -1 -1 -1)
     (5 10 6 4 7 8 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     (4 3 0 4 7 3 6 5 10 -1 -1 -1 -1 -1 -1 -1)
     (1 9 0 5 10 6 8 4 7 -1 -1 -1 -1 -1 -1 -1)
     (10 6 5 1 9 7 1 7 3 7 9 4 -1 -1 -1 -1)
     (6 1 2 6 5 1 4 7 8 -1 -1 -1 -1 -1 -1 -1)
     (1 2 5 5 2 6 3 0 4 3 4 7 -1 -1 -1 -1)
     (8 4 7 9 0 5 0 6 5 0 2 6 -1 -1 -1 -1)
     (7 3 9 7 9 4 3 2 9 5 9 6 2 6 9 -1)
     (3 11 2 7 8 4 10 6 5 -1 -1 -1 -1 -1 -1 -1)
     (5 10 6 4 7 2 4 2 0 2 7 11 -1 -1 -1 -1)
     (0 1 9 4 7 8 2 3 11 5 10 6 -1 -1 -1 -1)
     (9 2 1 9 11 2 9 4 11 7 11 4 5 10 6 -1)
     (8 4 7 3 11 5 3 5 1 5 11 6 -1 -1 -1 -1)
     (5 1 11 5 11 6 1 0 11 7 11 4 0 4 11 -1)
     (0 5 9 0 6 5 0 3 6 11 6 3 8 4 7 -1)
     (6 5 9 6 9 11 4 7 9 7 11 9 -1 -1 -1 -1)
     (10 4 9 6 4 10 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     (4 10 6 4 9 10 0 8 3 -1 -1 -1 -1 -1 -1 -1)
     (10 0 1 10 6 0 6 4 0 -1 -1 -1 -1 -1 -1 -1)
     (8 3 1 8 1 6 8 6 4 6 1 10 -1 -1 -1 -1)
     (1 4 9 1 2 4 2 6 4 -1 -1 -1 -1 -1 -1 -1)
     (3 0 8 1 2 9 2 4 9 2 6 4 -1 -1 -1 -1)
     (0 2 4 4 2 6 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     (8 3 2 8 2 4 4 2 6 -1 -1 -1 -1 -1 -1 -1)
     (10 4 9 10 6 4 11 2 3 -1 -1 -1 -1 -1 -1 -1)
     (0 8 2 2 8 11 4 9 10 4 10 6 -1 -1 -1 -1)
     (3 11 2 0 1 6 0 6 4 6 1 10 -1 -1 -1 -1)
     (6 4 1 6 1 10 4 8 1 2 1 11 8 11 1 -1)
     (9 6 4 9 3 6 9 1 3 11 6 3 -1 -1 -1 -1)
     (8 11 1 8 1 0 11 6 1 9 1 4 6 4 1 -1)
     (3 11 6 3 6 0 0 6 4 -1 -1 -1 -1 -1 -1 -1)
     (6 4 8 11 6 8 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     (7 10 6 7 8 10 8 9 10 -1 -1 -1 -1 -1 -1 -1)
     (0 7 3 0 10 7 0 9 10 6 7 10 -1 -1 -1 -1)
     (10 6 7 1 10 7 1 7 8 1 8 0 -1 -1 -1 -1)
     (10 6 7 10 7 1 1 7 3 -1 -1 -1 -1 -1 -1 -1)
     (1 2 6 1 6 8 1 8 9 8 6 7 -1 -1 -1 -1)
     (2 6 9 2 9 1 6 7 9 0 9 3 7 3 9 -1)
     (7 8 0 7 0 6 6 0 2 -1 -1 -1 -1 -1 -1 -1)
     (7 3 2 6 7 2 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     (2 3 11 10 6 8 10 8 9 8 6 7 -1 -1 -1 -1)
     (2 0 7 2 7 11 0 9 7 6 7 10 9 10 7 -1)
     (1 8 0 1 7 8 1 10 7 6 7 10 2 3 11 -1)
     (11 2 1 11 1 7 10 6 1 6 7 1 -1 -1 -1 -1)
     (8 9 6 8 6 7 9 1 6 11 6 3 1 3 6 -1)
     (0 9 1 11 6 7 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     (7 8 0 7 0 6 3 11 0 11 6 0 -1 -1 -1 -1)
     (7 11 6 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     (7 6 11 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     (3 0 8 11 7 6 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     (0 1 9 11 7 6 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     (8 1 9 8 3 1 11 7 6 -1 -1 -1 -1 -1 -1 -1)
     (10 1 2 6 11 7 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     (1 2 10 3 0 8 6 11 7 -1 -1 -1 -1 -1 -1 -1)
     (2 9 0 2 10 9 6 11 7 -1 -1 -1 -1 -1 -1 -1)
     (6 11 7 2 10 3 10 8 3 10 9 8 -1 -1 -1 -1)
     (7 2 3 6 2 7 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     (7 0 8 7 6 0 6 2 0 -1 -1 -1 -1 -1 -1 -1)
     (2 7 6 2 3 7 0 1 9 -1 -1 -1 -1 -1 -1 -1)
     (1 6 2 1 8 6 1 9 8 8 7 6 -1 -1 -1 -1)
     (10 7 6 10 1 7 1 3 7 -1 -1 -1 -1 -1 -1 -1)
     (10 7 6 1 7 10 1 8 7 1 0 8 -1 -1 -1 -1)
     (0 3 7 0 7 10 0 10 9 6 10 7 -1 -1 -1 -1)
     (7 6 10 7 10 8 8 10 9 -1 -1 -1 -1 -1 -1 -1)
     (6 8 4 11 8 6 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     (3 6 11 3 0 6 0 4 6 -1 -1 -1 -1 -1 -1 -1)
     (8 6 11 8 4 6 9 0 1 -1 -1 -1 -1 -1 -1 -1)
     (9 4 6 9 6 3 9 3 1 11 3 6 -1 -1 -1 -1)
     (6 8 4 6 11 8 2 10 1 -1 -1 -1 -1 -1 -1 -1)
     (1 2 10 3 0 11 0 6 11 0 4 6 -1 -1 -1 -1)
     (4 11 8 4 6 11 0 2 9 2 10 9 -1 -1 -1 -1)
     (10 9 3 10 3 2 9 4 3 11 3 6 4 6 3 -1)
     (8 2 3 8 4 2 4 6 2 -1 -1 -1 -1 -1 -1 -1)
     (0 4 2 4 6 2 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     (1 9 0 2 3 4 2 4 6 4 3 8 -1 -1 -1 -1)
     (1 9 4 1 4 2 2 4 6 -1 -1 -1 -1 -1 -1 -1)
     (8 1 3 8 6 1 8 4 6 6 10 1 -1 -1 -1 -1)
     (10 1 0 10 0 6 6 0 4 -1 -1 -1 -1 -1 -1 -1)
     (4 6 3 4 3 8 6 10 3 0 3 9 10 9 3 -1)
     (10 9 4 6 10 4 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     (4 9 5 7 6 11 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     (0 8 3 4 9 5 11 7 6 -1 -1 -1 -1 -1 -1 -1)
     (5 0 1 5 4 0 7 6 11 -1 -1 -1 -1 -1 -1 -1)
     (11 7 6 8 3 4 3 5 4 3 1 5 -1 -1 -1 -1)
     (9 5 4 10 1 2 7 6 11 -1 -1 -1 -1 -1 -1 -1)
     (6 11 7 1 2 10 0 8 3 4 9 5 -1 -1 -1 -1)
     (7 6 11 5 4 10 4 2 10 4 0 2 -1 -1 -1 -1)
     (3 4 8 3 5 4 3 2 5 10 5 2 11 7 6 -1)
     (7 2 3 7 6 2 5 4 9 -1 -1 -1 -1 -1 -1 -1)
     (9 5 4 0 8 6 0 6 2 6 8 7 -1 -1 -1 -1)
     (3 6 2 3 7 6 1 5 0 5 4 0 -1 -1 -1 -1)
     (6 2 8 6 8 7 2 1 8 4 8 5 1 5 8 -1)
     (9 5 4 10 1 6 1 7 6 1 3 7 -1 -1 -1 -1)
     (1 6 10 1 7 6 1 0 7 8 7 0 9 5 4 -1)
     (4 0 10 4 10 5 0 3 10 6 10 7 3 7 10 -1)
     (7 6 10 7 10 8 5 4 10 4 8 10 -1 -1 -1 -1)
     (6 9 5 6 11 9 11 8 9 -1 -1 -1 -1 -1 -1 -1)
     (3 6 11 0 6 3 0 5 6 0 9 5 -1 -1 -1 -1)
     (0 11 8 0 5 11 0 1 5 5 6 11 -1 -1 -1 -1)
     (6 11 3 6 3 5 5 3 1 -1 -1 -1 -1 -1 -1 -1)
     (1 2 10 9 5 11 9 11 8 11 5 6 -1 -1 -1 -1)
     (0 11 3 0 6 11 0 9 6 5 6 9 1 2 10 -1)
     (11 8 5 11 5 6 8 0 5 10 5 2 0 2 5 -1)
     (6 11 3 6 3 5 2 10 3 10 5 3 -1 -1 -1 -1)
     (5 8 9 5 2 8 5 6 2 3 8 2 -1 -1 -1 -1)
     (9 5 6 9 6 0 0 6 2 -1 -1 -1 -1 -1 -1 -1)
     (1 5 8 1 8 0 5 6 8 3 8 2 6 2 8 -1)
     (1 5 6 2 1 6 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     (1 3 6 1 6 10 3 8 6 5 6 9 8 9 6 -1)
     (10 1 0 10 0 6 9 5 0 5 6 0 -1 -1 -1 -1)
     (0 3 8 5 6 10 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     (10 5 6 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     (11 5 10 7 5 11 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     (11 5 10 11 7 5 8 3 0 -1 -1 -1 -1 -1 -1 -1)
     (5 11 7 5 10 11 1 9 0 -1 -1 -1 -1 -1 -1 -1)
     (10 7 5 10 11 7 9 8 1 8 3 1 -1 -1 -1 -1)
     (11 1 2 11 7 1 7 5 1 -1 -1 -1 -1 -1 -1 -1)
     (0 8 3 1 2 7 1 7 5 7 2 11 -1 -1 -1 -1)
     (9 7 5 9 2 7 9 0 2 2 11 7 -1 -1 -1 -1)
     (7 5 2 7 2 11 5 9 2 3 2 8 9 8 2 -1)
     (2 5 10 2 3 5 3 7 5 -1 -1 -1 -1 -1 -1 -1)
     (8 2 0 8 5 2 8 7 5 10 2 5 -1 -1 -1 -1)
     (9 0 1 5 10 3 5 3 7 3 10 2 -1 -1 -1 -1)
     (9 8 2 9 2 1 8 7 2 10 2 5 7 5 2 -1)
     (1 3 5 3 7 5 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     (0 8 7 0 7 1 1 7 5 -1 -1 -1 -1 -1 -1 -1)
     (9 0 3 9 3 5 5 3 7 -1 -1 -1 -1 -1 -1 -1)
     (9 8 7 5 9 7 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     (5 8 4 5 10 8 10 11 8 -1 -1 -1 -1 -1 -1 -1)
     (5 0 4 5 11 0 5 10 11 11 3 0 -1 -1 -1 -1)
     (0 1 9 8 4 10 8 10 11 10 4 5 -1 -1 -1 -1)
     (10 11 4 10 4 5 11 3 4 9 4 1 3 1 4 -1)
     (2 5 1 2 8 5 2 11 8 4 5 8 -1 -1 -1 -1)
     (0 4 11 0 11 3 4 5 11 2 11 1 5 1 11 -1)
     (0 2 5 0 5 9 2 11 5 4 5 8 11 8 5 -1)
     (9 4 5 2 11 3 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     (2 5 10 3 5 2 3 4 5 3 8 4 -1 -1 -1 -1)
     (5 10 2 5 2 4 4 2 0 -1 -1 -1 -1 -1 -1 -1)
     (3 10 2 3 5 10 3 8 5 4 5 8 0 1 9 -1)
     (5 10 2 5 2 4 1 9 2 9 4 2 -1 -1 -1 -1)
     (8 4 5 8 5 3 3 5 1 -1 -1 -1 -1 -1 -1 -1)
     (0 4 5 1 0 5 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     (8 4 5 8 5 3 9 0 5 0 3 5 -1 -1 -1 -1)
     (9 4 5 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     (4 11 7 4 9 11 9 10 11 -1 -1 -1 -1 -1 -1 -1)
     (0 8 3 4 9 7 9 11 7 9 10 11 -1 -1 -1 -1)
     (1 10 11 1 11 4 1 4 0 7 4 11 -1 -1 -1 -1)
     (3 1 4 3 4 8 1 10 4 7 4 11 10 11 4 -1)
     (4 11 7 9 11 4 9 2 11 9 1 2 -1 -1 -1 -1)
     (9 7 4 9 11 7 9 1 11 2 11 1 0 8 3 -1)
     (11 7 4 11 4 2 2 4 0 -1 -1 -1 -1 -1 -1 -1)
     (11 7 4 11 4 2 8 3 4 3 2 4 -1 -1 -1 -1)
     (2 9 10 2 7 9 2 3 7 7 4 9 -1 -1 -1 -1)
     (9 10 7 9 7 4 10 2 7 8 7 0 2 0 7 -1)
     (3 7 10 3 10 2 7 4 10 1 10 0 4 0 10 -1)
     (1 10 2 8 7 4 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     (4 9 1 4 1 7 7 1 3 -1 -1 -1 -1 -1 -1 -1)
     (4 9 1 4 1 7 0 8 1 8 7 1 -1 -1 -1 -1)
     (4 0 3 7 4 3 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     (4 8 7 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     (9 10 8 10 11 8 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     (3 0 9 3 9 11 11 9 10 -1 -1 -1 -1 -1 -1 -1)
     (0 1 10 0 10 8 8 10 11 -1 -1 -1 -1 -1 -1 -1)
     (3 1 10 11 3 10 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     (1 2 11 1 11 9 9 11 8 -1 -1 -1 -1 -1 -1 -1)
     (3 0 9 3 9 11 1 2 9 2 11 9 -1 -1 -1 -1)
     (0 2 11 8 0 11 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     (3 2 11 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     (2 3 8 2 8 10 10 8 9 -1 -1 -1 -1 -1 -1 -1)
     (9 10 2 0 9 2 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     (2 3 8 2 8 10 0 1 8 1 10 8 -1 -1 -1 -1)
     (1 10 2 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     (1 3 8 9 1 8 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     (0 9 1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     (0 3 8 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     (-1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1))))
