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

(defun normalize-vec3 (v)
  (let ((x (vec3-x v))
        (y (vec3-y v))
        (z (vec3-z v)))
    (let ((r (sqrt (+ (* x x) (* y y) (* z z)))))
      (if (/= r 0)
          (make-vec3 (/ x r) (/ y r) (/ z r))
          (error "zero vector")))))


;; triangle

(defstruct (triangle (:constructor make-triangle (vertex-0 vertex-1 vertex-2)))
  (vertex-0 nil :read-only t)
  (vertex-1 nil :read-only t)
  (vertex-2 nil :read-only t))

(defmacro triangle-vertex (tri i)
  (case i
    (0 `(triangle-vertex-0 ,tri))
    (1 `(triangle-vertex-1 ,tri))
    (2 `(triangle-vertex-2 ,tri))))

(defun triangle= (a b)
  (and (vec3= (triangle-vertex-0 a) (triangle-vertex-0 b))
       (vec3= (triangle-vertex-1 a) (triangle-vertex-1 b))
       (vec3= (triangle-vertex-2 a) (triangle-vertex-2 b))))

(defun degenerate-triangle (tri)
  (or (vec3= (triangle-vertex-0 tri) (triangle-vertex-1 tri))
      (vec3= (triangle-vertex-1 tri) (triangle-vertex-2 tri))
      (vec3= (triangle-vertex-2 tri) (triangle-vertex-0 tri))))


;; smooth triangle

(defstruct (smooth-triangle
             (:constructor make-smooth-triangle (vertex-0 normal-0
                                                 vertex-1 normal-1
                                                 vertex-2 normal-2)))
  (vertex-0 nil :read-only t)
  (normal-0 nil :read-only t)
  (vertex-1 nil :read-only t)
  (normal-1 nil :read-only t)
  (vertex-2 nil :read-only t)
  (normal-2 nil :read-only t))

(defmacro smooth-triangle-vertex (tri i)
  (case i
    (0 `(smooth-triangle-vertex-0 ,tri))
    (1 `(smooth-triangle-vertex-1 ,tri))
    (2 `(smooth-triangle-vertex-2 ,tri))))

(defmacro smooth-triangle-normal (tri i)
  (case i
    (0 `(smooth-triangle-normal-0 ,tri))
    (1 `(smooth-triangle-normal-1 ,tri))
    (2 `(smooth-triangle-normal-2 ,tri))))

(defun smooth-triangle= (a b)
  (and (vec3= (smooth-triangle-vertex-0 a) (smooth-triangle-vertex-0 b))
       (vec3= (smooth-triangle-normal-0 a) (smooth-triangle-normal-0 b))
       (vec3= (smooth-triangle-vertex-1 a) (smooth-triangle-vertex-1 b))
       (vec3= (smooth-triangle-normal-1 a) (smooth-triangle-normal-1 b))
       (vec3= (smooth-triangle-vertex-2 a) (smooth-triangle-vertex-2 b))
       (vec3= (smooth-triangle-normal-2 a) (smooth-triangle-normal-2 b))))

(defun to-smooth-triangle (tri fn)
  (let ((v0 (triangle-vertex tri 0))
        (v1 (triangle-vertex tri 1))
        (v2 (triangle-vertex tri 2)))
    (make-smooth-triangle v0 (funcall fn (vec3-x v0) (vec3-y v0) (vec3-z v0))
                          v1 (funcall fn (vec3-x v1) (vec3-y v1) (vec3-z v1))
                          v2 (funcall fn (vec3-x v2) (vec3-y v2) (vec3-z v2)))))


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
    (remove-degenerate-triangles
     (reduce-grid grid (lambda (cell)
                         (polygonise cell isolevel))))))

(defun marching-cubes-smooth (fn-value fn-normal min max delta isolevel)
  (mapcar (lambda (tri)
            (to-smooth-triangle tri fn-normal))
          (marching-cubes fn-value min max delta isolevel)))

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
    (when (= (aref +edge-table+ cube-index) 0)
      (return-from polygonise nil))
    
    ; Find the vertices where the surface intersects the cube
    (when (logand (aref +edge-table+ cube-index) 1)
      (setf (aref vert-array 0)
            (vertex-interop isolevel
                            (grid-cell-vertex grid 0) (grid-cell-vertex grid 1)
                            (grid-cell-value grid 0) (grid-cell-value grid 1))))
    (when (logand (aref +edge-table+ cube-index) 2)
      (setf (aref vert-array 1)
            (vertex-interop isolevel
                            (grid-cell-vertex grid 1) (grid-cell-vertex grid 2)
                            (grid-cell-value grid 1) (grid-cell-value grid 2))))
    (when (logand (aref +edge-table+ cube-index) 4)
      (setf (aref vert-array 2)
            (vertex-interop isolevel
                            (grid-cell-vertex grid 2) (grid-cell-vertex grid 3)
                            (grid-cell-value grid 2) (grid-cell-value grid 3))))
    (when (logand (aref +edge-table+ cube-index) 8)
      (setf (aref vert-array 3)
            (vertex-interop isolevel
                            (grid-cell-vertex grid 3) (grid-cell-vertex grid 0)
                            (grid-cell-value grid 3) (grid-cell-value grid 0))))
    (when (logand (aref +edge-table+ cube-index) 16)
      (setf (aref vert-array 4)
            (vertex-interop isolevel
                            (grid-cell-vertex grid 4) (grid-cell-vertex grid 5)
                            (grid-cell-value grid 4) (grid-cell-value grid 5))))
    (when (logand (aref +edge-table+ cube-index) 32)
      (setf (aref vert-array 5)
            (vertex-interop isolevel
                            (grid-cell-vertex grid 5) (grid-cell-vertex grid 6)
                            (grid-cell-value grid 5) (grid-cell-value grid 6))))
    (when (logand (aref +edge-table+ cube-index) 64)
      (setf (aref vert-array 6)
            (vertex-interop isolevel
                            (grid-cell-vertex grid 6) (grid-cell-vertex grid 7)
                            (grid-cell-value grid 6) (grid-cell-value grid 7))))
    (when (logand (aref +edge-table+ cube-index) 128)
      (setf (aref vert-array 7)
            (vertex-interop isolevel
                            (grid-cell-vertex grid 7) (grid-cell-vertex grid 4)
                            (grid-cell-value grid 7) (grid-cell-value grid 4))))
    (when (logand (aref +edge-table+ cube-index) 256)
      (setf (aref vert-array 8)
            (vertex-interop isolevel
                            (grid-cell-vertex grid 0) (grid-cell-vertex grid 4)
                            (grid-cell-value grid 0) (grid-cell-value grid 4))))
    (when (logand (aref +edge-table+ cube-index) 512)
      (setf (aref vert-array 9)
            (vertex-interop isolevel
                            (grid-cell-vertex grid 1) (grid-cell-vertex grid 5)
                            (grid-cell-value grid 1) (grid-cell-value grid 5))))
    (when (logand (aref +edge-table+ cube-index) 1024)
      (setf (aref vert-array 10)
            (vertex-interop isolevel
                            (grid-cell-vertex grid 2) (grid-cell-vertex grid 6)
                            (grid-cell-value grid 2) (grid-cell-value grid 6))))
    (when (logand (aref +edge-table+ cube-index) 2048)
      (setf (aref vert-array 11)
            (vertex-interop isolevel
                            (grid-cell-vertex grid 3) (grid-cell-vertex grid 7)
                            (grid-cell-value grid 3) (grid-cell-value grid 7))))
    
    ; Create the triangle
    (loop for i from 0 by 3
       while (/= (aref +tri-table+ cube-index i) -1)
       collect (make-triangle
                (aref vert-array (aref +tri-table+ cube-index i))
                (aref vert-array (aref +tri-table+ cube-index (+ i 1)))
                (aref vert-array (aref +tri-table+ cube-index (+ i 2)))))))

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

(defun remove-degenerate-triangles (tris)
  (remove-if #'degenerate-triangle tris))
