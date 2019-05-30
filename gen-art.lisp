;;;; gen-art.lisp
;;
;;;; Copyright (c) 2018 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>


(in-package #:gen-art)

(defun home-dir (path)
  "Utility function to make relative path names relative to the user's home directory to work around Cairo weirdness."
  (merge-pathnames path (user-homedir-pathname)))

(defun parametric-function (png-file-name
                            &key
                              (width 1200) (height 1200)
                              (steps 8000)
                              (open-png t)
                              (tmin 0.0)
                              (tmax (* 2 pi))
                              (line-width 0.8)
                              (x-scale (/ width 2.0))
                              (y-scale (/ height 2.0))
                              (x-function (lambda (tv) (cos (* 5.0 tv))))
                              (y-function (lambda (tv) (cos (* 7.0 tv)))))
  "Test writing a PNG file with Cairo."
  (let ((real-file-name (home-dir png-file-name))
        (half-width (/ width 2.0))
        (half-height (/ height 2.0))
         (dt (/ (- tmax tmin) steps))
         (xp nil)
         (yp nil))
    (cl-cairo2:with-png-file (real-file-name :argb32 width height)
                             (cl-cairo2:set-source-rgba 0.0 0.0 0.0 1.0)
                             (cl-cairo2:paint)
                             (cl-cairo2:set-line-width line-width)
                             (cl-cairo2:translate half-width half-height)
                             (cl-cairo2:set-source-rgba 0 1 0 1.0)
                             (dotimes (i steps)
                               (let* ((tv (* i dt))
                                      (x (* x-scale (funcall x-function tv)))
                                      (y (* y-scale (funcall y-function tv))))
                                 (if (not (and xp yp))
                                     (cl-cairo2:move-to x y)
                                   (cl-cairo2:line-to x y))
                                 (setf xp x)
                                 (setf yp y)))
                             (cl-cairo2:stroke))
    (when open-png
      (swank:eval-in-emacs (list 'find-file-other-window (namestring real-file-name))))))

(defun deg-to-rad (deg)
  "Convert degrees to radians."
  (declare (type double-float deg))
  (* deg (/ pi 180)))

(defun spiral (png-file-name
               &key
                 (open-png t)
                 (twists 30)
                 (width 1200)
                 (height 1200)
                 (steps (* twists 360)))
  "Draw a spiral with the specified number of twists, saving into the given file name."
  (let ((real-file-name (home-dir png-file-name)))
    (cl-cairo2:with-png-file (real-file-name :argb32 width height)
      (cl-cairo2:set-source-rgba 1.0 1.0 1.0 0.0)
      (cl-cairo2:paint)
      (cl-cairo2:translate (/ width 2.0) (/ height 2.0))
      (cl-cairo2:set-line-width 2)
      (cl-cairo2:scale (/ width 2.0) (/ height 2.0))
      (cl-cairo2:set-source-rgba 0 0 0 1.0)
      (cl-cairo2:move-to 0 0)
      (dotimes (i steps)
        (let* ((ifloat (coerce i 'double-float))
               (xoff (* (cos (deg-to-rad ifloat)) (/ ifloat steps)))
               (yoff (* (sin (deg-to-rad ifloat)) (/ ifloat steps))))
          ;; (cl-cairo2:move-to 0 0)
          (cl-cairo2:rel-line-to xoff yoff)))
      (cl-cairo2:stroke))
    (when open-png
      (swank:eval-in-emacs (list 'find-file-other-window (namestring real-file-name))))))

(defun fractal-tree (png-file-name
                     &key
                       (open-png t)
                       (width 1200) (height 1200)
                       (length 200)
                       (maxdepth 4)
                       (limbs 2)
                       )
  "Draw a fractal tree into the specified file, recursing to maxdepth, with the specified number of limbs at each level."
  (let ((real-file-name (home-dir png-file-name)))
    (cl-cairo2:with-png-file (real-file-name :argb32 width height)

      (cl-cairo2:set-source-rgba 0.0 0.0 0.0 1.0)
      (cl-cairo2:paint)
      
      (cl-cairo2:scale 1 1)

      (labels
          ((to-cart (len angle)
             "Convert a length and angle (polar coordinates) into x,y rectangular coordinates."
             (values (* (cos (deg-to-rad angle)) len) (* (sin (deg-to-rad angle)) len)))
           (draw-tree
               (x y length angle transparency depth )

             (multiple-value-bind
                   (nx ny) (to-cart length angle)

               (cl-cairo2:set-source-rgba 0 1.0 0 transparency)
               (cl-cairo2:set-line-width (/ (1+ depth) 0.5))
               (cl-cairo2:move-to x y)
               (cl-cairo2:line-to (+ x nx) (+ y ny))
               (cl-cairo2:stroke)

               (if (> depth 0)
                   (dotimes (i limbs)

                     (let ((nnx (+ x nx))
                           (nny (+ y ny))
                           (nl (/ length 2.0))
                           (ang1 (+ angle (/ 45 limbs) (* i (/ 180 limbs))))
                           (ang2 (- angle (/ 45 limbs) (* i (/ 180 limbs))))
                           (ntrans (* 1.75 (/ depth maxdepth) transparency))
                           (ndepth (- depth 1)))
                       (draw-tree nnx nny nl ang1 ntrans ndepth)
                       (draw-tree nnx nny nl ang2 ntrans ndepth)))))))

        (draw-tree (/ width 2) height length -90.0 1.0 maxdepth)))
    (when open-png
      (swank:eval-in-emacs (list 'find-file-other-window (namestring real-file-name))))))


(defmacro simple-animation ((variable duration) &body body)
  `(dotimes (,variable (ceiling (1+ (* 60 ,duration))))
     ,@body
     ))

(defun animated-parametric-function (output-directory mp4-file-name
                                     &key
                                       (width 1200) (height 1200)
                                       (steps 8000)
                                       (duration 60)
                                       (fps 30)
                                       (tmin 0.0)
                                       (tmax (* 2 pi))
                                       (line-width 0.8)
                                       (x-scale (/ width 2.0))
                                       (y-scale (/ height 2.0))
                                       (x-function (lambda (tv i) (declare (ignorable tv i)) (* (cos (* (/ i 60.0) tv)) (sin tv))))
                                       (y-function (lambda (tv i) (declare (ignorable tv i)) (* (cos (* (/ i 59.0) tv)) (cos tv)))))
  (ensure-directories-exist output-directory)
  (let ((frames (* duration fps)))
    (dotimes (i frames)
      (let ((current-png-name (format nil "~a/frame~8,'0d.png" output-directory i))
            (half-width (/ width 2.0))
            (half-height (/ height 2.0))
            (dt (/ (- tmax tmin) steps))
            (xp nil)
            (yp nil))
        (cl-cairo2:with-png-file (current-png-name :argb32 width height)
          (cl-cairo2:set-source-rgba 0.0 0.0 0.0 1.0)
          (cl-cairo2:paint)
          (cl-cairo2:set-line-width line-width)
          (cl-cairo2:translate half-width half-height)
          (cl-cairo2:set-source-rgba 0 1 0 1.0)
          (dotimes (j steps)
            (let* ((tv (* j dt))
                   (x (* x-scale (funcall x-function tv i)))
                   (y (* y-scale (funcall y-function tv i))))
              (when (not (and xp yp))
                (cl-cairo2:move-to x y))
              (cl-cairo2:line-to x y)
              (setf xp x)
              (setf yp y)))
          (cl-cairo2:stroke))))
    (anim-utils:make-movie :directory output-directory :file-name mp4-file-name)))
      
(defun compute-next (function values)
  (push (funcall function (car values)) values))

(defun random-complex (real-min real-max imag-min imag-max)
  (complex (+ (- real-max real-min)) real-min)
           (+ (random (- imag-max imag-min)) imag-min))

(defstruct ga-thread
  (color (vec3 (random 1.0) (random 1.0) (random 1.0)))
  (points (list (random-complex -1.0 1.0 -1.0 1.0))))

(defun create-ga-thread (sequence-number)
  (let ((red (ju:halton-sequence (+ sequence-number 0) 3))
        (green (ju:halton-sequence (+ sequence-number 1) 3))
        (blue (ju:halton-sequence (+ sequence-number 2) 3))

        (r1 (ju:halton-sequence (+ sequence-number 3) 3))
        (i1 (ju:halton-sequence (+ sequence-number 4) 3)))
    (make-ga-thread :color (vec3 red green blue)
                    :points (list (complex r1 i1)))))
(defun grow (gat growth)
  (push (+ (car (ga-thread-points gat))
           (random-complex (- growth) growth (- growth) growth))
        (ga-thread-points gat)))

(defun random-threads (output-directory 
                           &key
                             (width 1200) (height 1200)
                             (threads 10)
                             (duration 60)
                             (fps 30)
                             (line-width 0.8)
                             (growth 0.01))
  (ensure-directories-exist output-directory)
  (let* ((frames (* duration fps))
         (threads (loop for i below threads collecting (create-ga-thread (* 5 i))))
         (half-width (/ width 2.0))
         (half-height (/ height 2.0))
         (xs 120.0)
         (ys 120.0))
    (dotimes (frame-number frames)
      (let ((current-png-name (format nil "~a/frame~8,'0d.png" output-directory frame-number)))
        (cl-cairo2:with-png-file (current-png-name :argb32 width height)
          (cl-cairo2:set-source-rgba 0.0 0.0 0.0 1.0)
          (cl-cairo2:paint)
          (cl-cairo2:set-line-width line-width)
          (cl-cairo2:translate half-width half-height)

          (dolist (gat threads)
            (cl-cairo2:set-source-rgba (vx (ga-thread-color gat)) (vy (ga-thread-color gat)) (vz (ga-thread-color gat)) 1.0)
            (cl-cairo2:move-to (* xs (realpart (car (ga-thread-points gat)))) (* ys (imagpart (car (ga-thread-points gat)))))
            (dolist (value (ga-thread-points gat))
              (cl-cairo2:line-to (* xs (realpart value)) (* ys (imagpart value))))
            (cl-cairo2:stroke)
            (grow gat growth)))))))


(defun random-halton-sequence-lines (png-file-name &key (width 1200) (height 1200) (open-png t) (line-count 1000)  (line-width 1.3))
  (ensure-directories-exist png-file-name)
  (let* ((half-width (/ width 2.0))
         (half-height (/ height 2.0))
         (xs (* 1.5 half-width))
         (ys (* 1.5 half-height)))
    (cl-cairo2:with-png-file (png-file-name :argb32 width height)
      (cl-cairo2:set-source-rgba 0.0 0.0 0.0 1.0)
      (cl-cairo2:paint)
      (cl-cairo2:set-line-width line-width)
      (cl-cairo2:translate half-width half-height)
      (dotimes (i line-count)
        (cl-cairo2:set-source-rgba (random 1.0) (random 1.0) (random 1.0) 1.0)
        (let ((x1i (random 400))
              (x2i (random 400))
              (y1i (random 400))
              (y2i (random 400)))
        (cl-cairo2:move-to (ju:halton-between (- xs) xs x1i (+ (random 3) 2)) (ju:halton-between (- ys) ys y1i (+ (random 3) 2)))
        (cl-cairo2:line-to (ju:halton-between (- xs) xs x2i (+ (random 3) 2)) (ju:halton-between (- ys) ys y2i (+ (random 3) 2)))
        (cl-cairo2:stroke))))
  (when open-png
    (swank:eval-in-emacs (list 'find-file-other-window (namestring png-file-name))))))

(defun random-lines (png-file-name &key (width 1200) (height 1200) (open-png t) (line-count 1000) (line-width 1.3))
  (ensure-directories-exist png-file-name)
  (let* ((half-width (/ width 2.0))
         (half-height (/ height 2.0))
         (xs half-width)
         (ys half-height))
    (cl-cairo2:with-png-file (png-file-name :argb32 width height)
      (cl-cairo2:set-source-rgba 0.0 0.0 0.0 1.0)
      (cl-cairo2:paint)
      (cl-cairo2:set-line-width line-width)
      (cl-cairo2:translate half-width half-height)
      (dotimes (i line-count)
        (cl-cairo2:set-source-rgba (random 1.0) (random 1.0) (random 1.0) 1.0)
        (cl-cairo2:move-to (ju:random-between (- xs) xs) (ju:random-between (- ys) ys))
        (cl-cairo2:line-to (ju:random-between (- xs) xs) (ju:random-between (- ys) ys))
        (cl-cairo2:stroke))))
  (when open-png
      (swank:eval-in-emacs (list 'find-file-other-window (namestring png-file-name)))))

;; (defun random-lines (png-file-name &key (width 1200) (height 1200) (open-png t) (line-count 1000) (line-width 1.3))
;;   (ensure-directories-exist png-file-name)
;;   (let* ((half-width (/ width 2.0))
;;          (half-height (/ height 2.0))
;;          (xs half-width)
;;          (ys half-height))
;;     (cl-cairo2:with-png-file (png-file-name :argb32 width height)
;;       (cl-cairo2:set-source-rgba 0.0 0.0 0.0 1.0)
;;       (cl-cairo2:paint)
;;       (cl-cairo2:set-line-width line-width)
;;       (cl-cairo2:translate half-width half-height)
;;       (dotimes (i line-count)
;;         (cl-cairo2:set-source-rgba (random 1.0) (random 1.0) (random 1.0) 1.0)
;;         (cl-cairo2:move-to (ju:random-between (- xs) xs) (ju:random-between (- ys) ys))
;;         (cl-cairo2:line-to (ju:random-between (- xs) xs) (ju:random-between (- ys) ys))
;;         (cl-cairo2:stroke))))
;;   (when open-png
;;       (swank:eval-in-emacs (list 'find-file-other-window (namestring png-file-name)))))

(defun random-cubics (file-name &key (count 1000) (width 1600) (height 1600))
  (ensure-directories-exist file-name)
  (let ((fwidth (coerce width 'double-float))
        (fheight (coerce height 'double-float)))
    (bl:with-objects ((img bl:image-core)
                      (ctx bl:context-core)
                      (path bl:path-core)
                      (codec bl:image-codec-core)
                      (linear bl:linear-gradient-values)
                      (grad bl:gradient-core))

      (bl:image-init-as img width height bl:+format-prgb32+)

      (bl:context-init-as ctx img (cffi:null-pointer))
      (bl:context-set-comp-op ctx bl:+comp-op-src-copy+)
      (bl:context-fill-all ctx)

      (setf (bl:linear-gradient-values.x0 linear) 0.0d0)
      (setf (bl:linear-gradient-values.y0 linear) 0.0d0)
      (setf (bl:linear-gradient-values.x1 linear) 0.0d0)
      (setf (bl:linear-gradient-values.y1 linear) (coerce width 'double-float))


      (dotimes (i count)
        (bl:gradient-init-as grad
                             bl:+gradient-type-linear+
                             linear
                             bl:+extend-mode-pad+ (cffi:null-pointer) 0  (cffi:null-pointer))

        (bl:gradient-add-stop-rgba32 grad 0.0d0 (random #16rffffffff))
        (dotimes (stops (random 12))
          (bl:gradient-add-stop-rgba32 grad (random 1.0) (random #16rffffffff)))
        (bl:gradient-add-stop-rgba32 grad 1.0d0 (random #16rffffffff))



        (bl:path-init path)
        (bl:path-move-to path (random fwidth) (random fheight))
        (bl:path-cubic-to path
                          (random fwidth) (random fheight)
                          (random fwidth) (random fheight)
                          (random fwidth) (random fheight))
        (bl:context-set-comp-op ctx bl:+comp-op-src-over+)
        (bl:context-set-stroke-style ctx grad)
        (bl:context-set-stroke-width ctx 0.75)
        (bl:context-set-stroke-cap ctx bl:+stroke-cap-position-start+ bl:+stroke-cap-round+)
        (bl:context-set-stroke-cap ctx bl:+stroke-cap-position-end+ bl:+stroke-cap-round+)

        #+sbcl (sb-int:with-float-traps-masked (:invalid) (bl:context-stroke-geometry ctx bl:+geometry-type-path+ path))
        #-sbcl (bl:context-stroke-geometry ctx bl:+geometry-type-path+ path)

        
        (bl:path-reset path)
        (bl:gradient-reset grad))

      (bl:context-end ctx)

      (bl:image-codec-init codec)
      (bl:image-codec-by-name codec "BMP")
      (when (uiop/filesystem:file-exists-p file-name)
        (delete-file file-name))

      (bl:image-write-to-file img file-name codec))))

(defun centered-circles (file-name &key (width 1600) (height 1600))
  (bl:with-objects ((img  bl:image-core)
                    (ctx  bl:context-core)
                    (codec  bl:image-codec-core)
                    (circle  bl:circle))

    ;; Initialize and clear image
    (bl:image-init-as img width height bl:+format-prgb32+)
    (bl:context-init-as ctx img (cffi:null-pointer))
    (bl:context-set-comp-op ctx bl:+comp-op-src-copy+)
    (bl:context-fill-all ctx)

    (setup-window ctx -2.0 -2.0 2.0 2.0 width height)

    (bl:context-set-comp-op ctx bl:+comp-op-src-over+)

    (setf (bl:circle.cx circle) -0.5)
    (setf (bl:circle.cy circle) -0.5)
    (setf (bl:circle.r circle) 0.5)
    (bl:context-set-fill-style-rgba32 ctx #16rffff0000)
    (bl:context-fill-geometry ctx bl:+geometry-type-circle+ circle)

    (setf (bl:circle.cx circle) 0.5)
    (setf (bl:circle.cy circle) -0.5)
    (setf (bl:circle.r circle) 0.5)
    (bl:context-set-fill-style-rgba32 ctx #16rff0000ff)
    (bl:context-fill-geometry ctx bl:+geometry-type-circle+ circle)

    (setf (bl:circle.cx circle) -0.5)
    (setf (bl:circle.cy circle) 0.5)
    (setf (bl:circle.r circle) 0.5)
    (bl:context-set-fill-style-rgba32 ctx #16rff00ff00)
    (bl:context-fill-geometry ctx bl:+geometry-type-circle+ circle)

    (setf (bl:circle.cx circle) 0.5)
    (setf (bl:circle.cy circle) 0.5)
    (setf (bl:circle.r circle) 0.5)
    (bl:context-set-fill-style-rgba32 ctx #16rff00ffff)
    (bl:context-fill-geometry ctx bl:+geometry-type-circle+ circle)

    (bl:context-end ctx)
    (bl:image-codec-init codec)
    (bl:image-codec-by-name codec "BMP")
    (when (uiop/filesystem:file-exists-p file-name)
      (delete-file file-name))
    (bl:image-write-to-file img file-name codec)))

(defun gradient-sine-circles (file-name &key (width 1600) (height 1600))
  (bl:with-objects ((img  bl:image-core)
                    (ctx  bl:context-core)
                    (codec  bl:image-codec-core)
                    (circle  bl:circle)
                    (radial-vals bl:radial-gradient-values)
                    (rad-grad bl:gradient-core)
                    )

    ;; Initialize and clear image
    (bl:image-init-as img width height bl:+format-prgb32+)
    (bl:context-init-as ctx img (cffi:null-pointer))
    (bl:context-set-comp-op ctx bl:+comp-op-src-copy+)
    (bl:context-fill-all ctx)

    (setup-window ctx (- pi) (- pi) pi pi width height)

    (setf (bl:radial-gradient-values.x0 radial-vals) 0.0)
    (setf (bl:radial-gradient-values.y0 radial-vals) 0.0)
    (setf (bl:radial-gradient-values.x1 radial-vals) 0.0)
    (setf (bl:radial-gradient-values.y1 radial-vals) 0.0)
    (setf (bl:radial-gradient-values.r0 radial-vals) 2.0)
    (bl:gradient-init-as rad-grad
                         bl:+gradient-type-radial+
                         radial-vals
                         bl:+extend-mode-reflect+ (cffi:null-pointer) 0  (cffi:null-pointer))

    (bl:gradient-add-stop-rgba32 rad-grad 0.0 #16rff00ff00)
    (bl:gradient-add-stop-rgba32 rad-grad 0.5 #16rff0000ff)
    (bl:gradient-add-stop-rgba32 rad-grad 1.0 #16rffff0000)

    (bl:context-set-comp-op ctx bl:+comp-op-src-over+)
    (bl:context-set-fill-style ctx rad-grad)

    (loop 
       for dt = (/ (* 2 pi) 40000)
       for t-min = (- pi)
       for i below 40000
       for tv = t-min then (+ t-min (* dt i))
       do
         (setf (bl:circle.cx circle) tv)
         (setf (bl:circle.cy circle) (* 1.5 (sin (* 70 tv))))
         (setf (bl:circle.r circle) 0.005)
         (bl:context-fill-geometry ctx bl:+geometry-type-circle+ circle))

    (bl:context-end ctx)
    (bl:image-codec-init codec)
    (bl:image-codec-by-name codec "BMP")
    (when (uiop/filesystem:file-exists-p file-name)
      (delete-file file-name))
    (bl:image-write-to-file img file-name codec)))

(defun gradient-function-fill (file-name &key
                                           (width 1600)
                                           (height 1600)
                                           (t-min (* -1 pi))
                                           (t-max (* 1 pi))
                                           (count 200)
                                           (radius 0.02)
                                           (fx (lambda (tv) (cos tv)))
                                           (fy (lambda (tv) (sin tv))))
  (bl:with-objects ((img  bl:image-core)
                    (ctx  bl:context-core)
                    (codec  bl:image-codec-core)
                    (circle  bl:circle)
                    (radial-vals bl:radial-gradient-values)
                    (rad-grad bl:gradient-core)
                    )

    ;; Initialize and clear image
    (bl:image-init-as img width height bl:+format-prgb32+)
    (bl:context-init-as ctx img (cffi:null-pointer))
    (bl:context-set-comp-op ctx bl:+comp-op-src-copy+)
    (bl:context-fill-all ctx)

    (setup-window ctx (- pi) (- pi) pi pi width height)

    (setf (bl:radial-gradient-values.x0 radial-vals) 0.0)
    (setf (bl:radial-gradient-values.y0 radial-vals) 0.0)
    (setf (bl:radial-gradient-values.x1 radial-vals) 0.0)
    (setf (bl:radial-gradient-values.y1 radial-vals) 0.0)
    (setf (bl:radial-gradient-values.r0 radial-vals) 2.0)
    (bl:gradient-init-as rad-grad
                         bl:+gradient-type-radial+
                         radial-vals
                         bl:+extend-mode-reflect+ (cffi:null-pointer) 0  (cffi:null-pointer))

    (bl:gradient-add-stop-rgba32 rad-grad 0.0 (random #16rffffffff))
    (bl:gradient-add-stop-rgba32 rad-grad 0.25 (random #16rffffffff))
    (bl:gradient-add-stop-rgba32 rad-grad 0.5 (random #16rffffffff))
    (bl:gradient-add-stop-rgba32 rad-grad 0.75 (random #16rffffffff))
    (bl:gradient-add-stop-rgba32 rad-grad 1.0 (random #16rffffffff))

    (bl:context-set-comp-op ctx bl:+comp-op-src-over+)
    (bl:context-set-fill-style ctx rad-grad)

    (loop 
       for dt = (/ (- t-max t-min) count)
       for i below count
       for tv = t-min then (+ t-min (* dt i))
       do
         (setf (bl:circle.cx circle) (funcall fx tv))
         (setf (bl:circle.cy circle) (funcall fy tv))
         (setf (bl:circle.r circle) radius)
         (bl:context-fill-geometry ctx bl:+geometry-type-circle+ circle))

    (bl:context-end ctx)
    (bl:image-codec-init codec)
    (bl:image-codec-by-name codec "BMP")
    (when (uiop/filesystem:file-exists-p file-name)
      (delete-file file-name))
    (bl:image-write-to-file img file-name codec)))


(defun bl-fractal-tree (bmp-file-name
                        &key
                          (width 1200) (height 1200)
                          (length 1.0)
                          (maxdepth 4)
                          (limbs 2)
                          )
  "Draw a fractal tree into the specified file, recursing to maxdepth, with the specified number of limbs at each level."
  (let ((real-file-name (home-dir bmp-file-name))
        (fwidth (coerce width 'double-float))
        (fheight (coerce height 'double-float)))
    (ensure-directories-exist real-file-name)
    (bl:with-objects ((img bl:image-core)
                      (ctx bl:context-core)
                      (line bl:line)
                      (codec bl:image-codec-core)
                      (radial-vals bl:radial-gradient-values)
                      (rad-grad bl:gradient-core))

      (bl:image-init-as img width height bl:+format-prgb32+)

      (bl:context-init-as ctx img (cffi:null-pointer))
      (bl:context-set-comp-op ctx bl:+comp-op-src-copy+)
      (bl:context-fill-all ctx)

      (setup-window ctx -1.0 -1.0 1.0 1.0 fwidth fheight)

      (setf (bl:radial-gradient-values.x0 radial-vals) 0.0)
      (setf (bl:radial-gradient-values.y0 radial-vals) 0.0)
      (setf (bl:radial-gradient-values.x1 radial-vals) 0.0)
      (setf (bl:radial-gradient-values.y1 radial-vals) 0.0)
      (setf (bl:radial-gradient-values.r0 radial-vals) 0.5)
      (bl:gradient-init-as rad-grad
                           bl:+gradient-type-radial+
                           radial-vals
                           bl:+extend-mode-reflect+
                           (cffi:null-pointer)
                           0
                           (cffi:null-pointer))

      (bl:gradient-add-stop-rgba32 rad-grad 0.0 #16rff00ff00)
      (bl:gradient-add-stop-rgba32 rad-grad 0.5 #16rff0000ff)
      (bl:gradient-add-stop-rgba32 rad-grad 1.0 #16rffff0000)

      (bl:context-set-comp-op ctx bl:+comp-op-src-over+)
      (bl:context-set-stroke-style ctx rad-grad)
      (bl:context-set-comp-op ctx bl:+comp-op-src-over+)

      (labels
          ((to-cart (len angle)
             "Convert a length and angle (polar coordinates) into x,y rectangular coordinates."
             (values (* (cos (deg-to-rad angle)) len) (* (sin (deg-to-rad angle)) len)))
           (draw-tree (x y length angle depth)
             (multiple-value-bind (nx ny) (to-cart length angle)

               (setf (bl:line.x0 line) x)
               (setf (bl:line.y0 line) y)

               (setf (bl:line.x1 line) (+ x nx))
               (setf (bl:line.y1 line) (+ y ny))
               (bl:context-set-stroke-width ctx  (* 0.01 (+ (/ depth maxdepth) 0.01)))
               (bl:context-stroke-geometry ctx bl:+geometry-type-line+ line)

               (when (> depth 0)
                 (dotimes (i limbs)
                   (let ((nnx (+ x nx))
                         (nny (+ y ny))
                         (nl (/ length 2.0))
                         (ndepth (- depth 1))
                         (ang (+ angle (/ -90 (1- limbs)) (* i (/ 180 (1+ limbs))))))
                     (draw-tree nnx nny nl ang ndepth)))))))
        (draw-tree 0.0 1.0 length -90.0 maxdepth))
      (bl:context-end ctx)
      (bl:image-codec-init codec)
      (bl:image-codec-by-name codec "BMP")
      (when (uiop/filesystem:file-exists-p real-file-name)
        (delete-file real-file-name))
      (bl:image-write-to-file img bmp-file-name codec))))

(defun bl-fractal-tests (bmp-file-name
                        &key
                          (width 1200) (height 1200)
                          (length 1.0)
                          (maxdepth 4)
                          (limbs 2)
                          )
  "Draw a fractal tree into the specified file, recursing to maxdepth, with the specified number of limbs at each level."
  (let ((real-file-name (home-dir bmp-file-name))
        (fwidth (coerce width 'double-float))
        (fheight (coerce height 'double-float)))
    (ensure-directories-exist real-file-name)
    (bl:with-objects ((img bl:image-core)
                      (ctx bl:context-core)
                      (line bl:line)
                      (codec bl:image-codec-core)
                      (radial-vals bl:radial-gradient-values)
                      (rad-grad bl:gradient-core))

      (bl:image-init-as img width height bl:+format-prgb32+)

      (bl:context-init-as ctx img (cffi:null-pointer))
      (bl:context-set-comp-op ctx bl:+comp-op-src-copy+)
      (bl:context-fill-all ctx)

      (setup-window ctx -1.0 -1.0 1.0 1.0 fwidth fheight)

      (setf (bl:radial-gradient-values.x0 radial-vals) 0.0)
      (setf (bl:radial-gradient-values.y0 radial-vals) 0.0)
      (setf (bl:radial-gradient-values.x1 radial-vals) 0.0)
      (setf (bl:radial-gradient-values.y1 radial-vals) 0.0)
      (setf (bl:radial-gradient-values.r0 radial-vals) 0.5)
      (bl:gradient-init-as rad-grad
                           bl:+gradient-type-radial+
                           radial-vals
                           bl:+extend-mode-reflect+
                           (cffi:null-pointer)
                           0
                           (cffi:null-pointer))

      (bl:gradient-add-stop-rgba32 rad-grad 0.0 #16rff00ff00)
      (bl:gradient-add-stop-rgba32 rad-grad 0.5 #16rff004400)
      (bl:gradient-add-stop-rgba32 rad-grad 1.0 #16rff00aa00)

      (bl:context-set-comp-op ctx bl:+comp-op-src-over+)
      (bl:context-set-stroke-style ctx rad-grad)
      (bl:context-set-comp-op ctx bl:+comp-op-src-over+)

      (labels
          ((to-cart (len angle)
             "Convert a length and angle (polar coordinates) into x,y rectangular coordinates."
             (values (* (cos (deg-to-rad angle)) len) (* (sin (deg-to-rad angle)) len)))
           (draw-tree (x y length angle depth)
             (multiple-value-bind (nx ny) (to-cart length angle)

               (setf (bl:line.x0 line) x)
               (setf (bl:line.y0 line) y)

               (setf (bl:line.x1 line) (+ x nx))
               (setf (bl:line.y1 line) (+ y ny))
               (bl:context-set-stroke-width ctx  (* 0.02 (- (/ depth maxdepth) 0.01)))
               (bl:context-stroke-geometry ctx bl:+geometry-type-line+ line)

               (when (> depth 0)
                 (dotimes (i (+ 2 (random limbs)))
                   (let ((nnx (+ x nx))
                         (nny (+ y ny))
                         (nl (/ length 2.0))
                         (ndepth (- depth 1))
                         (ang (+ angle (- 90 (random 180.0)) (* i (/ (random 180.0) (1+ limbs))))))
                     (draw-tree nnx nny nl ang ndepth)))))))
        (draw-tree 0.0 1.0 length -90.0 maxdepth))
      (bl:context-end ctx)
      (bl:image-codec-init codec)
      (bl:image-codec-by-name codec "BMP")
      (when (uiop/filesystem:file-exists-p real-file-name)
        (delete-file real-file-name))
      (bl:image-write-to-file img bmp-file-name codec))))

(defun bl-fractal-wtf (bmp-file-name
                        &key
                          (width 1200) (height 1200)
                          (length 1.0)
                          (maxdepth 4)
                          (limbs 2)
                          )
  "Draw a fractal tree into the specified file, recursing to maxdepth, with the specified number of limbs at each level."
  (let ((real-file-name (home-dir bmp-file-name))
        (fwidth (coerce width 'double-float))
        (fheight (coerce height 'double-float)))
    (ensure-directories-exist real-file-name)
    (bl:with-objects ((img bl:image-core)
                      (ctx bl:context-core)
                      (line bl:line)
                      (codec bl:image-codec-core)
                      (radial-vals bl:radial-gradient-values)
                      (rad-grad bl:gradient-core))

      (bl:image-init-as img width height bl:+format-prgb32+)

      (bl:context-init-as ctx img (cffi:null-pointer))
      (bl:context-set-comp-op ctx bl:+comp-op-src-copy+)
      (bl:context-fill-all ctx)

      (setup-window ctx -1.0 -1.0 1.0 1.0 fwidth fheight)

      (setf (bl:radial-gradient-values.x0 radial-vals) 0.0)
      (setf (bl:radial-gradient-values.y0 radial-vals) 0.0)
      (setf (bl:radial-gradient-values.x1 radial-vals) 0.0)
      (setf (bl:radial-gradient-values.y1 radial-vals) 0.0)
      (setf (bl:radial-gradient-values.r0 radial-vals) 0.5)
      (bl:gradient-init-as rad-grad
                           bl:+gradient-type-radial+
                           radial-vals
                           bl:+extend-mode-reflect+
                           (cffi:null-pointer)
                           0
                           (cffi:null-pointer))

      (bl:gradient-add-stop-rgba32 rad-grad 0.0 #16rff00ff00)
      (bl:gradient-add-stop-rgba32 rad-grad 0.25 #16rff0000bb)
      (bl:gradient-add-stop-rgba32 rad-grad 0.50 #16rff00dd00)
      (bl:gradient-add-stop-rgba32 rad-grad 0.75 #16rffdd4400)
      (bl:gradient-add-stop-rgba32 rad-grad 1.0 #16rff00aa00)

      (bl:context-set-comp-op ctx bl:+comp-op-src-over+)
      (bl:context-set-stroke-style ctx rad-grad)
      (bl:context-set-comp-op ctx bl:+comp-op-src-over+)

      (labels
          ((to-cart (len angle)
             "Convert a length and angle (polar coordinates) into x,y rectangular coordinates."
             (values (* (cos (deg-to-rad angle)) len) (* (sin (deg-to-rad angle)) len)))
           (draw-tree (x y length angle depth)
             (multiple-value-bind (nx ny) (to-cart length angle)

               (setf (bl:line.x0 line) x)
               (setf (bl:line.y0 line) y)

               (setf (bl:line.x1 line) (+ x nx))
               (setf (bl:line.y1 line) (+ y ny))
               (bl:context-set-stroke-width ctx  (* 0.02 (- (/ depth maxdepth) 0.01)))
               (bl:context-stroke-geometry ctx bl:+geometry-type-line+ line)

               (when (> depth 0)
                 (dotimes (i (+ 2 (random limbs)))
                   (let ((nnx (+ x nx))
                         (nny (+ y ny))
                         (nl (/ length 2.0))
                         (ndepth (- depth 1))
                         (ang (+ angle (- 90 (random 180.0)) (* i (/ (random 180.0) (1+ limbs))))))
                     (draw-tree nnx nny nl ang ndepth)))))))
        (draw-tree 0.0 1.0 length -90.0 maxdepth))
      (bl:context-end ctx)
      (bl:image-codec-init codec)
      (bl:image-codec-by-name codec "BMP")
      (when (uiop/filesystem:file-exists-p real-file-name)
        (delete-file real-file-name))
      (bl:image-write-to-file img bmp-file-name codec))))

;; (defstruct strange-attractor 
;;   (a 2.24 :type double-float)
;;   (b 0.43 :type double-float)
;;   (c -0.65 :type double-float)
;;   (d -2.43 :type double-float)
;;   (e 1.0 :type double-float)
;;   (f 1.0 :type double-float)
;;   )

;; (defun random-float (minf maxf)
;;   (+ (random (- maxf minf)) minf))

;; (defun random-sa ()
;;   (make-strange-attractor
;;    :a (random-float -0.5 4.0)
;;    :b (random-float -0.5 4.0)
;;    :c (random-float -0.5 4.0)
;;    :d (random-float -0.5 4.0)
;;    :e (random-float -0.5 4.0)
;;    :f (random-float -0.5 4.0)))

;; (defun draw-strange-attractor (&key img
;;                                  iterations
;;                                  attractor
;;                                  xxmin xxmax yymin yymax)
;;   (let* (
;;          (height (png:image-height img))
;;          (width (png:image-width img))

;;          (xinc (/ width (- xxmax xxmin)))
;;          (yinc (/ height (- yymax yymin)))

;;          (a (strange-attractor-a attractor))
;;          (b (strange-attractor-b attractor))
;;          (c (strange-attractor-c attractor))
;;          (d (strange-attractor-d attractor))
;;          (e (strange-attractor-e attractor))
;;          (f (strange-attractor-f attractor))
;;          (x 0)
;;          (y 0)
;;          (z 0))
;;     (dotimes (i iterations)
;;       (let (
;;             (xx (- (sin (* a y)) (* z (cos (* b x)))))
;;             (yy (- (sin (* c x)) (cos (* d y))))
;;             (zz (* e (sin (* f x))))
;;             )
;;         (setf x xx
;;               y yy
;;               z zz)
;;         (if (and (< xx xxmax) (> xx xxmin)
;;                  (< yy yymax) (> yy yymin))
;;             (let ((xxx (* (- xx xxmin) xinc))
;;                   (yyy (* (- yy yymin) yinc)))
;;               (if (and (< xxx width) (< yyy height))
;;                   (increment-pixel img (truncate xxx) (truncate yyy) 1))))))))

;; (defun strange-attractor (&key file-name
;;                             (xxmin -2.4) (xxmax 2.4)
;;                             (yymin -2.4) (yymax 2.4)
;;                             (width 1600) (height 1600)
;;                             (iterations 5000000)
;;                             (attractor (make-strange-attractor)))
;;   "Draw a strange-attractor fractal into file-name, zoomed into the window specified by xxmin,xxmax and yymin,yymax.  iterations is the number of iterations to run.  a, b, c, d, and e are the parameters of the strange attractor and can be modified for various effects."
;;   (ensure-directories-exist file-name)
;;   (let ((img (png:make-image height width 3 8)))
;;     (draw-strange-attractor :img img
;;                             :attractor attractor
;;                             :iterations iterations
;;                             :xxmin xxmin :xxmax xxmax
;;                             :yymin yymin :yymax yymax)
;;     (with-open-file (output file-name
;;                             :element-type '(unsigned-byte 8)
;;                             :direction :output
;;                             :if-exists :supersede)
;;       (png:encode img output))))

(defun setup-window (ctx x-min y-min x-max y-max width height)
  (let ((x-scale (/ width (- x-max x-min)))
        (y-scale (/ height (- y-max y-min)))
        (x-trans (- x-min))
        (y-trans (- y-min)))
    (cffi:with-foreign-array (arr (make-array 2 :initial-contents (list x-scale y-scale)) '(:array :double 2))
      (bl:context-matrix-op ctx bl:+matrix2d-op-scale+ arr))

    (cffi:with-foreign-array (arr (make-array 2 :initial-contents (list x-trans y-trans)) '(:array :double 2))
      (bl:context-matrix-op ctx bl:+matrix2d-op-translate+ arr))))

(defun random-parametric-cubics (file-name &key
                                             (count 1000)
                                             (width 1600)
                                             (height 1600)
                                             (open-bmp t)
                                             (jump-count 4)
                                             (t-min 0.0)
                                             (t-max (* 2 pi))
                                             (t-win (/ pi 20))
                                             (stroke-width 0.02)
                                             (fx (lambda (tv) (cos tv)))
                                             (fy (lambda (tv) (sin tv))))
  (ensure-directories-exist file-name)
  (bl:with-objects ((img bl:image-core)
                    (ctx bl:context-core)
                    (path bl:path-core)
                    (codec bl:image-codec-core)
                    (linear bl:linear-gradient-values)
                    (grad bl:gradient-core))

    (bl:image-init-as img width height bl:+format-prgb32+)

    (bl:context-init-as ctx img (cffi:null-pointer))
    (bl:context-set-comp-op ctx bl:+comp-op-src-copy+)
    (bl:context-fill-all ctx)

    (setf (bl:linear-gradient-values.x0 linear) 0.0d0)
    (setf (bl:linear-gradient-values.y0 linear) 0.0d0)
    (setf (bl:linear-gradient-values.x1 linear) 0.0d0)
    (setf (bl:linear-gradient-values.y1 linear) (coerce width 'double-float))

    (setup-window ctx -2.0 -2.0 2.0 2.0 width height)

    (dotimes (i count)
      (bl:gradient-init-as grad
                           bl:+gradient-type-linear+
                           linear
                           bl:+extend-mode-pad+ (cffi:null-pointer) 0  (cffi:null-pointer))

      (bl:gradient-add-stop-rgba32 grad 0.0d0 (random #16rffffffff))
      (dotimes (stops (random 12))
        (bl:gradient-add-stop-rgba32 grad (random 1.0) (random #16rffffffff)))
      (bl:gradient-add-stop-rgba32 grad 1.0d0 (random #16rffffffff))

      (bl:path-init path)
      
      (let* ((t-start (ju:random-between t-min t-max)))
        (bl:path-move-to path
                         (funcall fx t-start) (funcall fy t-start))
        (loop
           for i below jump-count
           for t0 = t-start then (+ t2 (random t-win))
           for t1 = (+ t0 (random t-win))
           for t2 = (+ t1 (random t-win))
           do
             (bl:path-cubic-to path
                               (funcall fx t0) (funcall fy t0)
                               (funcall fx t1) (funcall fy t1)
                               (funcall fx t2) (funcall fy t2)))
        (bl:context-set-comp-op ctx bl:+comp-op-src-over+)
        (bl:context-set-stroke-style ctx grad)
        (bl:context-set-stroke-width ctx stroke-width)
        (bl:context-set-stroke-cap ctx bl:+stroke-cap-position-start+ bl:+stroke-cap-round+)
        (bl:context-set-stroke-cap ctx bl:+stroke-cap-position-end+ bl:+stroke-cap-round+)

        #+sbcl (sb-int:with-float-traps-masked (:invalid) (bl:context-stroke-geometry ctx bl:+geometry-type-path+ path))
        #-sbcl (bl:context-stroke-geometry ctx bl:+geometry-type-path+ path)

        
        (bl:path-reset path)
        (bl:gradient-reset grad)))

    (bl:context-end ctx)

    (bl:image-codec-init codec)
    (bl:image-codec-by-name codec "BMP")
    (when (uiop/filesystem:file-exists-p file-name)
      (delete-file file-name))

    (bl:image-write-to-file img file-name codec))
  (when open-bmp
    (swank:eval-in-emacs (list 'find-file-other-window (namestring file-name)))))

(defun interpolate (values idx frame frame-count)
  (let ((perc (* (1- (array-dimension values 1)) (/ frame frame-count 1.0))))
    (multiple-value-bind (int frac) (floor perc)
      (let ((first (aref values idx int))
            (second (aref values idx (1+ int))))
        (+ first (* frac (- second first)))))))

(defun random-integer-array (x-dim y-dim min-param max-param)
  (make-array (list x-dim y-dim)
              :element-type 'fixnum
              :initial-contents (loop for i below x-dim collecting
                                     (loop for i below y-dim
                                        collecting (ju:random-between min-param max-param)))))

(defun parametric-cubic-animation (output-directory &key
                                                      (duration 10.0)
                                                      (param-count 10)
                                                      (max-param 8)
                                                      (min-param 2)
                                                      (fps 30)
                                                      (width 1600)
                                                      (height 1600)
                                                      (cubic-count 40)
                                                      (thread-jumps 40)
                                                      (t-win (/ pi 12))
                                                      (stroke-width 0.00025)
                                                      (coefficients nil)
                                                      )
  (ensure-directories-exist output-directory)
  (let ((frame-count (ceiling (* fps duration)))
        (coeffs (if coefficients
                    coefficients 
                    (random-integer-array 4 param-count min-param max-param))))
    (format t "Generating animation for: ~a~%" coeffs)
    (loop
       for frame below frame-count
       do
         (let ((a (interpolate coeffs 0 frame frame-count))
               (b (interpolate coeffs 1 frame frame-count))
               (c (interpolate coeffs 2 frame frame-count))
               (d (interpolate coeffs 3 frame frame-count))
               (file-name (format nil "~a/cubics-~5,'0d.bmp" output-directory frame)))
           (gen-art:random-parametric-cubics file-name
                                             :width width
                                             :height height
                                             :count cubic-count
                                             :open-bmp nil
                                             :stroke-width stroke-width
                                             :t-win t-win
                                             :jump-count thread-jumps
                                             :fx (lambda (tv) (* 1.5  (sin (* a tv)) (cos (* b tv)) (cos tv)))
                                             :fy (lambda (tv) (* 1.5  (sin (* c tv)) (cos (* d tv)) (sin tv))))))
    coeffs))
