(defpackage #:opticl-ocr
  (:use #:cl
        #:opticl))

(in-package :opticl-ocr)

(defun is-8bit-gray-dark (c) (< c 50))


(defun flood-fill (image y x color match-fn &key (tolerance 1))
  (declare (type fixnum y x))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (with-image-bounds (height width)
                     image
    (flet ((fill-p (i j) (and (< -1 i height)
                              (< -1 j width)
                              (multiple-value-call match-fn (pixel image i j)))))
      (flet ((fill-line (i j step-i step-j)
               (let ((tolerance-threshold tolerance)
                     (distance 0))
                 (declare (type fixnum distance tolerance-threshold))
                 (loop for y fixnum = i then (+ step-i y) while (< -1 y height)
                       for x fixnum = j then (+ step-j x) while (< -1 x width)
                       do (progn
                            (cond ((fill-p y x)
                                   (setf (pixel image y x) (values-list color)
                                         tolerance-threshold tolerance
                                         distance (1+ distance)))
                                  ((> tolerance-threshold 0)
                                   (setf (pixel image y x) (values-list color)
                                         tolerance-threshold (1- tolerance-threshold)
                                         distance (1+ distance)))
                                  (t (return distance)))))
                 distance))
             (fill-around-the-line (i j distance step-i step-j)
               (let ((iter-y (= 0 step-j)))
                 (loop for d fixnum from 0 below distance
                       for y fixnum = i then (+ step-i y)
                       for x fixnum = j then (+ step-j x)
                       do (if iter-y
                              (progn
                                (flood-fill image y (1+ x) color match-fn :tolerance tolerance)
                                (flood-fill image y (1- x) color match-fn :tolerance tolerance))
                              (progn
                                (flood-fill image (1+ y) x color match-fn :tolerance tolerance)
                                (flood-fill image (1- y) x color match-fn :tolerance tolerance)))))))
        (when (fill-p y x)
          (progn
            (setf (pixel image y x) (values-list color))
            (let ((d-y (fill-line y x -1 0))
                  (d-x (fill-line y x 0 -1))
                  (d+y (fill-line y x 1 0))
                  (d+x (fill-line y x 0 1)))
              (fill-around-the-line y x d-y -1  0)
              (fill-around-the-line y x d-x  0 -1)
              (fill-around-the-line y x d+y  1  0)
              (fill-around-the-line y x d+x  0  1))))))))



(defmacro do-sliding-windows ((y1-var x1-var y2-var x2-var) (band-y band-x step-y step-x) image &body body)
  (declare (ignorable image))
  (alexandria:with-gensyms (image-height image-width)
    `(with-image-bounds (,image-height ,image-width)
                        ,image
       (loop for ,y1-var fixnum from 0 below ,image-height by ,step-y
             for ,y2-var fixnum = (min ,image-height (+ ,y1-var ,band-y))
             do (loop for ,x1-var fixnum from 0 below ,image-width by ,step-x
                      for ,x2-var fixnum = (min ,image-width (+ ,x1-var ,band-x))
                      when (and (> ,x2-var ,x1-var) (> ,y2-var ,y1-var))
                        do ,@body)))))

(defun remove-black-areas (image
                           white-color
                           &key
                             (lightness-fn #'max)
                             (grayscale-fn #'opticl::mean)
                             (size 20)
                             (depth 500)
                             (step 5)
                             (tolerance 20)
                             (fill-threshold 0.33)
                             (scan-threshold 0.95))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (declare (type fixnum size depth step tolerance) (type function lightness-fn grayscale-fn))
  (let ((white-grayscale (apply grayscale-fn white-color))
        (white-lightness (apply lightness-fn white-color)))
    (flet ((too-dark-p (&rest components)
             (>= fill-threshold (/ (apply grayscale-fn components) white-grayscale))))
      (flet ((process-region (y1 x1 y2 x2)
               (let ((lightness 0)
                     (total 0))
                 (do-region-pixels (y x y1 x1 y2 x2) image
                   (setf lightness (+ lightness (multiple-value-call lightness-fn (pixel image y x))))
                   (incf total))
                 (when (>= (/ (- white-lightness (/ lightness total)) white-lightness) scan-threshold)
                   (do-region-pixels (y x y1 x1 y2 x2)
                                     image
                     (flood-fill image y x white-color #'too-dark-p :tolerance tolerance))))))
        (progn
          (do-sliding-windows
              (y1 x1 y2 x2)
              (depth size step size)
              image
            (process-region y1 x1 y2 x2))
          (do-sliding-windows
              (y1 x1 y2 x2)
              (size depth size step)
              image
            (process-region y1 x1 y2 x2)))))))
