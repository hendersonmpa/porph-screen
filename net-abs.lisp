;;; net-abs.lisp
(in-package :porph-screen)

;; (defparameter *peak* (find-peak *data-set* 400 410))
;; (defparameter *base1* (find-base *data-set* (smoothed-2derivative *data-set* 5) 375 400))
;; (defparameter *base2* (find-base *data-set* (smoothed-2derivative *data-set* 5) 410 430))
;; (defparameter *triangle* (find-triangle *data-set*))

;; (defun parse-line (line)
;;   (let* ((nline (mapcar #'parse-number:parse-number line))
;;          (base1 (make-point :x (first nline) :y (second nline)))
;;          (base2 (make-point :x (third nline) :y (fourth nline)))
;;          (peak (make-point :x (fifth nline) :y (sixth nline)))
;;          (curve (make-curve :base1 base1 :base2 base2 :peak peak)))
;;     curve))

;; (parse-number:parse-number "5.5")
;; (mapcar  #'parse-number:parse-number '("5.5" "6.544" "0.645"))
;; (parse-line '(393.4 0.6467 415.85 0.4299 404.43 0.9408))
;; (point-x (curve-base1 (parse-line '(393.4 0.6467 415.85 0.4299 404.43 0.9408))))

(defmethod slope ((triangle-object triangle))
  "What is the slope of the line between base1 and base2"
  (with-accessors ((b1x base1-x)
                   (b1y base1-y)
                   (b2x base2-x)
                   (b2y base2-y)) triangle-object
    (/ (- b1y b2y)
       (- b1x b2x))))

;; Find the tangent line to the absorbance curve
; y-y1 = m(x-x1)
; y = m(x-x1) + y1
; b = m(-x1) + y1
(defmethod intercept ((triangle-object triangle))
  "The intercept of the line"
  (with-accessors ((b1x base1-x)
                   (b1y base1-y)) triangle-object
    (let ((slope (slope triangle-object)))
      (+ b1y (* slope (- b1x))))))

;;(intercept base1 base2 (slope base1 base2))

;; Given x for the peak interpolate point on the baseline
; result --> point
(defmethod interpolate ((triangle-object triangle))
  (with-accessors ((px peak-x)) triangle-object
    (let ((m (slope triangle-object))
          (b (intercept triangle-object)))
      (+ b (* m px)))))

;;(interpolate peak base1 base2)
(defmethod drop-net-abs ((triangle-object triangle))
  (with-accessors ((py peak-y)) triangle-object
    (let* ((drop-line-y (interpolate triangle-object)))
      (- py drop-line-y))))
