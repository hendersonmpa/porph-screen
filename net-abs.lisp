;;;; letter-writer.lisp
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

(defun delta (a-func base1 base2)
  "Difference between the x or y values of each point"
  (- (funcall a-func base2)
     (funcall a-func base1)))

(defun slope (base1 base2)
"What is the slope of the line between p1 and p2"
  (/ (delta #'point-y base1 base2)
     (delta #'point-x base1 base2)))

;;(slope base1 base2)

;; Find the tangent line to the absorbance curve
; y-y1 = m(x-x1)
; y = m(x-x1) + y1
; b = m(-x1) + y1
(defun intercept (base1 base2 slope)
"The intercept of the line"
  (+ (point-y base1)
   (* slope (- (point-x base1)))))

;;(intercept base1 base2 (slope base1 base2))

;; Given x for the peak interpolate point on the baseline
; result --> point
(defun interpolate (peak base1 base2)
  (let* ((m (slope base1 base2))
         (b (intercept base1 base2 m))
         (x (point-x peak))
         (y (+ b (* m x))))
    (make-point :x x :y y)))

;;(interpolate peak base1 base2)

(defun net-abs (spectra-struct)
  (let* ((triangle (spectra-triangle spectra-struct))
         (peak (triangle-peak triangle))
         (base1 (triangle-base1 triangle))
         (base2 (triangle-base2 triangle))
         (baseline (interpolate peak base1 base2)))
    (delta #'point-y baseline peak)))

(defparameter *urine-constant* 1.1097)
(defparameter *fecal-constant* 14.85)

;; Fecal: Net-Abs * 14.85/Weight of sample = “X” nmol/g wet feces
;; Urine: Net-Abs * 1.1097 * dil * vol = nmol/d
(defun concentration (spectra-struct &optional (urine-constant *urine-constant*)
                                       (fecal-constant *fecal-constant*))
  (let* ((matrix (spectra-matrix spectra-struct))
         (vol (spectra-vol spectra-struct))
         (dil (spectra-dil spectra-struct))
         (net-abs (spectra-net-abs spectra-struct)))
    (cond ((string= "urine" matrix)
           (* net-abs urine-constant dil vol))
          ((string= "fecal" matrix)
           (/ (* net-abs fecal-constant) (/ vol 1000)))
          (t (print "No matrix provided")))))

;; Normal if concentration <110 nmol/d for 24 h collections
;; or <110 nmol/L for random specimens.
;; Borderline if concentration is between 110 and 200 nmol/d or nmol/L.
;; Append the footnote POR 3 which expands to “Quantitation to follow”.  Test request a
;; quantitative urine porphyrin.
;; Elevated if concentration is >200 nmol/d or nmol/L.  Append the footnote POR 3.
;; Test request a quantitative urine porphyrin.

(defun results (spectra-struct)
  (let ((matrix (spectra-matrix spectra-struct))
        (conc (concentration spectra-struct)))
    (cond ((string= "urine" matrix)
           (cond ((and (>= conc -10) (< conc 110))(list conc "Normal"))
                 ((and (>= conc 110) (<= conc 200))(list conc "Borderline"))
                 ((> conc 200)(list conc "Elevated"))
                 (t (list conc "Interference?"))))
          ((string= "fecal" matrix)
           (cond ((and (>= conc -10) (<= conc 35))(list conc "Normal"))
                 ((> conc 35) (list conc "Elevated"))
                 (t (list conc "Interference?")))))))
