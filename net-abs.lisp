;;;; letter-writer.lisp
(in-package #:porph-screen)

(defstruct point "Cartesian co-ordinates" x y )
;; (setf peak (make-point :x 406 :y 0.5793))
;; (setf base1 (make-point :x 390 :y 0.5639))
;; (setf base2 (make-point :x 425 :y 0.4449))

(defstruct curve "The three points in an absorbance curve" base1 base2 peak)
;; (setf curve1 (make-curve :base1 base1 :base2 base2 :peak peak))

(defun parse-line (line)
  (let* ((nline (mapcar #'parse-number:parse-number line))
         (base1 (make-point :x (first nline) :y (second nline)))
         (base2 (make-point :x (third nline) :y (fourth nline)))
         (peak (make-point :x (fifth nline) :y (sixth nline)))
         (curve (make-curve :base1 base1 :base2 base2 :peak peak)))
    curve))

;; (parse-number:parse-number "5.5")
;; (mapcar  #'parse-number:parse-number '("5.5" "6.544" "0.645"))
;; (parse-line '(393.4 0.6467 415.85 0.4299 404.43 0.9408))
;; (point-x (curve-base1 (parse-line '(393.4 0.6467 415.85 0.4299 404.43 0.9408))))

(defun delta (a-func point1 point2)
  "Difference between the x or y values of each point"
  (- (funcall a-func point2)
     (funcall a-func point1)))

(defun slope (point1 point2)
"What is the slope of the line between p1 and p2"
  (/ (delta #'point-y point1 point2)
     (delta #'point-x point1 point2)))

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

(defun net-abs (curve)
  (let* ((peak (curve-peak curve))
         (base1 (curve-base1 curve))
         (base2 (curve-base2 curve))
         (baseline (interpolate peak base1 base2)))
    (delta #'point-y baseline peak)))

;; (let ((input (cl-csv:read-csv #P"~/Desktop/fecal_screen.csv")))
;;   (loop for i in input collect (nth 10 i)))

;; (let ((accum nil))
;;   (cl-csv:do-csv (row #P"~/Desktop/fecal_screen.csv")
;;     (push (nth 10 row) accum))
;;   accum)

;;(net-abs curve1)

;; (cl-csv:read-csv "393.4,0.6467,415.85,0.4299,404.43,0.9408")
;; (cl-csv:do-csv (row #P"~/Desktop/Porphyrin Screen data.csv")
;;   (format t "~D~%" (parse-line row)))
;; (cl-csv:read-csv #P"~/Desktop/Porphyrin Screen data.csv"
;;                  :map-fn #'parse-line)

(defun csv-map (a-func path-to-file)
  (with-open-file (out #P"/Users/matthew/lisp/site/porph-screen/dat/Net_Abs.csv"
                       :direction :output
                       :if-exists :supersede)
    (let* ((curve-list (cl-csv:read-csv path-to-file
                                        :map-fn #'parse-line))
           (abs (mapcar a-func curve-list)))
      (format out "~{~D~%~}" abs))))

;;(csv-map #'net-abs #P"~/Desktop/Porphyrin Screen data.csv")

;; (net-abs-map #P"~/Desktop/Porphyrin Screen data.csv")

;; (defun net-abs-do (file)
;;   (with-open-file (str file :direction :input)
;;     (do ((line (read-line str nil 'eof)
;;                (read-line str nil 'eof)))
;;         ((eql line 'eof))
;;       (let ((abs (net-abs ))) format t "~A~%" line))))

(defun pseudo-cat (file)
  (with-open-file (str file :direction :input)
    (do ((line (read-line str nil 'eof)
               (read-line str nil 'eof)))
        ((eql line 'eof))
      (format t "~A~%" line))))

;; (pseudo-cat "~/Desktop/Porphyrin Screen data.csv")
