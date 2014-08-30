;;;; find-points.lisp
(in-package #:porph-screen)

(defun read-data (&optional (file-path #P"/Users/matthew/lisp/site/porph-screen/data/test-data.csv"))
"Read in the csv and parse the numbers"
  (cl-csv:read-csv file-path
                   :map-fn #'(lambda (line) (mapcar #'parse-number line))))

(defun rotate (list-of-lists)
"Matrix transpose a list of lists so that column-major data becomes row major."
  (apply #'mapcar #'list list-of-lists))

(defparameter *rotated-data* (rotate(read-data)))

(defstruct spectra "The series of absorbance and wavelength data" nm abs)
(defparameter *data-set* (make-spectra :nm (first *rotated-data*)
                                       :abs (second *rotated-data*)))

(defparameter *aloa* (second (rotate(read-data))) "a list of absorbance readings")
(defparameter *alon* (first (rotate(read-data))) "a list of wavelengths")

(defun two-by-two (a-func a-list)
           (loop for (a b) on a-list by #'cddr collect (funcall a-func a b)))

(defun mid-point (alon)
  "Find the mid-points between wavelengths to correspond to the derivative points"
  (labels ((middle (a b)
             (/ (+ a b) 2)))
    (let* ((len (length alon)))
      (cond ((evenp len) (two-by-two #'middle alon))
            (t (two-by-two #'middle (cdr alon)))))))

(defun derivative (specta-struct)
  "Find the first derivative of successive specta-struct points i.e rate of change"
  (let* ((alon (spectra-nm specta-struct))
         (aloa (spectra-abs specta-struct))
         (len (length aloa))
         (rise-list (cond ((evenp len) (two-by-two #'- aloa))
                     (t (two-by-two #'- (cdr aloa)))))
         (run-list (cond ((evenp len) (two-by-two #'- alon))
                    (t (two-by-two #'- (cdr alon)))))
         (derivative (mapcar (lambda (rise run) (/ rise run)) rise-list run-list))
         (middle (mid-point alon)))
    (make-spectra :nm middle :abs derivative)))

(defun second-derivative (specta-struct)
  "y2=diff(y1)./diff(x)"
  (let ((first-derivative (derivative specta-struct)))
    (derivative first-derivative)))

(defun mean (a-list)
  (let ((n (length a-list))
        (sum (apply #'+ a-list)))
    (/ sum n)))

(defun smooth-struct (spectra-struct n)
  (let ((alon (spectra-nm spectra-struct))
        (aloa (spectra-abs spectra-struct))
        (labels ((rolling-mean )) )
        (mean-aloa (cond ((< (length aloa) n) nil)
                         (t (cons (mean (subseq a-list 0 n))
                                  (window-mean (cdr a-list) n))))))

    ))

;;(window-mean '(1 2 3 4 5 6 7 8 9) 3)

(defun smoothed-2derivative (specta-struct n)
"Apply a running window of length n mean to the second derivative"
  (let* ((derivative (second-derivative specta-struct))
         (dx (spectra-nm derivative))
         (dy (spectra-abs derivative))
         (smoothed (rolling-mean dy n))
         (trimmed-x (subseq dx 0 (- (length dx) n))))
    (make-spectra :nm trimmed-x :abs smoothed)))

;; Inflection points are where the function changes concavity. Since
;; concave up corresponds to a positive second derivative and concave
;; down corresponds to a negative second derivative, then when the
;; function changes from concave up to concave down (or vise versa)
;; the second derivative must equal zero at that point.

;; (with-open-file (out #p "~/Desktop/derivative.txt" :direction :output)
;;   (mapcar #'(lambda (n a) (format out "~D   ~D~%" n a))
;;           (mid-point *alon*) (derivative *aloa*)))

;; (with-open-file (out #p "~/Desktop/2derivative.txt" :direction :output)
;;   (mapcar #'(lambda (n a) (format out "~D   ~D~%" n a))
;;           (mid-point (mid-point *alon*)) (derivative (derivative *aloa*))))

(plot-data "test" (derivative *data-set*))

(plot-data "test2" (second-derivative *data-set*))

(plot-data "test3" (smoothed-2derivative *data-set* 5))

(defun gp-format-data (filename specta-struct)
  (with-open-file (out filename :direction :output)
    (apply #'mapcar #'(lambda (n a) (format out "~D   ~D~%" n a)) specta-struct)))

;; Divide the graph into regions
;; Base1: 370 - 400 smoothed second derivative closest to zero i.e min abs value
;; Peak:  400 - 410 maximum abs in range
;; Base2: 410 - 450 smoothed second derivative closest to zero i.e min abs value

;;(apply #'max *aloa*)

;; Find the wavelength with the maximum absorbance
(nth (position (apply #'max *aloa*) *aloa*) *alon*)

;; The max absorbance should be between 400 and 410nm
(defun make-coordinate-pairs (rotated-data)
  (pairlis (first rotated-data) (second rotated-data)))

(defun find-peak (spectra-struct lower-limit upper-limit)
  (let* ((alon (spectra-nm spectra-struct))
         (aloa (spectra-abs spectra-struct))
         (nm-window (mapcan
                     #'(lambda (x)
                         (and (> x lower-limit)
                              (< x upper-limit)
                              (list (position x alon)))) alon))
         (abs-values (subseq aloa
                             (first nm-window)
                             (car (last nm-window))))
         (max-abs (apply #'max abs-values))
         (max-nm (nth (position max-abs aloa) alon)))
    (make-point :x max-nm :y max-abs)))

(find-peak *data-set* 400 410)

(defun find-nearest-point (nm spectra-struct)
  (let* ((alon (spectra-nm spectra-struct))
         (aloa (spectra-abs spectra-struct))
         (abs-diffs (mapcar
                #'(lambda (x)
                    (abs (- x nm))) alon))
         ;(abs-diffs (mapcar #'abs diffs))
         (min-diff (apply #'min abs-diffs))
        (min-position (position min-diff abs-diffs))
        (nm (nth min-position alon))
        (abs (nth min-position aloa)))
    (make-point :x nm :y abs)))

(defun find-base (spectra-struct lower-limit upper-limit)
  (let* ((alon (spectra-nm spectra-struct))
         (aloa (spectra-abs spectra-struct))
         (nm-window (mapcan
                     #'(lambda (x)
                         (and (> x lower-limit)
                              (< x upper-limit)
                              (list (position x alon)))) alon))
         (abs-values (mapcar #'abs (subseq aloa
                                           (first nm-window)
                                           (car (last nm-window)))))
         (min-val (apply #'min abs-values))
         (near-0-position (+ (position min-val abs-values)
                                (first nm-window)))
         (base-nm (nth near-0-position alon)))
    (find-nearest-point base-nm spectra-struct)))

(find-base (smoothed-2derivative *data-set* 5) 385 400)
