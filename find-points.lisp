;;;; find-points.lisp
(in-package #:porph-screen)
;; Data Structures
(defstruct spectra "The series of absorbance and wavelength data" nm abs)
(defstruct point "Cartesian co-ordinates" x y )
(defstruct triangle "The three points in an absorbance curve" base1 peak base2)


(defun clean-up (&optional (file #P"2014-08-14.csv"))
  (let ((str (make-array 0
                         :element-type 'character
                         :adjustable t
                         :fill-pointer 0)))
    (with-open-file (in file :direction :input)
      (loop for line = (read-line in nil 'foo)
         until (eq line 'foo)
         do (let ((new-line (string-trim '(#\, #\Space #\Tab #\Newline #\Return) line)))
              (format str "~A~%" new-line))))
    (make-string-input-stream str)))

(defun read-data (&optional (data (clean-up)))
  "Read in the csv and parse the numbers"
  (let* ((data-set (cl-csv:read-csv file-path
                                    :map-fn
                                    #'(lambda (line) (mapcar #'parse-number line))))
         ())))

(defparameter *raw-data* (clean-up))

(defun get-sample-names (raw-data)
  (let ((id-line (car raw-data))
        (ids nil))
    (dolist (entry id-line (reverse ids))
      (cond ((not (equalp entry ""))(push entry ids))
            (t nil)))))

(defun make-spectra-list (raw-data)
  (let* ((numerical-data (cddr )))))

(defun rotate (list-of-lists)
"Matrix transpose a list of lists so that column-major data becomes row major."
  (apply #'mapcar #'list list-of-lists))

(defparameter *rotated-data* (rotate(read-data)))

(defparameter *data-set* (make-spectra :nm (first *rotated-data*)
                                       :abs (second *rotated-data*)))

;; (defparameter *aloa* (second (rotate(read-data))) "a list of absorbance readings")
;; (defparameter *alon* (first (rotate(read-data))) "a list of wavelengths")

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
  "apply a rolling mean and return a struct
struct-abs: smooth
struct-nm: trimmed on each end to mimic a middle position in the window "
  (labels ((rolling-mean (aloa n)
             (cond ((< (length aloa) n) nil)
                   (t (cons (mean (subseq aloa 0 n))
                            (rolling-mean (cdr aloa) n))))))
    (let* ((alon (spectra-nm spectra-struct))
           (aloa (spectra-abs spectra-struct))
           (len (length alon))
           (smooth-aloa (rolling-mean aloa n))
           ;(mid-point (truncate n 2))
           ;(trimmed-alon (subseq alon mid-point
           (trimmed-alon (subseq alon 0 (- len n))))
      (make-spectra :nm trimmed-alon :abs smooth-aloa))))

(defun smoothed-2derivative (specta-struct n)
"Apply a running window of length n mean to the second derivative"
  (let ((derivative (second-derivative specta-struct)))
    (smooth-struct derivative n)))

;; Inflection points are where the function changes concavity. Since
;; concave up corresponds to a positive second derivative and concave
;; down corresponds to a negative second derivative, then when the
;; function changes from concave up to concave down (or vise versa)
;; the second derivative must equal zero at that point.

;; Divide the graph into regions
;; Base1: 370 - 400 smoothed second derivative closest to zero i.e min abs value
;; Peak:  400 - 410 maximum abs in range
;; Base2: 410 - 450 smoothed second derivative closest to zero i.e min abs value
;; Find the wavelength with the maximum absorbance
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

(defun find-base (raw-struct derivative-struct lower-limit upper-limit)
  (let* ((alon (spectra-nm derivative-struct))
         (aloa (spectra-abs derivative-struct))
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
    (find-nearest-point base-nm raw-struct)))

(defun find-triangle (spectra-struct &optional
                                       (first 375) (second 400) (third 420) (fourth 430)
                                       (window 5))
"Document here please"
  (let* ((peak (find-peak spectra-struct second third))
        (smoothed (smoothed-2derivative spectra-struct window))
        (base1 (find-base spectra-struct smoothed first second))
        (base2 (find-base spectra-struct smoothed third fourth)))
    (make-triangle :base1 base1 :peak peak :base2 base2)))

;(find-triangle *data-set*)
