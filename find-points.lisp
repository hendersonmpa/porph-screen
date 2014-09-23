;;;; find-points.lisp
(in-package :porph-screen)

(defun two-by-two (a-func a-list)
  (loop for (a b) on a-list by #'cddr collect (funcall a-func b a)))

;; (defun mid-point (alon)
;;   "Find the mid-points between wavelengths to correspond to the derivative points"
;;   (labels ((middle (a b)
;;              (/ (+ a b) 2)))
;;     (let* ((len (length alon)))
;;       (cond ((evenp len) (two-by-two #'middle alon))
;;             (t (two-by-two #'middle (cdr alon)))))))

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
         (min-nm (cond ((evenp len) (two-by-two #'min alon))
                       (t (two-by-two #'min (cdr alon))))))
    (make-spectra :nm min-nm :abs derivative)))

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
           (mid-point (truncate n 2))
                                        ;(trimmed-alon (subseq alon n))
           (trimmed-alon (subseq alon mid-point (- len mid-point))))
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

(defun find-nearest-point (nm spectra-struct)
"Returns the point in spectra-struct nearest to nm"
  (let* ((alon (spectra-nm spectra-struct))
         (aloa (spectra-abs spectra-struct))
         (abs-diffs (mapcar
                #'(lambda (x)
                    (abs (- x nm))) alon))
         (min-diff (apply #'min abs-diffs))
        (min-position (position min-diff abs-diffs))
        (nm (nth min-position alon))
        (abs (nth min-position aloa)))
    (make-point :x nm :y abs)))

(defun find-window (alon lower-limit upper-limit)
  "Return the positions of the window in the spectra"
             (mapcan #'(lambda (x)
                         (and (> x lower-limit)
                              (< x upper-limit)
                              (list (position x alon)))) alon))

(defun target-window (a-func spectra lower-pos upper-pos
                      &optional (default-lower 390) (default-upper 400))
  "Return the lower and upper nm values with a positive first derivative"
  (labels ((differ (a-func alon aloa)
             (cond ((equal 1 (length aloa)) nil)
                   (t (cond ((funcall a-func (- (second aloa) (first aloa)))
                             (cons (first alon) (differ a-func (rest alon) (rest aloa))))
                            (t (differ a-func (rest alon) (rest aloa))))))))
    (let* ((alon (subseq (spectra-nm spectra) lower-pos upper-pos))
           (aloa (subseq (spectra-abs spectra) lower-pos upper-pos))
           (alon-pos (differ a-func alon aloa))
           (new-lower (first alon-pos))
           (new-upper (car (last alon-pos))))
      (cond ((and new-lower new-upper); test if positive values found
             (cond ((>= (- new-upper new-lower) 8)(list new-lower new-upper))
                   (t (list default-lower default-upper))))
            (t (list default-lower default-upper))))))

;; (find-window (spectra-nm (car *spectra*)) 375 400)
;; (target-window #'plusp (car *spectra*) 10 59)
;; (dolist (spectra *spectra*)
;;   (let ((window (target-window #'plusp spectra 10 59)))
;;     (format t "~A:~D~%" (spectra-id spectra) window)))

(defun find-base1 (a-func spectra derivative-spectra lower-limit upper-limit)
  "Select points with positive or negative slope and max 2nd derivative"
  (let* ((window (find-window (spectra-nm spectra) lower-limit upper-limit))
         (limits (target-window a-func spectra
                                        (first window)(car (last window))))
         ;; if limits are nil use original limits
         ;; (refined-limits (cond ((null limits) (list lower-limit upper-limit))
         ;;                       (t limits)))
         (d-alon (spectra-nm derivative-spectra))
         (d-aloa (spectra-abs derivative-spectra))
         (d-limits (find-window d-alon (first limits)
                                  (second limits)))
         (d-window (subseq d-aloa
                         (first d-limits)
                         (car (last d-limits))))
         (max-val (apply #'max d-window))
         (base-location (+ (position max-val d-window)
                           (first d-limits)))
         (base-nm (nth base-location d-alon)))
    (find-nearest-point base-nm spectra)))

(defun find-base2 (spectra derivative-spectra lower-limit upper-limit)
  "Select point with a max 2nd derivative"
  (let* ((d-alon (spectra-nm derivative-spectra))
         (d-aloa (spectra-abs derivative-spectra))
         (d-limits (find-window d-alon lower-limit upper-limit))
         (d-window (subseq d-aloa
                         (first d-limits)
                         (car (last d-limits))))
         (max-val (apply #'max d-window))
         (base-location (+ (position max-val d-window)
                           (first d-limits)))
         (base-nm (nth base-location d-alon)))
    (find-nearest-point base-nm spectra)))

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

(defun find-triangle (spectra &optional
                                (first 375) (second 403) (third 406) (fourth 420)
                                (window 5))
  "Document here please"
  (let* ((peak (find-peak spectra second third))
         (smoothed (smoothed-2derivative spectra window))
         (base1 (find-base1 #'plusp spectra smoothed first second))
         (base2 (find-base2 spectra smoothed third fourth)))
    (make-triangle :base1 base1 :peak peak :base2 base2)))

;(find-triangle (third *spectra*))
