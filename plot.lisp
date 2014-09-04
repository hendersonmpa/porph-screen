;;;; plot.lisp
(in-package :porph-screen)

;; (with-open-file (out #p "~/Desktop/derivative.txt" :direction :output)
;;   (mapcar #'(lambda (n a) (format out "~D   ~D~%" n a))
;;           (mid-point *alon*) (derivative *aloa*)))

;; (with-open-file (out #p "~/Desktop/2derivative.txt" :direction :output)
;;   (mapcar #'(lambda (n a) (format out "~D   ~D~%" n a))
;;           (mid-point (mid-point *alon*)) (derivative (derivative *aloa*))))


(defun gp-format-data (filename specta-struct)
  (with-open-file (out filename :direction :output)
    (apply #'mapcar #'(lambda (n a) (format out "~D   ~D~%" n a)) specta-struct)))

(defun formula-string (strm spectra-struct)
  (let* ((triangle-struct (spectra-triangle spectra-struct))
         (base1 (triangle-base1 triangle-struct))
         (base2 (triangle-base2 triangle-struct))
         (m (slope base1 base2))
         (b (intercept base1 base2 m)))
    (format strm "plot ~D*x + ~D lc rgb 'black'," m b)))

(defun format-data-set (strm xdata ydata)
  (loop for x in xdata
     for y in ydata
     do (format strm "~D~C~D~C" x #\tab y #\Linefeed)))

(defun gnuplot-stream (spectra-struct)
  (let ((strm (make-array 0
                         :element-type 'character
                         :adjustable t
                         :fill-pointer 0)))
    (format strm "set term push~C" #\Linefeed)
    (format strm "set term png~C" #\Linefeed)
    (format strm "set output '~A.png'~C" (spectra-id spectra-struct) #\Linefeed)
    ;;(format strm "set title 'Porphyrin Absorbance Spectra Baseline Correction'~C" #\Linefeed)
    (format strm "set xlabel 'Wavelength'~C" #\Linefeed)
    (format strm "set ylabel 'Absorbance'~C" #\Linefeed)
    (formula-string strm spectra-struct)
    (format strm "'-' u 1:2 linetype 1 ~C" #\Linefeed)
    (format-data-set strm (spectra-nm spectra-struct) (spectra-abs spectra-struct))
    (format strm "e~C" #\Linefeed)
    (format strm "set output~C" #\Linefeed)
    (format strm "~Cset term pop~C" #\Linefeed #\Linefeed)
    (make-string-input-stream strm)))

;; (sb-ext:run-program "tee" '("test")
;;                     :input (gnuplot-stream "test" *data-set*)
;;                     :search t
;;                     :wait t)

(defun plot-data (spectra-struct)
  (sb-ext:run-program "gnuplot" nil
                      :input (gnuplot-stream spectra-struct)
                      :search t
                      :wait t))

;; (plot-data "test" *data-set*)
;; (sb-ext:run-program "gnuplot"
;;                     '("./figures/clnuplot-example-1.data") :search t)

;; (plot-data "test" *data-set*)
;; (plot-data "test1" (derivative *data-set*))

;; (plot-data "test2" (second-derivative *data-set*))

;; (plot-data "test3" (smoothed-2derivative *data-set* 5))
