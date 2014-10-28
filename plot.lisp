;;;; plot.lisp
(in-package :porph-screen)

;; (defun gp-format-data (filename spectra-struct)
;;   (with-open-file (out filename :direction :output
;;                        :if-exists :supersede)
;;     (apply #'mapcar #'(lambda (n a) (format out "~D~C~D~%" n #\tab a))
;;            (list (spectra-nm spectra-struct) (spectra-abs spectra-struct)))))

;; (defun formula-string (strm spectra-struct)
;;   (let* ((triangle-struct (spectra-triangle spectra-struct))
;;          (base1 (triangle-base1 triangle-struct))
;;          (base2 (triangle-base2 triangle-struct))
;;          (m (slope base1 base2))
;;          (b (intercept base1 base2 m)))
;;     (format strm "plot ~D*x + ~D lc rgb 'black'," m b)))

(defun format-data-set (strm spectra-object)
  (let ((ab (ab spectra-object))
        (nm (nm spectra-object))
        (bkgd (bkgd spectra-object)))
    (map 'nil #'(lambda (x y)
                  (format strm "~D~C~D~C" x #\tab y #\Linefeed)) nm ab)
    (format strm "e~C" #\Linefeed)
    (map 'nil #'(lambda (x b)
                  (format strm "~D~C~D~C" x #\tab b #\Linefeed)) nm bkgd)))

(defun gnuplot-stream (spectra-object)
  (let ((strm (make-array 0
                         :element-type 'character
                         :adjustable t
                         :fill-pointer 0))
        (file-path (concatenate 'string *data-repository* (id spectra-object))))
    (format strm "set term push~C" #\Linefeed)
    (format strm "set term png truecolor ~C" #\Linefeed)
    (format strm "set output '~A.png'~C" file-path #\Linefeed)
    (format strm "set grid ytics lc rgb '#bbbbbb' lw 1 lt 0~C" #\Linefeed)
    (format strm "set grid xtics lc rgb '#bbbbbb' lw 1 lt 0~C" #\Linefeed)
    ;;(format strm "set title 'Porphyrin Absorbance Spectra Baseline Correction'~C" #\Linefeed)
    (format strm "set xlabel 'Wavelength'~C" #\Linefeed)
    (format strm "set ylabel 'Absorbance'~C" #\Linefeed)
    ;(formula-string strm spectra-struct)
    (format strm "plot '-' u 1:2 linetype 1, '-' u 1:2 linetype 2 ~C" #\Linefeed)
    (format-data-set strm spectra-object)
    (format strm "e~C" #\Linefeed)
    (format strm "set output~C" #\Linefeed)
    (format strm "~Cset term pop~C" #\Linefeed #\Linefeed)
    (make-string-input-stream strm)))

;; (sb-ext:run-program "tee" '("test")
;;                     :input (gnuplot-stream (car *spectra*))
;;                     :search t
;;                     :wait t)

(defun plot-data (spectra-list)
  (let ((los (los spectra-list)))
    (dolist (spectra los)
      (sb-ext:run-program "gnuplot" nil
                          :input (gnuplot-stream spectra)
                          :search t
                          :wait t))))

(defun single-plot (spectra)
  (sb-ext:run-program "gnuplot" nil
                      :input (gnuplot-stream spectra)
                      :search t
                      :wait t))

;; (dolist (spectra *spectra*)
;;   (sb-ext:run-program "gnuplot" nil
;;                       :input (gnuplot-stream spectra)
;;                       :search t
;;                       :wait nil))
;; (plot-data (car *spectra*))

;; (sb-ext:run-program "gnuplot"
;;                     '("./figures/clnuplot-example-1.data") :search t)

;; (plot-data "test" *data-set*)
;; (plot-data "test1" (derivative *data-set*))

;; (plot-data "test2" (second-derivative *data-set*))

;; (plot-data "test3" (smoothed-2derivative *data-set* 5))
