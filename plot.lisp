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
    (declare (ignorable file-path))
    (format strm "set term push~C" #\Linefeed)
    ;; (format strm "set terminal wxt size 550,500 enhanced font 'Verdana,10' persist ~C" #\Linefeed)
    (format strm "set term png truecolor ~C" #\Linefeed)
    (format strm "set output '~A.png'~C" file-path #\Linefeed)
    (format strm "set style fill transparent solid 0.65 ~C" #\Linefeed)
    (format strm "set style line 1 lc rgb '#4682b4' lt 1 lw 3 ~C" #\Linefeed) ;steelblue
    (format strm "set style line 2 lc rgb '#b22222' lt 1 lw 3 ~C" #\Linefeed)  ;firebrick
    (format strm "set grid ytics lc rgb '#bbbbbb' lw 1 lt 0~C" #\Linefeed)
    (format strm "set grid xtics lc rgb '#bbbbbb' lw 1 lt 0~C" #\Linefeed)
    ;;(format strm "set title 'Porphyrin Absorbance Spectra Baseline Correction'~C" #\Linefeed)
    (format strm "set xlabel 'Wavelength'~C" #\Linefeed)
    (format strm "set ylabel 'Absorbance'~C" #\Linefeed)
    ;(formula-string strm spectra-struct)
    (format strm "plot '-' u 1:2 w lines ls 2 title 'data', '-' u 1:2 w lines ls 1 title 'background' ~C" #\Linefeed)
    (format-data-set strm spectra-object)
    (format strm "e~C" #\Linefeed)
    (format strm "set output~C" #\Linefeed)
    (format strm "~Cset term pop~C" #\Linefeed #\Linefeed)
    (make-string-input-stream strm)))




;; (with-open-file (out "~/lisp/site/porph-screen/devel/spectra.gp"
;;                      :direction :output
;;                      :if-exists :supersede)
;;   (let ((in (gnuplot-stream (car (los *spectra*)))))
;;     (when in
;;       (loop for line = (read-line in nil)
;;          while line do (format out "~a~%" line))
;;       (close in))))




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
