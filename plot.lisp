;;;; plot.lisp
(in-package #:porph-screen)

;; (defmethod write-plot ((plot clnuplot::gnuplot) (style (eql :png)))
;;   (with-new-file (out (clnuplot:fullpath plot))
;;     (format out "set term push~C" #\Linefeed)
;;     (format out "set term png~C" #\Linefeed)
;;     (format out "set output '~A.png'~C" (filename plot) #\Linefeed))
;;   (let ((*file-if-exists* :append))
;;     (clnuplot:write-plot plot :gnuplot)
;;     (with-new-file (out (clnuplot:fullpath plot))
;;       (format out "set output~C" #\Linefeed)
;;       (format out "~Cset term pop~C" #\Linefeed #\Linefeed)))
;;   plot)

;; (defmethod write-plot ((plot clnuplot::gnuplot) (style (eql :png-image)))
;;   (write-plot plot :png)
;;   (clnuplot::execute-plot plot))

;; (with-open-file (out #p "~/Desktop/derivative.txt" :direction :output)
;;   (mapcar #'(lambda (n a) (format out "~D   ~D~%" n a))
;;           (mid-point *alon*) (derivative *aloa*)))

;; (with-open-file (out #p "~/Desktop/2derivative.txt" :direction :output)
;;   (mapcar #'(lambda (n a) (format out "~D   ~D~%" n a))
;;           (mid-point (mid-point *alon*)) (derivative (derivative *aloa*))))


(defun gp-format-data (filename specta-struct)
  (with-open-file (out filename :direction :output)
    (apply #'mapcar #'(lambda (n a) (format out "~D   ~D~%" n a)) specta-struct)))


(defun formula-string (str triangle-struct)
  (let* ((base1 (triangle-base1 triangle-struct))
         (base2 (triangle-base2 triangle-struct))
         (m (slope base1 base2))
         (b (intercept base1 base2 m)))
    (format str "plot ~D*x + ~D lc rgb 'black'," m b)))

(defun format-data-set (str xdata ydata)
  (loop for x in xdata
     for y in ydata
     do (format str "~D~C~D~C" x #\tab y #\Linefeed)))

(defun gnuplot-stream (filename spectra-struct)
  (let ((str (make-array 0
                         :element-type 'character
                         :adjustable t
                         :fill-pointer 0))
        (triangle (find-triangle spectra-struct)))
    (format str "set term push~C" #\Linefeed)
    (format str "set term png~C" #\Linefeed)
    (format str "set output '~A.png'~C" filename #\Linefeed)
    (format str "set title 'Porphyrin Absorbance Spectra Baseline Correction'~C" #\Linefeed)
    (formula-string str triangle)
    (format str "'-' u 1:2~C" #\Linefeed)
    (format-data-set str (spectra-nm spectra-struct) (spectra-abs spectra-struct))
    (format str "e~C" #\Linefeed)
    (format str "set output~C" #\Linefeed)
    (format str "~Cset term pop~C" #\Linefeed #\Linefeed)
    (make-string-input-stream str)))


(sb-ext:run-program "tee" '("test")
                    :input (gnuplot-stream "test" *data-set*)
                    :search t
                    :wait t)

(defun plot-data (filename spectra-struct)
  (sb-ext:run-program "gnuplot" nil
                      :input (gnuplot-stream filename spectra-struct)
                      :search t
                      :wait t))

(plot-data "test" *data-set*)
(sb-ext:run-program "gnuplot"
                    '("./figures/clnuplot-example-1.data") :search t)

;; (plot-data "test" *data-set*)
;; (plot-data "test1" (derivative *data-set*))

;; (plot-data "test2" (second-derivative *data-set*))

;; (plot-data "test3" (smoothed-2derivative *data-set* 5))
