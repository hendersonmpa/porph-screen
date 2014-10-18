;;;; snip.lisp
;; Background subtraction method C.G Ryan et al 1998
(in-package :porph-screen)

(defparameter *window* 5 "Default window size for mppc")
(defparameter *iterations* 50 "Default number of iterations for mppc-do")

;; (defun log-trans (y-list)
;; "low statistics digital filter"
;;   (mapcar #'(lambda (y)
;;               (log (+ (log (+ y 1)) 1))) y-list))

;; (defun back-trans (z-list)
;; "back transformation for lsdf function
;; x = log (y+1)
;; solve for y in terms of x:
;; e^x = y + 1
;; y = e^x - 1. "
;; (mapcar #'(lambda (z)
;;             (- (exp (- (exp z) 1)) 1)) z-list))

(defun z-bar (z-list p w)
"Mean of points +/- window w from point p"
  (let* ((len (- (length z-list) 1))
        (w-back (min w p))
        (w-for (min w (- len p)))
        (back (elt z-list (- p w-back)))
        (forward (elt z-list (+ p w-for))))
    (/ (+ back forward) 2)))

(defun mppc (z-list &optional (w *window*))
  "Multi-Pass Peak Clipping
min[z(x), z-bar(x,w)]
z-bar(x,w) = [z(x + w) + z (x - w)]/2"
  (let ((accum nil)
        (len (length z-list)))
    (dotimes (p len (reverse accum))
      (push (min (elt z-list p) (z-bar z-list p w)) accum))))

(defun mppc-do (spectra-struct &optional (n *iterations*))
  (let ((z-list (spectra-abs spectra-struct)))
    (do ((count 0 (1+ count))
         (new-list z-list (mppc new-list)))
        ((> count n) new-list))))

(defun print-spectra (y-list x-list &optional (stream t) (count 0))
  "Print a spectra-structin gnuplot format:
gnuplot> plot for [IDX=start:end:step] 'file-name' index IDX u 1:2 title columnheader(1)"
  (format stream " 'n=~D~%'" count)
  (loop for x in x-list
     for y in y-list
     do (format stream "~A ~20T~A~%" x y))
  (terpri stream)
  (terpri stream))

(defun mppc-to-file (spectra-struct &optional (w *window*) (n *iterations*) )
  "gnuplot> plot for [IDX=0:50:10] 'mppc_5' index IDX u 1:2 title columnheader(1)"
  (let ((file-name (concatenate 'string "mppc_" (write-to-string w)))
        (z-list (spectra-abs spectra-struct))
        (x-list (spectra-nm spectra-struct)))
    (with-open-file (out file-name :direction :output
                         :if-exists :supersede)
      (do ((count 0 (1+ count))
           (new-list z-list (mppc new-list w)))
          ((equal count n) new-list)
        (print-spectra new-list x-list out count)))))

(defun background-substraction (spectra-struct)
  (let* ((abs (spectra-abs spectra-struct))
         (background (mppc-do spectra-struct))
         (corrected (map 'list #'- abs background)))
    (setf (spectra-bkgd spectra-struct) background)
    corrected))

(defun net-abs (spectra-struct &optional (lower-limit 401) (upper-limit 405))
  (let* ((alon (spectra-nm spectra-struct))
         (corrected (background-substraction spectra-struct))
         (nm-window (mapcan
                     #'(lambda (x)
                         (and (> x lower-limit)
                              (< x upper-limit)
                              (list (position x alon)))) alon))
         (abs-values (subseq corrected
                             (first nm-window)
                             (car (last nm-window))))
         (max-abs (apply #'max abs-values)))
    (setf (spectra-net-abs spectra-struct) max-abs)))
