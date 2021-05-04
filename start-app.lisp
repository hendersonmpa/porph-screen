;;;$ sbcl --noinform --load "start-app.lisp"
;; called by start-app.sh
(asdf:load-system :porph-screen)
(porph-screen::start-server)

;; To keep the server running.
(defvar *alive* t)
(loop (sleep 1000) (if (not *alive*) (quit)))

;; (load "/home/mpah/lisp/swank-image.lisp")
;; (load "~/quicklisp/setup.lisp")
;; (ql:quickload :porph-screen)
;; (porph-screen::start-server)
