(in-package :common-lisp-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun ~ (path)
    (merge-pathnames path (user-homedir-pathname)))

  (defun load-quicklisp ()
    (block nil
      (flet ((try (x) (when (probe-file x) (return (load x)))))
        (try (~ "quicklisp/setup.lisp"))
        (try (~ ".quicklisp/setup.lisp"))
        (error "Can't find an installation of quicklisp."))))

  #-quicklisp
  (load-quicklisp))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload 'fiveam))

(push (truename #p"./") asdf:*central-registry*)
(ql:quickload 'spritz :verbose t)
(ql:quickload (asdf:system-depends-on (asdf:find-system 'spritz/tests)))
(ql:quickload 'spritz/tests :verbose t)

(let ((result-list (fiveam:run :spritz)))
  (fiveam:explain! result-list)
  (uiop:quit
   (if (remove-if-not
        (lambda (res)
          (typep res 'fiveam::test-failure))
        result-list)
       1 0)))
