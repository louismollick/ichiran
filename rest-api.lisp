(defpackage :ichiran/rest-api
  (:use :cl)
  (:export :start))

(in-package :ichiran/rest-api)

(defun handler (env) 
  (declare (ignore env))
  '(200 nil ("Hello World, redefinable!")))

(defun start ()
  (clack:clackup (lambda (env) (funcall 'handler env)) 
  :port 5003
  :use-thread nil))