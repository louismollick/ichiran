(defpackage :ichiran/rest-api
  (:use :cl)
  (:export :start))

(in-package :ichiran/rest-api)

(defun handler (env) 
  (declare (ignore env))
  '(200 nil ("Hello World, redefinable!")))

(defun start ()
  (clack:clackup (lambda (env) (funcall 'handler env)) 
  :address "0.0.0.0"
  :port 5001
  :use-thread nil))