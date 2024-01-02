(defpackage :ichiran/rest-api
 (:use :cl :ichiran/all)
 (:export :start))

(in-package :ichiran/rest-api)

(defmethod jsown:to-json ((word-info word-info))
 (jsown:to-json (word-info-gloss-json word-info)))

(defun handler (env) 
 (declare (ignore env))
 (handler-case 
  (destructuring-bind (&key path-info &allow-other-keys) env
   (let ((input (subseq path-info 1))) ;; remove leading "/" from path
    `(200 nil (,(jsown:to-json (romanize* input :limit (parse-integer (uiop:getenv "LIMIT"))))))))
  (error (c)
   `(400 nil (,(format nil "ERROR: ~a~%" c))))))
    
(defun start ()
 (clack:clackup (lambda (env) (funcall 'handler env)) 
  :address "0.0.0.0"
  :port (parse-integer (uiop:getenv "PORT"))
  :use-thread nil))