;; * Overview

;; This program is an Html generator for Common Lisp.

;; Why another one?

;; I want programmatically generate and process Html.
;; In particular I want to

;; - Store Html in variables

;; - Use Html as a function argument

;; - Use Html as a function return value

;; To achieve this, we transform symbolic expressions into lists of tag
;; structures.  The method print-html then prints its input as properly
;; escaped Html.


;; * Package

;; We only use common-lisp.

(defpackage html
  (:use :common-lisp)
  (:export :render :print-html :print-html-to-string :html :unsafe))

(in-package :html)

;; * Render

;; The /RENDER/ method is called by /PRINT-HTML/ to map an
;; object into something that /PRINT-HTML/ is specialized for.
;; By default, /RENDER/ calls /princ-to-string/.

(defgeneric render (object)
  (:method (object)
    (princ-to-string object)))

;; * Print Html

;; The interpreter. Calls /render/ for objects it is not specialized
;; for.

(defgeneric print-html (object stream)
  (:method (object stream)
    (print-html (render object) stream))
  (:method ((char character) stream)
    (case char
      (#\< (write-string "&lt;" stream))
      (#\> (write-string "&gt;" stream))
      (#\& (write-string "&amp;" stream))
      (#\" (write-string "&quot;" stream))
      (t (write-char char stream))))
  (:method ((string string) stream)
    (map nil (lambda (char) (print-html char stream)) string))
  (:method ((list list) stream)
    (dolist (object list)
      (print-html object stream))))

;; Print /OBJECT/ to string.  Note: This function is also used to
;; escape attributes.

(defun print-html-to-string (object)
  (with-output-to-string (stream)
    (print-html object stream)))

;; * Tag

(defstruct tag name attrs children)

(defmethod print-html ((self tag) stream)
  (print-html (tag-children self) stream))

(defmethod print-html :before ((self tag) stream)
  (format stream "~&<~(~a~)~{ ~(~a~)=~s~}>" (tag-name self)
	  (loop for (k v) on (tag-attrs self) by #'cddr when v
	     collect (print-html-to-string k) and
	     collect (print-html-to-string (if (eq v t) k v)))))

(defmethod print-html :after ((self tag) stream)
  (unless (member (tag-name self) (list :input))
    (format stream "</~(~a~)>~&" (tag-name self))))

;; * Html DSL 

;; Macroexpand example:

;; The code

;; #+begin_example
;;  (print-html-to-string
;;    (html
;;      ((:span :style "color:blue") "text")))
;; #+end_example

;; expands to

;; #+begin_example
;; (PRINT-HTML-TO-STRING (LIST (MAKE-TAG :NAME
;;                                       :SPAN
;;                                       :ATTRS
;;                                       (LIST :STYLE "color:blue")
;;                                       :CHILDREN
;;                                       (HTML "text"))))
;; #+end_example

;; and evaluates to

;; #+begin_example
;; "<span style=\"color:blue\">text</span>"
;; #+end_example

;; The html generation macro: 

(defmacro html (&body body)
  (labels ((listify (x) (if (listp x) x (list x)))
           (codegen (x)
             (cond ((atom x) x)
                   ((not (keywordp (car (listify (car x))))) x)
                   (t (destructuring-bind (head &rest body) x
                        (destructuring-bind (name &rest attrs) (listify head)
                          `(make-tag :name ,name :attrs (list ,@attrs)
                                     :children (html ,@body))))))))
    `(list ,@(mapcar #'codegen body))))

;; * Extending the Html package

;; ** Doctype

;; Print doctype.

(defmethod print-html ((self (eql :doctype-html)) stream)
  (format stream "<!doctype html>~&"))

;; ** Unsafe

;; Print string without escaping

(defstruct (unsafe (:constructor unsafe (string))) string)

(defmethod print-html ((unsafe unsafe) stream)
  (write-string (unsafe-string unsafe) stream))


