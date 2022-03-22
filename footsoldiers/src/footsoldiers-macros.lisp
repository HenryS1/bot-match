(defpackage :footsoldiers-macros
  (:use :cl :alexandria)
  (:export :def-copy-struct))

(in-package :footsoldiers-macros)

(defun make-constructor-name (struct-name)
  (intern (concatenate 'string "MAKE-" (symbol-name struct-name))))

(defun symbol-to-keyword (sym)
  (make-keyword (symbol-name sym)))

(defun make-reader (struct-name slot-name)
  (intern (concatenate 'string (symbol-name struct-name) "-" (symbol-name slot-name))))

(defun make-initialisers (struct-name slots initargs obj-name)
  (let ((uninitialised (remove-if (lambda (slot) 
                                    (some (lambda (arg) 
                                            (equal (symbol-name (car arg)) (symbol-name slot)))
                                          initargs)) slots)))
    (append (apply #'append initargs)
            (apply #'append (mapcar (lambda (slot-name) 
                                      `(,(symbol-to-keyword slot-name) 
                                         (,(make-reader struct-name slot-name) ,obj-name)))
                                    uninitialised)))))

(defun pair-initargs (initargs)
  (loop for arg = initargs then (cddr arg)
     while arg
     collect (list (car arg) (cadr arg))))

(defmacro def-copy-struct (name &rest properties)
  (with-gensyms (obj initargs)
    `(progn
       (defstruct ,name ,@properties)
       (defmacro
           ,(intern (concatenate 'string "DUPLICATE-" (symbol-name name)))
           (,obj &rest ,initargs)
         `(,(make-constructor-name ',name)
           ,@(make-initialisers
              ',name
              ',(mapcar (lambda (p) (if (consp p) (car p) p)) properties)
              (pair-initargs ,initargs) ,obj))))))
