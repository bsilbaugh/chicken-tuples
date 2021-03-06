;;;
;;; Tuples
;;;
;;; Copyright 2014 Benjamin Silbaugh
;;;
;;; See LICENSE file for redistribution and modification permissions.

(module tuples *

  (import scheme chicken)

(define-syntax define-tuple
  (er-macro-transformer
   (lambda (exp rename comp)
	 (letrec ((rng 
			   (lambda (a b)
				 (if (< a b)
					 (cons a (rng (+ a 1) b))
					 '())))
			  (elem 
			   (lambda (i)
				 (string->symbol (string-append "elem-" (number->string i)))))
			  (elem-acc 
			   (lambda (name i)
				 (symbol-append name 
								'-elem- 
								(string->symbol (number->string i)))))
			  (make 
				  (lambda (name size)
					`(define-record ,name ,@(map elem (rng 0 size)))))
			  (reduce 
			   (lambda (name size)
				 `(define ,(symbol-append name '-reduce)
					(lambda (f tpl)
					  (f ,@(map (lambda (i) `(,(elem-acc name i) tpl)) (rng 0 size)))))))
			  (for-each-ufun
			   (lambda (name size)
				 `(define ,(symbol-append name '-for-each-ufun)
					(lambda (f tpl)
					  (begin
					  ,@(map (lambda (i) `(f (,(elem-acc name i) tpl))) (rng 0 size)))))))
			  (for-each-bfun 
			   (lambda (name size)
				 `(define ,(symbol-append name '-for-each-bfun) 
					(lambda (f tpl-1 tpl-2)
					  (begin
					   ,@(map (lambda (i) 
								`(f (,(elem-acc name i) tpl-1)
									(,(elem-acc name i) tpl-2)))
							  (rng 0 size)))))))
			  (map-ufun 
			   (lambda (name size)
				 `(define ,(symbol-append name '-map-ufun) 
					(lambda (f tpl) 
					  (,(symbol-append 'make- name)
					   ,@(map (lambda (i) `(f (,(elem-acc name i) tpl))) (rng 0 size)))))))
			  (map-bfun 
			   (lambda (name size)
				 `(define ,(symbol-append name '-map-bfun) 
					(lambda (f tpl-1 tpl-2)
					  (,(symbol-append 'make- name)
					   ,@(map (lambda (i) 
								`(f (,(elem-acc name i) tpl-1)
									(,(elem-acc name i) tpl-2)))
							  (rng 0 size))))))))
	   (let ((name (cadr exp))
		   (size (caddr exp)))
	   `(begin
		  ;; constructor
		  ,(make name size)
		  ;; reduce map
		  ,(reduce name size)
		  ;; unitary function for-each
		  ,(for-each-ufun name size)
		  ;; binary function for-each
		  ,(for-each-bfun name size)
		  ;; unitary function map
		  ,(map-ufun name size)
		  ;; binary function map
		  ,(map-bfun name size)))))))

;;; Tuple instantiations

(define-tuple unit 0)

(define-tuple single 1)

(define-tuple double 2)

(define-tuple triple 3)

(define-tuple quad 4)

(define-tuple quintuple 5)

(define-tuple sextuple 6)

(define-tuple septuple 7)

(define-tuple octuple 8)

(define-tuple nonuple 9)

(define-tuple decuple 10)

); module
