;;;
;;; Performance comparison between Tuples objects and built-in data
;;; structures.

(use tuples)

;;; Helpers

;; Execute function f n times
(define ((repeat-call n) f)
  (define (iter f n i)
	(if (< i n)
		(begin
		  (f)
		  (iter f n (+ i 1)))))
  (iter f n 0))

(define ((scale-by c) x)
  (* c x))

;;; Define equivolent map functions for R5RS vectors

;; Unitary function map
(define (vector-map-ufun f u)
  (let ((v (make-vector 3)))
	(vector-set! v 0 (f (vector-ref u 0)))
	(vector-set! v 1 (f (vector-ref u 1)))
	(vector-set! v 2 (f (vector-ref u 2)))))

;; Binary function map
(define (vector-map-bfun f u v)
  (let ((w (make-vector 3)))
	(vector-set! w 0 (f (vector-ref u 0) (vector-ref v 0)))
	(vector-set! w 1 (f (vector-ref u 1) (vector-ref v 1)))
	(vector-set! w 2 (f (vector-ref u 2) (vector-ref v 2)))))

;;; Compare performance

;; Unitary function map
(let ((u (make-triple 0.1 0.2 0.3))
	  (v (list->vector '(0.1 0.2 0.3)))
	  (f (scale-by 0.5))
	  (rcall (repeat-call 1000000)))
  (begin
	(print "----")
	(print "triple:")
    (time (rcall (lambda () (triple-map-ufun f u))))
	(print "vector:")
	(time (rcall (lambda () (vector-map-ufun f v))))))

;; Binary function map
(let ((u1 (make-triple 0.1 0.2 0.3))
	  (u2 (make-triple 0.01 0.02 0.03))
	  (v1 (list->vector '(0.1 0.2 0.3)))
	  (v2 (list->vector '(0.01 0.02 0.03)))
	  (rcall (repeat-call 2000000)))
  (begin
	(print "----")
	(print "triple:")
    (time (rcall (lambda () (triple-map-bfun * u1 u2))))
	(print "vector:")
	(time (rcall (lambda () (vector-map-bfun * v1 v2))))))
