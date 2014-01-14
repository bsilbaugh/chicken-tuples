;;;
;;; Tuples Test Suite
;;;

(use test)

(use tuples)

(test-group "element access"
  (test 'a (triple-elem-0 (make-triple 'a 'b 'c)))
  (test 'b (triple-elem-1 (make-triple 'a 'b 'c)))
  (test 'c (triple-elem-2 (make-triple 'a 'b 'c))))

(test-group "reduce"
  (test 0.6 (triple-reduce + (make-triple 0.1 0.2 0.3))))

(test-group "map"
  (test-assert (equal? (make-triple 1.1 1.2 1.3)
					   (triple-map-ufun (lambda (x) (+ x 1))
										(make-triple 0.1 0.2 0.3))))
  (test-assert (equal? (make-triple 1.1 2.2 3.3)
					   (triple-map-bfun (lambda (x y) (+ x y))
										(make-triple 0.1 0.2 0.3)
										(make-triple 1.0 2.0 3.0)))))

(test-group "for-each"
  (test-assert (let ((vals '()))
				 (triple-for-each-ufun 
				  (lambda (p) (set! vals (cons p vals)))
				  (make-triple 'a 'b 'c))
				 (equal? '(a b c) (reverse vals))))
  (test-assert (let ((vals '()))
				 (triple-for-each-bfun 
				  (lambda (p q) (set! vals (cons (cons p q) vals)))
				  (make-triple 'a 'b 'c)
				  (make-triple 'x 'y 'z))
				 (equal? '((a . x) (b . y) (c . z)) (reverse vals)))))

