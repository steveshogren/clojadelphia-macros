#lang racket

(begin

  (require (planet "main.rkt" ("neil" "sicp.plt" 1 (= 17)))
           rackunit)

  (define (square x)
    (* x x))

  (define (average x y)
    (/ (+ x y) 2))

  (define (improve guess x)
    (average guess (/ x guess)))

  ;;(check-equal? (improve 1 2) 3/2)
  (define (new-if predicate then-clause else-clause)
    (if predicate
        then-clause
        else-clause))

  (define (sqrt-iter guess x)
    (new-if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x) x)))

  (define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.001))

  (define (sqrt x)
    (sqrt-iter 1.0 x))

  ;(sqrt 2)

  )

