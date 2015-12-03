#lang racket

(begin

  (require (planet "main.rkt" ("neil" "sicp.plt" 1 (= 17)))
           rackunit)

  (define (add x y)
    (+ x y))

  (check-equal? (add 1 2) 3)
  (check-equal? (add 2 2) 4)


  (define (new-work-day? day hour)
    (cond ((or (and (= day 2)
                    (= hour 8))
               (and (= day 3)
                    (= hour 8))) true)
          (true false)))
  (check-equal? (new-work-day? 1 2) false)
  (check-equal? (new-work-day? 1 8) false)
  (check-equal? (new-work-day? 2 8) true)
  (check-equal? (new-work-day? 3 8) true)


  (check-equal? (a-plus-abs-b 3 2) 5)

  (define (negative? a) (< a 0))

  (define add +)
  (define sub -)

  (define (a-plus-abs-b a b)
    ((if (negative? b) sub add) a b))

  (define (a-plus-abs-b-lame a b)
    (if (negative? b)
        (- a b)
        (+ a b)))

  (check-equal? (a-plus-abs-b-lame 3 2) 5)
  (check-equal? (a-plus-abs-b-lame 3 -2) 5)
  )

