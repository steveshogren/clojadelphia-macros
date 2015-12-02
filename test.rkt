#lang racket

(begin

  (require (planet "main.rkt" ("neil" "sicp.plt" 1 (= 17)))
           rackunit)

  (define (add x y)
    (+ x y))

  (check-equal? (add 1 2) 3)
  (check-equal? (add 2 2) 4)

  (define (work-day? day hour)
    (cond ((and (= hour 8)
                (or (= day 2)
                    (= day 3)
                    (= day 4)
                    (= day 5)
                    (= day 6))) true)
          (true false)))
  (check-equal? (work-day? 1 2) false)
  (check-equal? (work-day? 2 2) false)
  (check-equal? (work-day? 2 8) true)
  (check-equal? (work-day? 2 9) false)
  (check-equal? (work-day? 5 8) true)
  (check-equal? (work-day? 5 5) false)
  )
