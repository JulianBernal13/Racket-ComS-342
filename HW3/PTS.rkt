#lang racket
(require racket/trace)
(provide (all-defined-out)) 

;;;; These are the ones already given
(define pts1 '((0 1) (2 3) (1 0) (3 3) (5 0))) ;; '(-27 1459)

(define pts2 '((0 0) (1 1) (2 2) (3 3))) ;; '(1000 0)

(define pts3 '((1 0) (2 1) (3 2) (4 3))) ;; '(1000 -1000)


#|
The following are used for grading
|#
;; growing slowly but linearly; '(200 0)
(define p1 '((0 0) (1 1/5) (2 2/5) (3 3/5) (4 4/5)))

;; growing slowly with an intercept
(define p2 '((0 100) (1 100.05) (2 100.10) (3 100.15) (4 100.2))) ; '(50 100000)

;; one point out of place
(define p3 '((0 0) (1 1/5) (2 2/5) (3 3) (4 4/5))); '(440 0)

(define p4 '((0 100) (1 100.05) (2 10) (3 100.15) (4 100.2))); '(50 81980)

;; points out of order
(define p5  '((1 1/5) (4 4/5) (2 2/5) (0 0) (3 3/5) )) ; '(200 0) 

(define p6 '((2 100.10) (0 100) (4 100.2) (1 100.05)  (3 100.15) )) ;'(50 100000)

;;inverse relation
(define p7 '((0 20) (1 15) (2 10) (3 5) (4 0))) ; '(-5000 20000)

#|

> (translatelist (gradient_mc p1 0.1 0.001 5000))
'(180 48 1)

> (translatelist (gradient_mc p2 0.1 0.001 500))
'(68 99948 1)

> (translatelist (gradient_mc p2 0.1 0.001 50))
'(1489 95897 5643)

> (translatelist (gradient_mc p2 0.1 0.001 5))
'(23150 34604 1435726)

> (translatelist (gradient_mc p2 0.1 0.001 5000))
'(68 99948 1)

> (translatelist (gradient_mc p3 0.1 0.001 5000))
'(440 0 806)

(translatelist (gradient_mc p4 0.1 0.001 5000))
'(50 81980 1298882)

(translatelist (gradient_mc p5 0.01 0.0005 5000))
'(187 38 0)

(translatelist (gradient_mc p5 0.01 0.001 5000))
'(179 52 1)

; not used - similar to the one above
;> (translatelist (gradient_mc p5 0.01 0.01 5000))
;'(148 47 9)

(translatelist (gradient_mc p6 0.01 0.0001 5000))
'(56 99983 0)

(translatelist (gradient_mc p6 0.01 0.01 5000))
'(110 99828 10)

(translatelist (gradient_mc p7 0.01 0.01 1))
'(200 200 142440)

(translatelist (gradient_mc p7 0.01 0.01 5000))
'(-4940 19828 10)

|#