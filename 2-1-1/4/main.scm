;;; File:  main.scm
;; Author: ymiyamoto
;;
;; Created on Tue Oct 15 01:30:03 2019
;;
(define-syntax read-number
  (syntax-rules ()
    ((_ nums)
     (define-values nums
       (apply values (map string->number (string-split (read-line) #\space)))))))

(define-syntax read-numbers
  (syntax-rules ()
    ((_ as)
     (define as (map string->number (string-split (read-line) #\space))))
    ((_ as n)
     (define as (map (lambda (_)
		       (map string->number (string-split (read-line) #\space))) (iota n))))))

(define-syntax prlist
  (syntax-rules ()
    ((_ lis)
     (print (string-join (map number->string lis) " ")))))

(define-syntax 1+ (syntax-rules () ((_ x) (+ x 1))))

(define-syntax 1- (syntax-rules () ((_ x) (- x 1))))

;; for gauche 0.9.3.3
(define (append-map thunk l)
  (apply append (map thunk l)))

(define MOD 1000000007)

(use util.match)

(read-number (n))
(read-numbers as n)

(define (aux as left right)
  (if (null? as)
      (max left right)
      (match-let1 (a) (car as)
		  (min (aux (cdr as) (+ a left) right)
		       (aux (cdr as) left (+ a right))))))

(define (solve)
  (print (aux as 0 0)))

(solve)
