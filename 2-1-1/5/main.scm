;;; File:  main.scm
;; Author: ymiyamoto
;;
;; Created on Thu Oct 17 22:26:20 2019
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
     (define as (map (lambda (_) (map string->number (string-split (read-line) #\space))) (iota n))))))

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

(read-number (n m))
(read-numbers as m)

(use util.combinations)

(define (aquaitance s t)
  (find (lambda (x) (or (equal? (list s t) x) (equal? (list t s) x))) as))

(define (aux as set)
  (if (<= (length set) 1)
      (length set)
      (let loop ((pairs (combinations set 2)))
	(cond ((null? pairs) (length set))
	      ((aquaitance (car (car pairs)) (cadr (car pairs))) (loop (cdr pairs)))
	      (else 0)))))

(define (solve)
  (print (apply max (map (lambda (s) (aux as s)) (power-set (iota n 1))))))

(solve)
