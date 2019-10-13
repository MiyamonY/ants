;;; File:  main.scm
;; Author: ymiyamoto
;;
;; Created on Mon Oct 14 00:39:27 2019
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

(define (append-map thunk ls)
  (apply append (map thunk ls)))

(define MOD 1000000007)

(define (diff x y)
  (define diff-x (- (car x) (car y)))
  (define diff-y (- (cadr x) (cadr y)))
  (sqrt (+ (* diff-x diff-x) (* diff-y diff-y))))

(read-number (n))
(read-numbers as n)

(define (solve)
  (print (apply max (append-map (lambda (x) (map  (lambda (y) (diff x y)) as)) as))))

(solve)
