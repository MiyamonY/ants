;;; File:  main.scm
;; Author: ymiyamoto
;;
;; Created on Tue Oct 15 00:11:47 2019
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

(define s (read-line))

(define (aux sum str l)
  (cond ((null? l)
	 (when (= sum 7)
	   (print (string-copy (string-append str "=7") 1))
	   (exit)))
	(else
	 (let1 n (car l)
	       (aux (+ sum n) (string-append str (format "+~a" n)) (cdr l))
	       (aux (- sum n) (string-append str (format "-~a" n)) (cdr l))))))

(define (solve)
  (define nums (map digit->integer (string->list s)))
  (aux 0 "" nums))

(solve)
