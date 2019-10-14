;;; File:  main.scm
;; Author: ymiyamoto
;;
;; Created on Mon Oct 14 02:58:35 2019
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

(define (aux sum acc l)
  (if (null? l)
      (+ sum acc)
      (let* ((n (digit->integer (car l)))
	     (added (+ (* 10 acc) n)))
	(+ (aux sum added (cdr l))
	   (aux (+ sum added) 0(cdr l))))))


(define (solve)
  (print (/ (aux 0 0 (string->list s)) 2)))

(solve)
