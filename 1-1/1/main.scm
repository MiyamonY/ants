;;; File:  main.scm
;; Author: ymiyamoto
;;
;; Created on Sun Oct 13 20:35:32 2019
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

(define MOD 1000000007)

(define (binary-search vec pred)
  (let loop ((left -1)
	     (right (vector-length vec)))
    (if (>= left (1- right))
	right
	(let* ((mid (ash (+ left right) -1))
	       (result (pred (vector-ref vec mid))))
	  (loop (if result left mid) (if result mid right))))))

(define (dedup lst)
  (define (aux lst c acc)
    (if (null? lst)
	acc
	(if (= c (car lst))
	    (aux (cdr lst) c acc)
	    (aux (cdr lst) (car lst) (cons (car lst) acc)))))
  (reverse (aux lst (car lst) ())))

(define (solve)
  (read-number (n m))
  (let* ((as (cons 0
  		   (map (lambda (_) (string->number (read-line))) (iota n))))
	 (sums (list->vector (dedup (sort (apply append (map (lambda (x) (map (lambda (y) (+ x y)) as)) as)))))))
    (print (apply max (map (lambda (i)
			     (let* ((sum (vector-ref sums i))
				    (index (binary-search sums (lambda (x) (< m (+ sum x))))))
			       (if (> index 0)
				   (+ sum (vector-ref sums (1- index)))
				   0)))
			   (iota (vector-length sums)))))))

(solve)
