#lang racket
;;; A function (binomial N k) that returns the binomial coefficients C(N, k), defined recursively as: C(N,0) = 1, C(N, N) = 1, and, for 0<k < N, C(N, k) = C(N-1, k) + C(N - 1, k -1).
;;; Test binomial for C(4,0), C(8, 8), C(3,2) and C(7, 4). 
(define (binomial n k)
  (if (or (= 0 k) (= n k))
      1
      (+ (binomial (- n 1) k) (binomial (- n 1) (- k 1)))))
(binomial 4 0)
(binomial 8 8)
(binomial 3 2)
(binomial 7 4)

;;; A function (mod N M) that returns the modulus remainder when dividing N by M.
;;; Test mod for arguments 9 and 5, 7 and 9, 100 and 37, 20 and 5, -11 and 3. 
(define (mod n m)
  (modulo n m))
(mod 9 5)
(mod 7 9)
(mod 100 37)
(mod 20 5)
(mod -11 3)
      
;;; A function (binaryToDecimal b) that takes a binary number and returns its decimal value. (binaryToDecimal 1101) returns 13. 
;;; Test binaryToDecimal with arguments 0, 1011, 111111, 10001.
(define (binaryToDecimal b)
  (if (= 0 b)
      0
      (+ (mod b 10) (* 2 (binaryToDecimal (quotient b 10))))))
(binaryToDecimal 0)
(binaryToDecimal 1011)
(binaryToDecimal 111111)
(binaryToDecimal 10001)

;;; A function (addBinary binaryList) that takes a list of binary numbers and returns their decimal sum. (addBinary '(1101 111 10 101)) returns 27 
;;; Test addBinary with (1101 111 10 101), (0), (11011).
(define (addBinary binaryList)
  (if (null? binaryList)
      0
      (+ (binaryToDecimal (car binaryList)) (addBinary (cdr binaryList)))))
(addBinary '(1101 111 10 101))
(addBinary '(0))
(addBinary '(11011))

;;; A function (min list) that returns the smallest value in a simple list of integers. 
;;; Test min with (4 5 1 2 5), (3), (), (5 5 5)
(define (min list)
  (if (null? list)
      '()
      (if (null? (cdr list))
          (car list)
          (if (< (car list) (min (cdr list)))
              (car list)
              (min (cdr list))))))
(min '(4 5 1 2 5))
(min '(3))
(min '())
(min '(5 5 5))
          

;;; A function (myRemove atm list) that removes all occurrences of the atom atm from a simple list, returning list with atm removed. myRemove should return the original list if atm is not found. 
;;; Test myRemove with atom a and list arguments (), (a), (a b c d a b a a), (x y z), (a (x y z) (r s t a)), (((a (l a) b) a) m a).
(define (myRemove atm list)
  (if (null? list)
      '()
      (if (list? (car list))
          (append (myRemove atm (car list)) (myRemove atm (cdr list)))
          (if (equal? atm (car list))
              (myRemove atm (cdr list))
              (cons (car list) (myRemove atm (cdr list)))))))
(myRemove 'a '())
(myRemove 'a '(a))
(myRemove 'a '(a b c d a b a a))
(myRemove 'a '(x y z))
(myRemove 'a '(a (x y z) (r s t a)))
(myRemove 'a '(((a (l a) b) a) m a))


;;; A function (selectionSort list) that returns a simple list of integers in ascending order using a recursive selection sort algorithm. Hint: use your min function. 
;;; Test selectionSort with lists (), (5), (6 10 23 12 2 9 18 1 0 15), (3 4 7 3 7 7 4 3 2 3 7)
(define (deleteOnce atm list)
  (if (null? list)
      '()
      (if (equal? atm (car list))
          (cdr list)
          (cons (car list) (deleteOnce atm (cdr list))))))
(deleteOnce '5 '(4 3 2 5 4 3 2 5))

(define (selectionSort list)
  (if (null? list)
      '()
      (cons (min list) (selectionSort (deleteOnce (min list) list)))))

(selectionSort '())
(selectionSort '(5))
(selectionSort '(6 10 23 12 2 9 18 1 0 15))
(selectionSort '(3 4 7 3 7 7 4 3 2 3 7))