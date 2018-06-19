
; *********************************************
; *  314 Principles of Programming Languages  *
; *  Spring 2017                              *
; *  Student Version                          *
; *********************************************


;; contains "ctv", "A", and "reduce" definitions
(load "include.ss")

;; contains simple dictionary definition
(load "test-dictionary.ss")

;; -----------------------------------------------------
;; HELPER FUNCTIONS

;; *** CODE FOR ANY HELPER FUNCTION GOES HERE ***


;;creates the bitvector
(define createHash
  (lambda (hashfunctionlist dict)
    (cond ((null? dict) '())(else (append (hashWord hashfunctionlist (car dict)) (createHash hashfunctionlist (cdr dict)))))
))

;;scans the bitvector and removed duplicate values 
(define noDup
  (lambda (bitvector)
    (cond ((null? bitvector) '())((member (car bitvector) (cdr bitvector)) (noDup (cdr bitvector)))(else (cons (car bitvector) (noDup (cdr bitvector)))))
))

;;checks the bitvector if word is spelled correctly
(define verify
  (lambda (hashfunctionlist w bitvector)
    (cond ((null? hashfunctionlist) #t)((and (check bitvector (getProp (car hashfunctionlist) w)) (verify (cdr hashfunctionlist) w bitvector)) #t)(else #f))
))

;;creates hash values for the users word input which will be verified for correctness
(define hashWord
  (lambda (hashfunctionlist w)
    (cond ((null? hashfunctionlist) '())(else (cons(getProp (car hashfunctionlist) w) (hashWord (cdr hashfunctionlist) w))))
))

;;check if the boolean value objects are equal 
(define hasObject
  (lambda (firstOb secondOb)(cond ((eq? firstOb secondOb) #t)((or (eq? firstOb #t) (eq? secondOb #t)) #t)(else secondOb))
))

;;checks if a hashvalue exists in bitvector
(define check
  (lambda (bitvector hashValue)
    (define ver (reduce hasObject bitvector hashValue))(cond((boolean? ver) #t) (else #f))
))

;;gets the exact proper value of a hashvalue 
(define getProp
  (lambda(hashFunc w)
    (inexact->exact(hashFunc w))))
;; -----------------------------------------------------
;; KEY FUNCTION

(define key
  (lambda (w)
    (if (null? w) 5187
        (+ (* 29 (key (cdr w))) (ctv (car w))))
))

;; -----------------------------------------------------
;; EXAMPLE KEY VALUES
;;   (key '(h e l l o))       = 106402241991
;;   (key '(m a y))           = 126526810
;;   (key '(t r e e f r o g)) = 2594908189083745

;; -----------------------------------------------------
;; HASH FUNCTION GENERATORS

;; value of parameter "size" should be a prime number
(define gen-hash-division-method
  (lambda (size) ;; range of values: 0..size-1
    (lambda (w)
      (modulo (key w) size))
))

;; value of parameter "size" is not critical
;; Note: hash functions may return integer values in "real"
;;       format, e.g., 17.0 for 17
(define gen-hash-multiplication-method
  (lambda (size) ;; range of values: 0..size-1
    (lambda (w)
    (floor (* size (- (* (key w) 0.6780219887) (floor (* (key w) 0.6780219887)) ) )))
))


;; -----------------------------------------------------
;; EXAMPLE HASH FUNCTIONS AND HASH FUNCTION LISTS

(define hash-1 (gen-hash-division-method 70111))
(define hash-2 (gen-hash-division-method 89997))
(define hash-3 (gen-hash-multiplication-method 7224))
(define hash-4 (gen-hash-multiplication-method 900))

(define hashfl-1 (list hash-1 hash-2 hash-3 hash-4))
(define hashfl-2 (list hash-1 hash-3))
(define hashfl-3 (list hash-2 hash-3))

;; -----------------------------------------------------
;; EXAMPLE HASH VALUES
;;   to test your hash function implementation
;;
;;  (hash-1 '(h e l l o))       ==> 35616
;;  (hash-1 '(m a y))           ==> 46566
;;  (hash-1 '(t r e e f r o g)) ==> 48238
;;
;;  (hash-2 '(h e l l o))       ==> 48849
;;  (hash-2 '(m a y))           ==> 81025
;;  (hash-2 '(t r e e f r o g)) ==> 16708
;;
;;  (hash-3 '(h e l l o))       ==> 6331.0
;;  (hash-3 '(m a y))           ==> 2456.0
;;  (hash-3 '(t r e e f r o g)) ==> 1806.0
;;
;;  (hash-4 '(h e l l o))       ==> 788.0
;;  (hash-4 '(m a y))           ==> 306.0
;;  (hash-4 '(t r e e f r o g)) ==> 225.0


;; -----------------------------------------------------
;; SPELL CHECKER GENERATOR
 
(define gen-checker
  (lambda (hashfunctionlist dict)
    (lambda (w)
      (define bitvector (noDup (createHash hashfunctionlist dict)))
      (verify hashfunctionlist w bitvector)
)))

;; -----------------------------------------------------
;; EXAMPLE SPELL CHECKERS

(define checker-1 (gen-checker hashfl-1 dictionary))
(define checker-2 (gen-checker hashfl-2 dictionary))
(define checker-3 (gen-checker hashfl-3 dictionary))

;; EXAMPLE APPLICATIONS OF A SPELL CHECKER
;;
;;  (checker-1 '(a r g g g g)) ==> #f
;;  (checker-2 '(h e l l o)) ==> #t
;;  (checker-2 '(a r g g g g)) ==> #t  // false positive

(checker-1 '(a r g g g g))
(checker-2 '(h e l l o))
