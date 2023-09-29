#lang racket/base

(module+ test
  (require rackunit))

;; Notice
;; To install (from within the package directory):
;;   $ raco pkg install
;; To install (once uploaded to pkgs.racket-lang.org):
;;   $ raco pkg install <<name>>
;; To uninstall:
;;   $ raco pkg remove <<name>>
;; To view documentation:
;;   $ raco docs <<name>>
;;
;; For your convenience, we have included LICENSE-MIT and LICENSE-APACHE files.
;; If you would prefer to use a different license, replace those files with the
;; desired license.
;;
;; Some users like to add a `private/` directory, place auxiliary files there,
;; and require them in `main.rkt`.
;;
;; See the current version of the racket style guide here:
;; http://docs.racket-lang.org/style/index.html

;; Code here

(require racket/stream (for-syntax racket/base racket/syntax syntax/parse))
(provide define-sieve-box-constructor)

;;Utilities
(begin-for-syntax
  (define ((make-continuations-wrapper pairs) core)
    ;;pair: (cons operator body)
    (foldl (lambda (p i) (list (car p) (cdr p) i)) core pairs)))

;;Transformers
(define-syntax operator/c #'(-> (-> any/c any) stream? stream?))
(define-syntax (define-sieve-box-constructor stx)
  (define-splicing-syntax-class pass
    #:description "Pass in the sieve box"
    (pattern (~seq operator (body:expr ...))
             #:declare operator (expr/c #'operator/c)))
  (define-syntax-class sieve-box-constructor
    #:description "Sieve box constructor"
    (pattern (_ name:id pass:pass ...)
             #:with (operator ...) #'(pass.operator ...)
             #:with ((body ...) ...) #'((pass.body ...) ...)))
  
  (syntax-parse stx
    (form:sieve-box-constructor
     (with-syntax
         ((stm (cadr (syntax-e (syntax-local-introduce (datum->syntax #f (list #'form (generate-temporary 'stream))))))) ;;Add the scope introduced when the syntax object is passed into the transformer
          ((pass ...) (map (lambda _ (generate-temporary #'pass)) (syntax->list #'(form.operator ...))))) ;;Generate names for passes
       (let ((wrapper (make-continuations-wrapper (map cons (syntax->list #'(form.operator ...)) (syntax->list #'(pass ...))))))
         #`(define (form.name stm)
             (let* #,(syntax-local-introduce
                      ;;Identifiers `CURRENT` and `OTHER` are both available in the body of the function
                      ;;Other identifiers are either unstable or inavailable
                      #'((CURRENT (stream-first stm))
                         (pass (lambda (OTHER) form.body ...))
                         ...))
               (stream-cons #:eager CURRENT
                            (form.name #,(wrapper #'(stream-rest stm)))))))))))

(module+ test
  ;; Any code in this `test` submodule runs when this file is run using DrRacket
  ;; or with `raco test`. The code here does not run when this file is
  ;; required by another module.

  (define (standard-ith-prime i)
    (define (prime? n)
      (for/and ((f (in-naturals 2))) #:break (> (* f f) n)
        (not (zero? (remainder n f)))))

    (let loop ((c 0) (s 2))
      (cond ((= i c) (sub1 s))
            ((prime? s) (loop (add1 c) (add1 s)))
            (else (loop c (add1 s))))))

  (define (sieve-ith-prime i)
    (struct integer (n)
      #:methods gen:stream [(define (stream-empty? _) #f)
                            (define (stream-first i) (integer-n i))
                            (define (stream-rest i) (integer (add1 (integer-n i))))])
    
    (define-sieve-box-constructor primes-in
      stream-filter
      ((not (zero? (remainder OTHER CURRENT)))))
    
    (stream-ref (primes-in (integer 2)) (sub1 i)))
  
  (check-true (= 233 (standard-ith-prime 51) (sieve-ith-prime 51)))

  (define i (random 1000 2000))
  (displayln (format "Standard method: ~a" i))
  (define result1 (time (standard-ith-prime i)))
  (displayln (format "Sieve's approach: ~a" i))
  (define result2 (time (sieve-ith-prime i)))

  (check-true (= result1 result2)))
