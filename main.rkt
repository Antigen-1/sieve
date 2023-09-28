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

(define-syntax (define-sieve-box-constructor stx)
  (define-syntax-class sieve-box-constructor
    #:description "Sieve box constructor"
    (pattern (_ name:id body:expr ...)))
  
  (syntax-parse stx
    (form:sieve-box-constructor
     (with-syntax
         ;;Add the scope introduced when the syntax object is passed into the transformer
         ((stm (cadr (syntax-e (syntax-local-introduce (datum->syntax #f (list #'form (generate-temporary 'stream))))))))
       #`(define (form.name stm)
           (let* #,(syntax-local-introduce
                    ;;Identifiers `current-value` and `other-item` are both available in the body of the filter
                    #'((current-value (stream-first stm)) (new-filter (lambda (other-item) form.body ...))))
             (stream-cons #:eager current-value
                          (form.name (stream-filter new-filter (stream-rest stm))))))))))

(module+ test
  ;; Any code in this `test` submodule runs when this file is run using DrRacket
  ;; or with `raco test`. The code here does not run when this file is
  ;; required by another module.

  (struct integer (n)
    #:methods gen:stream [(define (stream-empty? _) #f)
                          (define (stream-first i) (integer-n i))
                          (define (stream-rest i) (integer (add1 (integer-n i))))])
  (define-sieve-box-constructor primes-in
    (not (zero? (remainder other-item current-value))))
  (check-true (= 233 (time (stream-ref (primes-in (integer 2)) 50)))))
