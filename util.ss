#lang scheme
(require (only-in srfi/26 cut)
         (only-in srfi/60 bit-field))

;; Return a list consisting of all elements from clist with indexes from is.
(define (list-refs clist . is)
  (map (cut list-ref clist <>) is))

;; Return a bit generator using bits from number.
(define (make-bit-stream number)
  (let ((position 0))
    (lambda (count)
      (let ((old-position position))
        (set! position (+ count position))
        (bit-field number old-position position)))))

(define (degrees->radians degrees)
  (* pi (/ (remainder degrees 360) 180)))

;; Covert file extension into an image kind appropriate to pass into save-file
;; method of a bitmap% instance.
(define (file-extension->kind extension (default 'png))
  (case extension
    [("png") 'png]
    [("jpg" "jpeg") 'jpg]
    [("xbm") 'xbm]
    [("xpm") 'xpm]
    [("bmp") 'bmp]
    [else default]))

;; Contract that checks if procedure is of arity one.
(define arity-one-procedure?
  (flat-contract (cut procedure-arity-includes? <> 1)))

(provide/contract
 [list-refs ((list?) () #:rest (listof integer?) . ->* . list?)]
 [make-bit-stream (integer? . -> . (and/c procedure? arity-one-procedure?))]
 [degrees->radians (number? . -> . number?)]
 [file-extension->kind (string? . -> . symbol?)]
 [arity-one-procedure? contract?])
