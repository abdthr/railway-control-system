#lang racket
(require racket/class "protocols.rkt" "util.rkt" )
(provide signal% SIGNAL-CODES valid-signal-code?)
(define signal%
  (class* object% (Signal<%>)
    (init-field id [code 'Hp0])
    (super-new)
    (ensure-symbol 'signal% id)
    (unless (valid-signal-code? code)
      (error 'signal% "unknown signal code ~a" code))
    (field [cd code])
    (define/public (get-id) id)
    (define/public (get-code) cd)
    (define/public (set-code c)
      (unless (valid-signal-code? c) (error 'set-code "unknown signal code ~a" c))
      (set! cd c))))

(define SIGNAL-CODES '(Hp0 Hp1 Hp0+Sh0 Ks1+Zs3 Ks2 Ks2+Zs3 Sh1 Ks1+Zs3+Zs3v))

(define (valid-signal-code? c) (and (symbol? c) (memq c SIGNAL-CODES)))

