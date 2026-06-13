#lang racket
(require racket/class "util.rkt" "protocols.rkt")
(provide train%)
(define train%
  (class* object% (Train<%>)
    (init-field id
                [speed 0]
                [current-block #f]
                [next-block #f]
                [direction 'forward])
    (super-new)
    (begin ; check if given values validate
      (ensure-symbol 'train% id)
      (unless (and (number? speed) (>= speed 0))
        (error 'train% "speed >= 0, got ~a" speed))
      (unless (or (not current-block) (symbol? current-block))
        (error 'train% "current-block must be symbol or #f"))
      (unless (or (not next-block) (symbol? next-block))
        (error 'train% "next-block must be symbol or #f"))
      (unless (memq direction '(forward backward))
        (error 'train% "direction must be 'forward or 'backward")))
    (field [spd speed]
           [cb current-block]
           [nb next-block]
           [dir direction])
    (define/public (get-id) id)
    (define/public (get-speed) spd)
    (define/public (get-current-block) cb)
    (define/public (get-next-block) nb)
    (define/public (get-direction) dir)
    (define/public (set-speed v)
      (unless (and (number? v) (>= v 0)) (error 'set-speed "speed >= 0, got ~a" v))
      (set! spd v))
    (define/public (set-position #:current-block [new-cb cb]
                                 #:next-block    [new-nb nb]
                                 #:direction     [new-dir dir])
      (unless (or (not new-cb) (symbol? new-cb)) (error 'set-position "current-block must be symbol or #f"))
      (unless (or (not new-nb) (symbol? new-nb)) (error 'set-position "next-block must be symbol or #f"))
      (unless (memq new-dir '(forward backward)) (error 'set-position "direction must be 'forward or 'backward"))
      (set! cb new-cb) (set! nb new-nb) (set! dir new-dir))))


; TEST
; (define t (new train% [id 'T1] [speed 50] [current-block 'B1] [next-block 'B2] [direction 'forward]))
; (send t set-speed 120)