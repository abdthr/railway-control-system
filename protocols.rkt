#lang racket
(require racket/class)
(provide Train<%> Switch<%> Signal<%> Crossing<%>)

(define Train<%>
  (interface ()
    get-id get-speed get-current-block get-next-block get-direction
    set-speed set-position))

(define Switch<%>
  (interface () get-id get-position set-position))

(define Signal<%>
  (interface () get-id get-code set-code))

(define Crossing<%>
  (interface () get-id get-state open! close!))
