#lang racket

(require racket/tcp
         "tcp-protocol.rkt")

(provide
  nmbs-connect!
  nmbs-disconnect!
  nmbs-connected?
  nmbs-send-request)

(define current-in #f)
(define current-out #f)

(define (nmbs-connected?)
  (and current-in current-out))

(define (nmbs-connect! [host DEFAULT-TCP-HOST] [port DEFAULT-TCP-PORT])
  (unless (nmbs-connected?)
    (define-values (in out) (tcp-connect host port))
    (set! current-in in)
    (set! current-out out))
  'ok)

(define (nmbs-disconnect!)
  (when current-in
    (close-input-port current-in)
    (set! current-in #f))
  (when current-out
    (close-output-port current-out)
    (set! current-out #f))
  'ok)

(define (nmbs-send-request msg)
  (unless (nmbs-connected?)
    (error 'nmbs-send-request "NMBS is not connected to Infrabel"))
  (write msg current-out)
  (newline current-out)
  (flush-output current-out)
  (read current-in))