#lang racket

(provide
  DEFAULT-TCP-HOST
  DEFAULT-TCP-PORT
  ok-msg
  error-msg)

(define DEFAULT-TCP-HOST "127.0.0.1")

(define DEFAULT-TCP-PORT 45678)

(define (ok-msg . xs)
  (cons 'ok xs))

(define (error-msg msg)
  (list 'error msg))