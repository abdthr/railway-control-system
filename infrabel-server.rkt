#lang racket

(require racket/tcp
         racket/class
         "tcp-protocol.rkt"
         "infrabel-core.rkt"
         "util.rkt")

(provide start-infrabel-server!)


;; ==============================
;;  Build full state snapshot
;; ==============================

(define (build-state)
  (define trains
    (map (lambda (id)
           (train->sexpr (infrabel-get-train id)))
         (infrabel-train-ids)))

  (define switches
    (map (lambda (id)
           (switch->sexpr (infrabel-get-switch id)))
         (infrabel-switch-ids)))

  (define signals
    (map (lambda (id)
           (signal->sexpr (infrabel-get-signal id)))
         (infrabel-signal-ids)))

  (define crossings
    (map (lambda (id)
           (crossing->sexpr (infrabel-get-crossing id)))
         (infrabel-crossing-ids)))

  (list 'state
        (list 'trains trains)
        (list 'switches switches)
        (list 'signals signals)
        (list 'crossings crossings)
        (list 'detection-ids (infrabel-detection-ids))
        (list 'occupied-detection-ids (infrabel-occupied-detection-ids))
        (list 'reservations (infrabel-reservations))))

;; ==============================
;;  Init
;; ==============================

(define (server-init!)
  (infrabel-load-infrastructure-from-backend!))

;; ==============================
;;  Helpers
;; ==============================

(define (safe-read in)
  (with-handlers ([exn:fail? (lambda (_e) eof)])
    (read in)))

(define (safe-handle thunk)
  (with-handlers ([exn:fail?
                   (lambda (e)
                     (list 'error (exn-message e)))])
    (thunk)))

;; ==============================
;;  Dispatcher
;; ==============================

(define (handle-message msg)
  (safe-handle
   (lambda ()
     (cond
       ;; ==============================
       ;;  Init / state
       ;; ==============================

       [(equal? msg '(nmbs-init!))
        (server-init!)
        '(ok)]

       [(equal? msg '(get-state))
        (build-state)]

       ;; ==============================
       ;;  Backend (simulator / hardware)
       ;; ==============================

       [(equal? msg '(get-backend))
        (infrabel-get-backend)]

       [(and (pair? msg) (eq? (car msg) 'set-backend))
        (infrabel-switch-backend! (cadr msg))
        (list 'ok (infrabel-get-backend))]

       ;; ==============================
       ;;  Queries - Scenario
       ;; ==============================

       [(equal? msg '(get-scenario))
        (infrabel-export-scenario)]

       [(and (pair? msg) (eq? (car msg) 'apply-scenario))
        (infrabel-apply-scenario! (cadr msg))]

       ;; ==============================
       ;;  Queries – trains
       ;; ==============================

       [(equal? msg '(nmbs-train-ids))
        (infrabel-train-ids)]

       [(and (pair? msg) (eq? (car msg) 'nmbs-get-train))
        (train->sexpr (infrabel-get-train (cadr msg)))]

       ;; ==============================
       ;;  Queries – switches
       ;; ==============================

       [(equal? msg '(nmbs-switch-ids))
        (infrabel-switch-ids)]

       [(and (pair? msg) (eq? (car msg) 'nmbs-get-switch))
        (switch->sexpr (infrabel-get-switch (cadr msg)))]

       ;; ==============================
       ;;  Queries – signals
       ;; ==============================

       [(equal? msg '(nmbs-signal-ids))
        (infrabel-signal-ids)]

       [(and (pair? msg) (eq? (car msg) 'nmbs-get-signal))
        (signal->sexpr (infrabel-get-signal (cadr msg)))]

       ;; ==============================
       ;;  Queries – crossings
       ;; ==============================

       [(equal? msg '(nmbs-crossing-ids))
        (infrabel-crossing-ids)]

       [(and (pair? msg) (eq? (car msg) 'nmbs-get-crossing))
        (crossing->sexpr (infrabel-get-crossing (cadr msg)))]

       ;; ==============================
       ;;  Queries – detections
       ;; ==============================

       [(equal? msg '(nmbs-detection-ids))
        (infrabel-detection-ids)]

       [(equal? msg '(nmbs-occupied-detection-ids))
        (infrabel-occupied-detection-ids)]

       [(equal? msg '(nmbs-reservations))
        (infrabel-reservations)]

       ;; ==============================
       ;;  Commands – trains
       ;; ==============================

       [(and (pair? msg) (eq? (car msg) 'nmbs-create-train-at!))
        (define new-id       (cadr msg))
        (define curr-block   (caddr msg))
        (define prev-block   (cadddr msg))
        (infrabel-add-train! new-id
                             #:speed 0
                             #:current-block curr-block
                             #:next-block prev-block
                             #:direction 'forward)
        new-id]

       [(and (pair? msg) (eq? (car msg) 'nmbs-remove-train!))
        (infrabel-remove-train! (cadr msg))
        '(ok)]

       [(and (pair? msg) (eq? (car msg) 'nmbs-set-train-speed!))
        (infrabel-set-train-speed! (cadr msg) (caddr msg))
        '(ok)]

       [(and (pair? msg) (eq? (car msg) 'nmbs-set-train-direction!))
        (infrabel-set-train-direction! (cadr msg) (caddr msg))
        '(ok)]

       ;; ==============================
       ;;  Commands – switches
       ;; ==============================

       [(and (pair? msg) (eq? (car msg) 'nmbs-create-default-switch!))
        (define new-id (cadr msg))
        (infrabel-put-switch! new-id #:position 1)
        new-id]

       [(and (pair? msg) (eq? (car msg) 'nmbs-remove-switch!))
        (infrabel-remove-switch! (cadr msg))
        '(ok)]

       [(and (pair? msg) (eq? (car msg) 'nmbs-set-switch-position!))
        (infrabel-set-switch-position! (cadr msg) (caddr msg))
        '(ok)]

       ;; ==============================
       ;;  Commands – signals
       ;; ==============================

       [(and (pair? msg) (eq? (car msg) 'nmbs-create-default-signal!))
        (define new-id (cadr msg))
        (infrabel-put-signal! new-id #:code 'Hp0)
        new-id]

       [(and (pair? msg) (eq? (car msg) 'nmbs-remove-signal!))
        (infrabel-remove-signal! (cadr msg))
        '(ok)]

       [(and (pair? msg) (eq? (car msg) 'nmbs-set-signal-code!))
        (infrabel-set-signal-code! (cadr msg) (caddr msg))
        '(ok)]

       ;; ==============================
       ;;  Commands – crossings
       ;; ==============================

       [(and (pair? msg) (eq? (car msg) 'nmbs-create-default-crossing!))
        (define new-id (cadr msg))
        (infrabel-put-crossing! new-id #:state 'closed)
        new-id]

       [(and (pair? msg) (eq? (car msg) 'nmbs-remove-crossing!))
        (infrabel-remove-crossing! (cadr msg))
        '(ok)]

       [(and (pair? msg) (eq? (car msg) 'nmbs-open-crossing!))
        (infrabel-open-crossing! (cadr msg))
        '(ok)]

       [(and (pair? msg) (eq? (car msg) 'nmbs-close-crossing!))
        (infrabel-close-crossing! (cadr msg))
        '(ok)]

       [(and (pair? msg) (eq? (car msg) 'nmbs-drive-train!))
        (infrabel-drive-train! (cadr msg) (caddr msg))]

       [else
        (list 'error "unknown message" msg)]))))

;; ==============================
;;  Client loop
;; ==============================

(define (handle-client in out)
  (let loop ()
    (define msg (safe-read in))
    (unless (eof-object? msg)
      (define response (handle-message msg))
      (write response out)
      (newline out)
      (flush-output out)
      (loop))))
;; ==============================
;;  Start server
;; ==============================

(define (start-infrabel-server! [port DEFAULT-TCP-PORT])
  (define listener (tcp-listen port 4 #t))
  (displayln (format "Infrabel server listening on port ~a" port))

  (thread
   (lambda ()
     (let accept-loop ()
       (define-values (in out) (tcp-accept listener))
       (displayln "Client connected")
       (thread (lambda () (handle-client in out)))
       (accept-loop))))

  listener)

(start-infrabel-server!)

