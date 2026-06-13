#lang racket

(require "nmbs-client.rkt"
         "util.rkt")

(provide
 
 nmbs-init!
 
 ;; connection
 nmbs-connect!
 nmbs-disconnect!
 nmbs-connected?

 ;; backend
 nmbs-get-backend
 nmbs-set-backend!

 ;; scenario
 nmbs-get-scenario
 nmbs-apply-scenario!
 nmbs-save-scenario!
 nmbs-load-scenario!
 
 ;; id helpers
 nmbs-fresh-switch-id
 nmbs-fresh-signal-id
 nmbs-fresh-crossing-id
 
 ;; queries
 nmbs-train-ids
 nmbs-get-train
 
 nmbs-switch-ids
 nmbs-get-switch
 
 nmbs-signal-ids
 nmbs-get-signal
 
 nmbs-crossing-ids
 nmbs-get-crossing

 nmbs-detection-ids
 nmbs-occupied-detection-ids
 nmbs-reservations

 nmbs-get-state
 
 ;; high-level commands for GUI
 nmbs-create-train!
 nmbs-remove-train!
 nmbs-set-train-speed!
 nmbs-set-train-direction!
 
 nmbs-create-default-switch!
 nmbs-remove-switch!
 nmbs-set-switch-position!
 
 nmbs-create-default-signal!
 nmbs-remove-signal!
 nmbs-set-signal-code!
 
 nmbs-create-default-crossing!
 nmbs-remove-crossing!
 nmbs-open-crossing!
 nmbs-close-crossing!

 nmbs-drive-train!)

;; ==============================
;;  Defaults
;; ==============================

(define DEFAULT-PREV-SEG '1-4)
(define DEFAULT-CURR-SEG '1-3)

;; ==============================
;;  Small helper
;; ==============================

(define (rpc msg)
  (define response (nmbs-send-request msg))
  (cond
    [(and (pair? response) (eq? (car response) 'error))
     (error 'nmbs (format "~a" response))]
    [else
     response]))

;; ==============================
;;  Init
;; ==============================

(define (nmbs-init!)
  (rpc '(nmbs-init!)))

;; ==============================
;;  Backend (simulator / hardware)
;; ==============================


(define (nmbs-get-backend)
  (rpc '(get-backend)))

(define (nmbs-set-backend! mode)
  (unless (memq mode '(simulator hardware))
    (error 'nmbs-set-backend! "mode must be 'simulator or 'hardware, got ~a" mode))
  (rpc (list 'set-backend mode)))

;; ==============================
;;  Scencario
;; ==============================

(define (nmbs-get-scenario)
  (rpc '(get-scenario)))

(define (nmbs-apply-scenario! scenario)
  (rpc (list 'apply-scenario scenario)))

(define (nmbs-save-scenario! path)
  (define scenario (nmbs-get-scenario))
  (call-with-output-file path
    (lambda (out)
      (write scenario out)
      (newline out))
    #:exists 'replace)
  '(ok))

(define (nmbs-load-scenario! path)
  (define scenario
    (call-with-input-file path
      (lambda (in)
        (define v (read in))
        (if (eof-object? v)
            (error 'nmbs-load-scenario! "empty scenario file")
            v))))
  (nmbs-apply-scenario! scenario))

;; ==============================
;;  ID-helpers
;; ==============================

(define (nmbs-fresh-switch-id)
  (fresh-id "S-" (nmbs-switch-ids)))

(define (nmbs-fresh-signal-id)
  (fresh-id "L-" (nmbs-signal-ids)))

(define (nmbs-fresh-crossing-id)
  (fresh-id "C-" (nmbs-crossing-ids)))

;; ==============================
;;  Queries
;; ==============================

(define (nmbs-train-ids)
  (rpc '(nmbs-train-ids)))

(define (nmbs-get-train id)
  (rpc (list 'nmbs-get-train id)))

(define (nmbs-switch-ids)
  (rpc '(nmbs-switch-ids)))

(define (nmbs-get-switch id)
  (rpc (list 'nmbs-get-switch id)))

(define (nmbs-signal-ids)
  (rpc '(nmbs-signal-ids)))

(define (nmbs-get-signal id)
  (rpc (list 'nmbs-get-signal id)))

(define (nmbs-crossing-ids)
  (rpc '(nmbs-crossing-ids)))

(define (nmbs-get-crossing id)
  (rpc (list 'nmbs-get-crossing id)))

(define (nmbs-detection-ids)
  (rpc '(nmbs-detection-ids)))

(define (nmbs-occupied-detection-ids)
  (rpc '(nmbs-occupied-detection-ids)))

(define (nmbs-reservations)
  (rpc '(nmbs-reservations)))

(define (nmbs-get-state)
  (rpc '(get-state)))

;; ==============================
;;  Commands – trains
;; ==============================

(define (train-number->id number)
  (unless (and (integer? number) (positive? number))
    (error 'nmbs-create-train! "treinnummer moet een positief geheel getal zijn, kreeg: ~a" number))
  (string->symbol (format "T-~a" number)))

(define (nmbs-create-train! number current-block next-block)
  (define new-id (train-number->id number))
  (when (member new-id (nmbs-train-ids))
    (error 'nmbs-create-train! "trein ~a bestaat al" new-id))
  (rpc (list 'nmbs-create-train-at! new-id current-block next-block))
  new-id)

(define (nmbs-remove-train! id)
  (rpc (list 'nmbs-remove-train! id)))

(define (nmbs-set-train-speed! id speed)
  (rpc (list 'nmbs-set-train-speed! id speed)))

(define (nmbs-set-train-direction! id direction)
  (rpc (list 'nmbs-set-train-direction! id direction)))

;; ==============================
;;  Commands – switches
;; ==============================

(define (nmbs-create-default-switch!)
  (define new-id (nmbs-fresh-switch-id))
  (rpc (list 'nmbs-create-default-switch! new-id))
  new-id)

(define (nmbs-remove-switch! id)
  (rpc (list 'nmbs-remove-switch! id)))

(define (nmbs-set-switch-position! id pos)
  (rpc (list 'nmbs-set-switch-position! id pos)))

;; ==============================
;;  Commands – signals
;; ==============================

(define (nmbs-create-default-signal!)
  (define new-id (nmbs-fresh-signal-id))
  (rpc (list 'nmbs-create-default-signal! new-id))
  new-id)

(define (nmbs-remove-signal! id)
  (rpc (list 'nmbs-remove-signal! id)))

(define (nmbs-set-signal-code! id code)
  (rpc (list 'nmbs-set-signal-code! id code)))

;; ==============================
;;  Commands – crossings
;; ==============================

(define (nmbs-create-default-crossing!)
  (define new-id (nmbs-fresh-crossing-id))
  (rpc (list 'nmbs-create-default-crossing! new-id))
  new-id)

(define (nmbs-remove-crossing! id)
  (rpc (list 'nmbs-remove-crossing! id)))

(define (nmbs-open-crossing! id)
  (rpc (list 'nmbs-open-crossing! id)))

(define (nmbs-close-crossing! id)
  (rpc (list 'nmbs-close-crossing! id)))

;; ==============================
;;  Routing
;; ==============================

(define (nmbs-drive-train! id destination)
  (rpc (list 'nmbs-drive-train! id destination)))