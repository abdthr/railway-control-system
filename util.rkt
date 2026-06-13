#lang racket
(provide ensure-symbol h-add h-del fresh-id symbols->strings clear-panel!
         list-index sort-switch-ids entity-field train-current-block train-next-block
         train-direction train-speed switch-position signal-code crossing-state
         train->sexpr switch->sexpr signal->sexpr crossing->sexpr section-value record-value)


(define (ensure-symbol who x)
  (unless (symbol? x)
    (error who "id must be a symbol, got: ~a" x)))

(define (h-add h k v) (hash-set h k v))
(define (h-del h k)   (hash-remove h k))

(define (fresh-id prefix ids)
  (let loop ([n 1])
    (define id (string->symbol (format "~a~a" prefix n)))
    (if (member id ids)
        (loop (add1 n))
        id)))

(define (symbols->strings lst)
  (map symbol->string lst))

(define (clear-panel! panel)
  (for ([child (send panel get-children)])
    (send panel delete-child child)))

(define (list-index pred lst)
  (let loop ((lst lst)
             (i 0))
    (cond
      ((null? lst) #f)
      ((pred (car lst)) i)
      (else (loop (cdr lst) (+ i 1))))))

(define (switch-id->number sid)
  (string->number
   (substring (symbol->string sid) 2)))

(define (sort-switch-ids ids)
  (sort ids < #:key switch-id->number))

;; ============================================================
;;  ENTITY FIELD HELPERS
;; ============================================================

(define (entity-field entity key)
  (define p (assoc key (cdr entity)))
  (and p (cadr p)))

(define (train-current-block tr)
  (entity-field tr 'current-block))

(define (train-next-block tr)
  (entity-field tr 'next-block))

(define (train-direction tr)
  (entity-field tr 'direction))

(define (train-speed tr)
  (entity-field tr 'speed))

(define (switch-position sw)
  (entity-field sw 'position))

(define (signal-code sig)
  (entity-field sig 'code))

(define (crossing-state cr)
  (entity-field cr 'state))

;; ==============================
;;  Object -> s-expr helpers
;; ==============================

(define (train->sexpr tr)
  (and tr
       (list 'train
             (list 'id (send tr get-id))
             (list 'speed (send tr get-speed))
             (list 'current-block (send tr get-current-block))
             (list 'next-block (send tr get-next-block))
             (list 'direction (send tr get-direction)))))

(define (switch->sexpr sw)
  (and sw
       (list 'switch
             (list 'id (send sw get-id))
             (list 'position (send sw get-position)))))

(define (signal->sexpr sg)
  (and sg
       (list 'signal
             (list 'id (send sg get-id))
             (list 'code (send sg get-code)))))

(define (crossing->sexpr cr)
  (and cr
       (list 'crossing
             (list 'id (send cr get-id))
             (list 'state (send cr get-state)))))

;; =========================================
;; SCENARIO HELPERS
;; =========================================

(define (section-value sexpr key [default #f])
  (define p (assoc key (cdr sexpr)))
  (if p (cadr p) default))

(define (record-value rec key [default #f])
  (define p (assoc key (cdr rec)))
  (if p (cadr p) default))



