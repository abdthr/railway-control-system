#lang racket
(require racket/class racket/match
         "util.rkt" "protocols.rkt"
         "train.rkt" "switch.rkt" "signal.rkt" "crossing.rkt")
(provide railway%)
(define railway%
  (class object%
    (super-new)
    (field [trains     (make-immutable-hash)]
           [switches   (make-immutable-hash)]
           [signals    (make-immutable-hash)]
           [crossings  (make-immutable-hash)])
    
    ; helpers to not allow duplicate values 
    (define/private (ensure-absent tbl id who)
      (when (hash-has-key? tbl id) (error who "~a already exists" id)))
    
    (define/private (ensure-present tbl id who)
      (unless (hash-has-key? tbl id) (error who "unknown ~a" id)))
    
    ;; queries
    (define/public (train-ids)     (hash-keys trains))
    (define/public (get-train id)  (hash-ref trains id #f))
    
    (define/public (switch-ids)    (hash-keys switches))
    (define/public (get-switch id) (hash-ref switches id #f))
    
    (define/public (signal-ids)    (hash-keys signals))
    (define/public (get-signal id) (hash-ref signals id #f))
  
    (define/public (crossing-ids)  (hash-keys crossings))
    
    (define/public (get-crossing id) (hash-ref crossings id #f))
    
    ;; commands
    (define/public (add-train id #:speed [v 0]
                              #:current-block [cb #f]
                              #:next-block [nb #f]
                              #:direction [dir 'forward])
      (ensure-symbol 'add-train id)
      (ensure-absent trains id 'add-train)
      (define t (new train% [id id] [speed v] [current-block cb] [next-block nb] [direction dir]))
      (set! trains (h-add trains id t)))

    (define/public (remove-train id)
      (ensure-present trains id 'remove-train)
      (set! trains (h-del trains id)))
    
    (define/public (set-train-speed! id v)
      (ensure-present trains id 'set-train-speed)
      (send (hash-ref trains id) set-speed v))
    
    (define/public (put-switch id #:position [pos 1])
      (ensure-symbol 'put-switch id)
      (ensure-absent switches id 'put-switch)
      (set! switches (h-add switches id (new switch% [id id] [position pos]))))

    (define/public (remove-switch id)
      (ensure-present switches id 'remove-switch)
      (set! switches (h-del switches id)))
    
    (define/public (set-switch-position! id pos)
      (ensure-present switches id 'set-switch-position)
      (send (hash-ref switches id) set-position pos))
    
    (define/public (put-signal id #:code [c 'Hp0])
      (ensure-symbol 'put-signal id)
      (ensure-absent signals id 'put-signal)
      (set! signals (h-add signals id (new signal% [id id] [code c]))))

    (define/public (remove-signal id)
      (ensure-present signals id 'remove-signal)
      (set! signals (h-del signals id)))
    
    (define/public (set-signal-code! id code)
      (ensure-present signals id 'set-signal-code)
      (send (hash-ref signals id) set-code code))
    
    (define/public (put-crossing id #:state [st 'open])
      (ensure-symbol 'put-crossing id)
      (ensure-absent crossings id 'put-crossing)
      (set! crossings (h-add crossings id (new crossing% [id id] [state st]))))

    (define/public (remove-crossing id)
      (ensure-present crossings id 'remove-crossing)
      (set! crossings (h-del crossings id)))
    
    (define/public (open-crossing id)
      (ensure-present crossings id 'open-crossing)
      (send (hash-ref crossings id) open!))
    
    (define/public (close-crossing id)
      (ensure-present crossings id 'close-crossing)
      (send (hash-ref crossings id) close!))))
