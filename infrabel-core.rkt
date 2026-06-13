#lang racket

(require racket/class
         racket/list
         racket/set
         "railway.rkt"
         "util.rkt"
         "routing.rkt"
         (prefix-in sim: "simulator/interface.rkt")
         (prefix-in hw:  "hardware-library/interface.rkt"))


(provide
 infrabel-reset!
 ;; queries
 infrabel-train-ids
 infrabel-get-train
 infrabel-switch-ids
 infrabel-backend-switch-ids
 infrabel-get-switch
 infrabel-signal-ids
 infrabel-get-signal
 infrabel-crossing-ids
 infrabel-get-crossing
 infrabel-detection-ids
 infrabel-occupied-detection-ids
 ;; commands
 infrabel-add-train!
 infrabel-remove-train!
 infrabel-set-train-speed!
 infrabel-set-train-direction!
 infrabel-put-switch!
 infrabel-remove-switch!
 infrabel-set-switch-position!
 infrabel-put-signal!
 infrabel-remove-signal!
 infrabel-set-signal-code!
 infrabel-put-crossing!
 infrabel-remove-crossing!
 infrabel-open-crossing!
 infrabel-close-crossing!

 infrabel-load-switches-from-backend!
 infrabel-load-detections-from-backend!
 infrabel-load-infrastructure-from-backend!

 infrabel-set-backend!
 infrabel-set-setup!
 infrabel-switch-backend!
 infrabel-get-backend
 infrabel-get-setup
 infrabel-export-scenario
 infrabel-apply-scenario!

 ;; routing
 infrabel-drive-train!
 infrabel-reservations)

;; =========================================
;; INTERNAL STATE
;; =========================================

(define rw #f)
(define backend-started? #f)
(define position-tracker-thread #f)
(define drive-monitors (make-hash))
(define train-sim-prev (make-hash))
(define reservations (make-hash))

(define current-backend 'simulator)
(define current-setup 'hardware)

;; =========================================
;; BACKEND DISPATCH
;; =========================================

(define (hardware-mode?)
  (eq? current-backend 'hardware))

(define (start)
  (if (hardware-mode?) (hw:start) (sim:start)))

(define (stop)
  (if (hardware-mode?) (hw:stop) (sim:stop)))

(define (add-loco id previous-segment current-segment)
  (if (hardware-mode?)
      (hw:add-loco id previous-segment current-segment)
      (sim:add-loco id previous-segment current-segment)))

(define (remove-loco id)
  (unless (hardware-mode?)
    (sim:remove-loco id)))

(define (get-loco-speed id)
  (if (hardware-mode?) (hw:get-loco-speed id) (sim:get-loco-speed id)))

(define (set-loco-speed! id speed)
  (if (hardware-mode?) (hw:set-loco-speed! id speed) (sim:set-loco-speed! id speed)))

(define (get-occupied-detection-blocks)
  (if (hardware-mode?)
      (hw:get-occupied-detection-blocks)
      (sim:get-occupied-detection-blocks)))

(define (get-detection-block-ids)
  (if (hardware-mode?) (hw:get-detection-block-ids) (sim:get-detection-block-ids)))

(define (get-switch-ids)
  (if (hardware-mode?) (hw:get-switch-ids) (sim:get-switch-ids)))

(define (get-switch-position id)
  (if (hardware-mode?) (hw:get-switch-position id) (sim:get-switch-position id)))

(define (set-switch-position! id position)
  (if (hardware-mode?)
      (hw:set-switch-position! id position)
      (sim:set-switch-position! id position)))

(define (open-crossing! id)
  (if (hardware-mode?) (hw:open-crossing! id) (sim:open-crossing! id)))

(define (close-crossing! id)
  (if (hardware-mode?) (hw:close-crossing! id) (sim:close-crossing! id)))

(define (set-sign-code! id code)
  (if (hardware-mode?) (hw:set-sign-code! id code) (sim:set-sign-code! id code)))

(define (setup-current-backend!)
  (unless (hardware-mode?)
    (case current-setup
      [(hardware)             (sim:setup-hardware)]
      [(straight)             (sim:setup-straight)]
      [(straight-with-switch) (sim:setup-straight-with-switch)]
      [(loop)                 (sim:setup-loop)]
      [(loop-and-switches)    (sim:setup-loop-and-switches)]
      [else
       (error 'setup-current-backend! "unknown setup ~a" current-setup)])))

(define (ensure-rw!)
  (unless rw
    (set! rw (new railway%))))

(define (infrabel-get-backend)
  current-backend)

(define (infrabel-get-setup)
  current-setup)

(define (infrabel-set-backend! backend)
  (unless (memq backend '(hardware simulator))
    (error 'infrabel-set-backend! "unknown backend ~a" backend))
  (when backend-started?
    (stop)
    (set! backend-started? #f))
  (set! current-backend backend)
  (set! current-setup (if (eq? backend 'hardware) 'hardware 'loop))
  backend)

(define (infrabel-set-setup! setup)
  (unless (memq setup '(hardware straight straight-with-switch loop loop-and-switches))
    (error 'infrabel-set-setup! "unknown setup ~a" setup))
  (when backend-started?
    (stop)
    (set! backend-started? #f))
  (set! current-setup setup)
  (set! current-backend (if (eq? setup 'hardware) 'hardware 'simulator))
  setup)


(define (infrabel-switch-backend! mode)
  (unless (memq mode '(hardware simulator))
    (error 'infrabel-switch-backend! "unknown backend ~a" mode))
  (infrabel-reset!)               
  (set! current-backend mode)
  (set! current-setup 'hardware)  
  (infrabel-load-infrastructure-from-backend!)
  mode)

(define (start-backend!)
  (unless backend-started?
    (setup-current-backend!)   
    (start)                    
    (set! backend-started? #t)))

;; =========================================
;; RESERVATIONS
;; =========================================

(define (held-by? element-id train-id)
  (equal? (hash-ref reservations element-id #f) train-id))

(define (held-by-other? element-id train-id)
  (define holder (hash-ref reservations element-id #f))
  (and holder (not (equal? holder train-id))))

(define (reserve-elements! ids train-id)
  (for ([id (in-list ids)])
    (hash-set! reservations id train-id)))

(define (release-element! id train-id)
  (when (held-by? id train-id)
    (hash-remove! reservations id)))

(define (release-all-for-train! train-id)
  (for ([id (in-list (hash-keys reservations))])
    (release-element! id train-id)))

(define (release-train-except! train-id keep-id)
  (for ([id (in-list (hash-keys reservations))])
    (when (and (held-by? id train-id) (not (equal? id keep-id)))
      (hash-remove! reservations id))))

(define (route-conflicts route train-id)
  (filter (lambda (id) (held-by-other? id train-id)) route))

(define (infrabel-reservations)
  (for/list ([id (in-list (hash-keys reservations))])
    (cons id (hash-ref reservations id))))

;; =========================================
;; POSITION TRACKER
;; =========================================
(define (compute-sim-prev new-cb old-cb)
  (define back-path (find-route current-setup new-cb old-cb))
  (if (and back-path (> (length back-path) 1))
      (cadr back-path)
      old-cb))

(define (update-train-positions!)
  (define occupied (infrabel-occupied-detection-ids))
  (define ids      (infrabel-train-ids))
  (define current-blocks
    (map (lambda (id)
           (define t (infrabel-get-train id))
           (and t (send t get-current-block)))
         ids))
  (for ([id (in-list ids)]
        [cb (in-list current-blocks)])
    (when (and cb (not (member cb occupied)))
      (define other-cbs (filter (lambda (b) (not (equal? b cb))) current-blocks))
      (define candidates (filter (lambda (b) (not (member b other-cbs))) occupied))
      (when (pair? candidates)
        (define new-cb (car candidates))
        (define t (infrabel-get-train id))
        (send t set-position
              #:current-block new-cb
              #:next-block    cb
              #:direction     (send t get-direction))
        (hash-set! train-sim-prev id (compute-sim-prev new-cb cb))
        (release-element! cb id)))))

(define (start-position-tracker!)
  (when position-tracker-thread
    (kill-thread position-tracker-thread)
    (set! position-tracker-thread #f))
  (set! position-tracker-thread
        (thread
         (lambda ()
           (let loop ()
             (sleep 0.5)
             (with-handlers ([exn:fail? (lambda (_) (void))])
               (when backend-started?
                 (update-train-positions!)))
             (loop))))))

(define (stop-drive-monitor! id)
  (define t (hash-ref drive-monitors id #f))
  (when t
    (kill-thread t)
    (hash-remove! drive-monitors id)))

(define (infrabel-reset!)
  (set! rw (new railway%))
  (when backend-started?
    (stop)
    (set! backend-started? #f))
  (when position-tracker-thread
    (kill-thread position-tracker-thread)
    (set! position-tracker-thread #f))
  (for ([id (in-list (hash-keys drive-monitors))])
    (stop-drive-monitor! id))
  (hash-clear! train-sim-prev)
  (hash-clear! reservations))

(define (infrabel-load-infrastructure-from-backend!)
  (ensure-rw!)
  (start-backend!)
  (infrabel-load-switches-from-backend!)
  (infrabel-load-detections-from-backend!)
  (start-position-tracker!))

;; =========================================
;; SCENARIO
;; =========================================

(define (infrabel-export-scenario)
  (ensure-rw!)
  (list 'scenario
        (list 'backend current-backend)
        (list 'setup current-setup)
        (list 'trains
              (map (lambda (id)
                     (train->sexpr (send rw get-train id)))
                   (send rw train-ids)))
        (list 'switches
              (map (lambda (id)
                     (switch->sexpr (send rw get-switch id)))
                   (send rw switch-ids)))
        (list 'signals
              (map (lambda (id)
                     (signal->sexpr (send rw get-signal id)))
                   (send rw signal-ids)))
        (list 'crossings
              (map (lambda (id)
                     (crossing->sexpr (send rw get-crossing id)))
                   (send rw crossing-ids)))))

(define (infrabel-apply-scenario! scenario)
  (unless (and (pair? scenario) (eq? (car scenario) 'scenario))
    (error 'infrabel-apply-scenario! "invalid scenario format"))

  (define backend   (section-value scenario 'backend 'simulator))
  (define setup     (section-value scenario 'setup 'loop))
  (define trains    (section-value scenario 'trains '()))
  (define switches  (section-value scenario 'switches '()))
  (define signals   (section-value scenario 'signals '()))
  (define crossings (section-value scenario 'crossings '()))

  (infrabel-reset!)
  (infrabel-set-backend! backend)
  (infrabel-set-setup! setup)

  (ensure-rw!)
  (start-backend!)
  (infrabel-load-switches-from-backend!)

  ;; switches
  (for ([sw (in-list switches)])
    (define id  (record-value sw 'id))
    (define pos (record-value sw 'position 1))
    (define existing (send rw get-switch id))
    (if existing
        (send existing set-position pos)
        (send rw put-switch id #:position pos))
    (with-handlers ([exn:fail? (lambda (_e) (void))])
      (set-switch-position! id pos)))

  ;; signals
  (for ([sg (in-list signals)])
    (define id   (record-value sg 'id))
    (define code (record-value sg 'code 'Hp0))
    (if (send rw get-signal id)
        (infrabel-set-signal-code! id code)
        (infrabel-put-signal! id #:code code)))

  ;; crossings
  (for ([cr (in-list crossings)])
    (define id    (record-value cr 'id))
    (define state (record-value cr 'state 'open))
    (if (send rw get-crossing id)
        (case state
          [(open)   (infrabel-open-crossing! id)]
          [(closed) (infrabel-close-crossing! id)])
        (infrabel-put-crossing! id #:state state)))

  ;; trains
  (for ([tr (in-list trains)])
    (define id        (record-value tr 'id))
    (define speed     (record-value tr 'speed 0))
    (define cb        (record-value tr 'current-block #f))
    (define nb        (record-value tr 'next-block #f))
    (define direction (record-value tr 'direction 'forward))
    (infrabel-add-train! id
                         #:speed speed
                         #:current-block cb
                         #:next-block nb
                         #:direction direction))

  (start-position-tracker!)

  '(ok))

;; =========================================
;; QUERIES
;; =========================================

;; trains
(define (infrabel-train-ids)
  (ensure-rw!)
  (send rw train-ids))

(define (infrabel-get-train id)
  (ensure-rw!)
  (send rw get-train id))

;; switches
(define (infrabel-switch-ids)
  (ensure-rw!)
  (send rw switch-ids))

(define (infrabel-get-switch id)
  (ensure-rw!)
  (send rw get-switch id))

(define (infrabel-backend-switch-ids)
  (start-backend!)
  (get-switch-ids))

;; signals
(define (infrabel-signal-ids)
  (ensure-rw!)
  (send rw signal-ids))

(define (infrabel-get-signal id)
  (ensure-rw!)
  (send rw get-signal id))

;; crossings
(define (infrabel-crossing-ids)
  (ensure-rw!)
  (send rw crossing-ids))

(define (infrabel-get-crossing id)
  (ensure-rw!)
  (send rw get-crossing id))

;; detection
(define (infrabel-detection-ids)
  (start-backend!)
  (get-detection-block-ids))

(define (infrabel-occupied-detection-ids)
  (start-backend!)
  (get-occupied-detection-blocks))

;; =========================================
;; COMMANDS – TRAINS
;; =========================================

(define (infrabel-add-train! id
                             #:speed         [speed 0]
                             #:current-block [current-block #f]
                             #:next-block    [next-block #f]
                             #:direction     [direction 'forward])
  (ensure-rw!)
  (start-backend!)
  (send rw add-train id
        #:speed speed
        #:current-block current-block
        #:next-block next-block
        #:direction direction)
  (when (and current-block next-block)
    (define sim-prev (compute-sim-prev current-block next-block))
    (add-loco id sim-prev current-block)
    (set-loco-speed! id speed)
    (hash-set! train-sim-prev id sim-prev))
  (when current-block
    (hash-set! reservations current-block id))
  id)

(define (infrabel-remove-train! id)
  (ensure-rw!)
  (send rw remove-train id)
  (hash-remove! train-sim-prev id)
  (stop-drive-monitor! id)
  (release-all-for-train! id)
  (with-handlers ([exn:fail? (lambda (_e) (void))])
    (remove-loco id)))

(define (infrabel-set-train-speed! id speed)
  (ensure-rw!)
  (start-backend!)
  (send rw set-train-speed! id speed)
  (with-handlers ([exn:fail? (lambda (_e) (void))])
    (set-loco-speed! id speed)))

(define (infrabel-set-train-direction! id direction)
  (ensure-rw!)
  (define t (send rw get-train id))
  (unless t
    (error 'infrabel-set-train-direction! "unknown train ~a" id))
  (send t set-position
        #:current-block (send t get-current-block)
        #:next-block    (send t get-next-block)
        #:direction     direction)
  (with-handlers ([exn:fail? (lambda (_e) (void))])
    (start-backend!)
    (define cur-speed (get-loco-speed id))
    (define abs-speed (abs cur-speed))
    (define new-speed
      (cond [(eq? direction 'forward)  abs-speed]
            [(eq? direction 'backward) (- abs-speed)]
            [else cur-speed]))
    (set-loco-speed! id new-speed)))

;; =========================================
;; COMMANDS – SWITCHES
;; =========================================

(define (infrabel-put-switch! id #:position [position 1])
  (ensure-rw!)
  (send rw put-switch id #:position position)
  (with-handlers ([exn:fail? (lambda (_e) (void))])
    (set-switch-position! id position))
  id)

(define (infrabel-remove-switch! id)
  (ensure-rw!)
  (send rw remove-switch id))

(define (infrabel-set-switch-position! id position)
  (ensure-rw!)
  (send rw set-switch-position! id position)
  (with-handlers ([exn:fail? (lambda (_e) (void))])
    (set-switch-position! id position)))

(define (infrabel-load-switches-from-backend!)
  (ensure-rw!)
  (start-backend!)
  (for ([sid (in-list (get-switch-ids))])
    (define pos
      (with-handlers ([exn:fail? (lambda (_e) 1)])
        (get-switch-position sid)))
    (define sw (send rw get-switch sid))
    (if sw
        (send sw set-position pos)
        (send rw put-switch sid #:position pos))))

;; =========================================
;; COMMANDS – SIGNALS
;; =========================================

(define (infrabel-put-signal! id #:code [code 'Hp0])
  (ensure-rw!)
  (send rw put-signal id #:code code)
  (with-handlers ([exn:fail? (lambda (_e) (void))])
    (set-sign-code! id code))
  id)

(define (infrabel-remove-signal! id)
  (ensure-rw!)
  (send rw remove-signal id))

(define (infrabel-set-signal-code! id code)
  (ensure-rw!)
  (send rw set-signal-code! id code)
  (with-handlers ([exn:fail? (lambda (_e) (void))])
    (set-sign-code! id code)))

;; =========================================
;; COMMANDS – CROSSINGS
;; =========================================

(define (infrabel-put-crossing! id #:state [state 'open])
  (ensure-rw!)
  (send rw put-crossing id #:state state)
  (case state
    [(open)
     (with-handlers ([exn:fail? (lambda (_e) (void))])
       (open-crossing! id))]
    [(closed)
     (with-handlers ([exn:fail? (lambda (_e) (void))])
       (close-crossing! id))])
  id)

(define (infrabel-remove-crossing! id)
  (ensure-rw!)
  (send rw remove-crossing id))

(define (infrabel-open-crossing! id)
  (ensure-rw!)
  (send rw open-crossing id)
  (with-handlers ([exn:fail? (lambda (_e) (void))])
    (open-crossing! id)))

(define (infrabel-close-crossing! id)
  (ensure-rw!)
  (send rw close-crossing id)
  (with-handlers ([exn:fail? (lambda (_e) (void))])
    (close-crossing! id)))

;; =========================================
;; COMMANDS – DETECTION
;; =========================================

(define (infrabel-load-detections-from-backend!)
  (start-backend!)
  (get-detection-block-ids)
  (void))

;; =========================================
;; ROUTING – DRIVE TO
;; =========================================

(define DRIVE-SPEED       80)
(define DRIVE-POLL-PERIOD 0.1)
(define DRIVE-TIMEOUT-S   90)

(define (drive-backward? train-id first-step)
  (equal? first-step (hash-ref train-sim-prev train-id #f)))

(define (try-set-switch! id position)
  (with-handlers ([exn:fail? (lambda (_) (void))])
    (infrabel-set-switch-position! id position)))


(define (set-route-switch-position! sw position)
  (cond
    [(eq? sw 'S-2-3)
     (case position
       [(1) (try-set-switch! 'S-2 1)]
       [(2) (try-set-switch! 'S-2 2) (try-set-switch! 'S-3 1)]
       [(3) (try-set-switch! 'S-2 2) (try-set-switch! 'S-3 2)])]
    [else (try-set-switch! sw position)]))

(define (apply-route-switches! full-path)
  (define sw-ids (list->set (switch-ids-for-setup current-setup)))
  (for ([i (in-range 1 (- (length full-path) 1))])
    (define node   (list-ref full-path i))
    (define before (list-ref full-path (- i 1)))
    (define after  (list-ref full-path (+ i 1)))
    (when (set-member? sw-ids node)
      (define pos (switch-position-for-path current-setup node before after))
      (when pos
        (set-route-switch-position! node pos)))))

(define (commit-arrival! train-id destination)
  (define t (infrabel-get-train train-id))
  (when t
    (define old-cb (send t get-current-block))
    (unless (equal? old-cb destination)
      (send t set-position
            #:current-block destination
            #:next-block    old-cb
            #:direction     (send t get-direction))
      (hash-set! train-sim-prev train-id
                 (compute-sim-prev destination old-cb))))
  (release-train-except! train-id destination)
  (hash-set! reservations destination train-id))

(define (flip-train-sim-prev! train-id)
  (define current (hash-ref train-sim-prev train-id #f))
  (define t (infrabel-get-train train-id))
  (when (and current t)
    (define cb (send t get-current-block))
    (when cb
      (define other (graph-other-neighbor current-setup cb current))
      (hash-set! train-sim-prev train-id (or other 'uninitialized)))))

(define (spawn-drive-monitor! train-id destination)
  (define t
    (thread
     (lambda ()
       (let loop ([ticks 0])
         (when (< ticks (/ DRIVE-TIMEOUT-S DRIVE-POLL-PERIOD))
           (sleep DRIVE-POLL-PERIOD)
           (define tr (infrabel-get-train train-id))
           (cond
             [(not tr) (void)]
             [else
              (define occupied
                (with-handlers ([exn:fail? (lambda (_) '())])
                  (infrabel-occupied-detection-ids)))
              (cond
                [(member destination occupied)
                 (define old-speed
                   (with-handlers ([exn:fail? (lambda (_) 0)])
                     (get-loco-speed train-id)))
                 (with-handlers ([exn:fail? (lambda (_) (void))])
                   (infrabel-set-train-speed! train-id 0))
                 (commit-arrival! train-id destination)
                 ;; Stopping negative-speed train inverts
                 (when (negative? old-speed)
                   (flip-train-sim-prev! train-id))
                 (hash-remove! drive-monitors train-id)]
                [else
                 (loop (+ ticks 1))])]))))))
  (hash-set! drive-monitors train-id t))

(define (drive-at-signed-speed! train-id signed-speed)
  (define t (infrabel-get-train train-id))
  (when t
    (send t set-position
          #:current-block (send t get-current-block)
          #:next-block    (send t get-next-block)
          #:direction     (if (negative? signed-speed) 'backward 'forward))
    (send t set-speed (abs signed-speed)))
  (with-handlers ([exn:fail? (lambda (_e) (void))])
    (start-backend!)
    (set-loco-speed! train-id signed-speed)))

(define (infrabel-drive-train! train-id destination)
  (define t (infrabel-get-train train-id))
  (unless t
    (error 'infrabel-drive-train! "train not found: ~a" train-id))
  (define start (send t get-current-block))
  (unless start
    (error 'infrabel-drive-train! "train ~a has no current block" train-id))
  (when (equal? start destination)
    (error 'infrabel-drive-train! "train ~a is already at ~a" train-id destination))

  (define full-path (find-route current-setup start destination))
  (unless full-path
    (error 'infrabel-drive-train! "no route from ~a to ~a" start destination))

  (define conflicts (route-conflicts full-path train-id))
  (unless (null? conflicts)
    (error 'infrabel-drive-train!
           "route blocked at ~a (held by ~a)"
           conflicts
           (map (lambda (id) (hash-ref reservations id)) conflicts)))

  (stop-drive-monitor! train-id)


  (release-all-for-train! train-id)
  (reserve-elements! full-path train-id)

  (apply-route-switches! full-path)

  (define first-step (cadr full-path))
  (define backward?  (drive-backward? train-id first-step))
  (define speed      (if backward? (- DRIVE-SPEED) DRIVE-SPEED))

  (drive-at-signed-speed! train-id speed)
  (spawn-drive-monitor! train-id destination)

  (define det-set (list->set (infrabel-detection-ids)))
  (define det-path (filter (lambda (b) (set-member? det-set b)) full-path))
  (list 'route det-path))

