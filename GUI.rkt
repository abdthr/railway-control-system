#lang racket
(require racket/gui/base
         "nmbs.rkt"
         "util.rkt")

;; --- Constants ---

(define DEFAULT-FRAME-WIDTH 900)
(define DEFAULT-FRAME-HEIGHT 600)
(define border-number 8)


(define SIGNAL-CODES '(Hp0 Hp1 Hp0+Sh0 Ks1+Zs3 Ks2 Ks2+Zs3 Sh1 Ks1+Zs3+Zs3v))

(nmbs-connect!)
(nmbs-init!)

(define SPECIAL-SWITCH-SPECS
  ;; BASED ON MODEL
  '((S-2-3
     (parts  (S-2 S-3))
     (labels ("left" "middle" "right"))
     (states ((1 1) (2 1) (2 2)))
     (order  (S-2 S-3)))

    (S-5-6
     (parts  (S-5 S-6))
     (labels ("position 1" "position 2"))
     (states ((1 1) (2 2)))
     (order  (S-5 S-6)))))

;; ============================================================
;;  FRAME & BASIC-LAYOUT
;; ============================================================

(define frame
  (new frame%
       (label "GUI")
       (width DEFAULT-FRAME-WIDTH)
       (height DEFAULT-FRAME-HEIGHT)))

(define root (new horizontal-panel% (parent frame)))

;; LEFT side

(define panel-left
  (new vertical-panel%
       (parent root)
       (stretchable-width #t)
       (min-width 500)))

(define entity-tabs
  (new tab-panel%
       (parent panel-left)
       (choices '("Trains" "Switches" "Signals" "Crossings"))
       (callback
        (lambda (_tab _evt)
          (case (send entity-tabs get-selection)
            [(0) (build-trains-view!)]
            [(1) (build-switches-view!)]
            [(2) (build-signals-view!)]
            [(3) (build-crossings-view!)]
            [else (void)])))))

(define entity-content-panel
  (new vertical-panel%
       (parent entity-tabs)
       (border border-number)))

;; RIGHT side

(define panel-right
  (new vertical-panel%
       (parent root)))

(define scenario-buttons-row
  (new horizontal-panel%
       (parent panel-right)
       (stretchable-height #f)))

(new button%
     (parent scenario-buttons-row)
     (label "Save Scenario")
     (callback
      (lambda (_b _e)
        (define path (put-file "Scenario opslaan" frame #f "scenario" "txt" '() '()))
        (when path
          (with-handlers ([exn:fail?
                           (lambda (e)
                             (message-box "Fout"
                                          (format "Opslaan mislukt:\n~a" (exn-message e))
                                          frame '(ok)))])
            (nmbs-save-scenario! path)
            (add-log! "Scenario opgeslagen: ~a" (path->string path)))))))

(new button%
     (parent scenario-buttons-row)
     (label "Load Scenario")
     (callback
      (lambda (_b _e)
        (define path (get-file "Scenario laden" frame #f #f "txt" '() '()))
        (when path
          (with-handlers ([exn:fail?
                           (lambda (e)
                             (message-box "Fout"
                                          (format "Laden mislukt:\n~a" (exn-message e))
                                          frame '(ok)))])
            (nmbs-load-scenario! path)
            (set! selected-train-id #f)
            (set! current-block-msg #f)
            (build-trains-view!)
            (build-detections-view!)
            (add-log! "Scenario geladen: ~a" (path->string path)))))))

;; ============================================================
;;  BACKEND SWITCH (Simulator <-> Z21 hardware)
;; ============================================================

(define current-backend-mode
  (with-handlers ([exn:fail? (lambda (_) 'simulator)])
    (nmbs-get-backend)))

(define (backend-mode->index mode)
  (if (eq? mode 'hardware) 1 0))

(define (refresh-all-views!)
  (case (send entity-tabs get-selection)
    [(0) (build-trains-view!)]
    [(1) (build-switches-view!)]
    [(2) (build-signals-view!)]
    [(3) (build-crossings-view!)]
    [else (void)])
  (build-detections-view!))

(define backend-row
  (new horizontal-panel%
       (parent panel-right)
       (stretchable-height #f)))

(define backend-radio
  (new radio-box%
       (parent backend-row)
       (label "Backend")
       (choices '("Simulator" "Z21 hardware"))
       (selection (backend-mode->index current-backend-mode))
       (callback
        (lambda (rb _evt)
          (define mode (if (zero? (send rb get-selection)) 'simulator 'hardware))
          (with-handlers ([exn:fail?
                           (lambda (e)
                             (send rb set-selection (backend-mode->index current-backend-mode))
                             (message-box "Backend"
                                          (format "Kon niet naar ~a schakelen:\n~a"
                                                  mode (exn-message e))
                                          frame '(ok stop)))])
            (nmbs-set-backend! mode)
            (set! current-backend-mode mode)
            (set! selected-train-id #f)
            (set! current-block-msg #f)
            (refresh-all-views!)
            (add-log! "Backend gewisseld naar ~a" mode))))))

(define logbook-tab
  (new tab-panel%
       (parent panel-right)
       (choices '("Logbook"))))

(define detections-tab
  (new tab-panel%
       (parent panel-right)
       (choices '("Blocks"))))

;; --- Info Panels ---

(define logbook-info-panel
  (new vertical-panel% (parent logbook-tab) (border border-number)))

(define detections-info-panel
  (new vertical-panel% (parent detections-tab) (border border-number)))

;; ============================================================
;;  LOGBOOK UI + Helper
;; ============================================================

(define log-list
  (new list-box%
       (parent logbook-info-panel)
       (label "Log")
       (choices '())))

(define (add-log! fmt . args)
  (define msg (apply format fmt args))
  (send log-list append msg))

;; ============================================================
;;  DETECTION UI + Helper
;; ============================================================

(define last-occupied-detection-ids #f)

(define (normalize-block-ids ids)
  (sort (remove-duplicates ids) symbol<?))

(define (current-occupied-block-ids)
  (nmbs-occupied-detection-ids))

(define (build-detections-view!)
  (clear-panel! detections-info-panel)
  (define occupied (normalize-block-ids (current-occupied-block-ids)))
  (for ([bid (in-list (nmbs-detection-ids))])
    (render-detection-row detections-info-panel
                          bid
                          (member bid occupied)))
  (set! last-occupied-detection-ids occupied))

(define (refresh-detections-if-needed!)
  (define current (normalize-block-ids (current-occupied-block-ids)))
  (unless (equal? current last-occupied-detection-ids)
    (build-detections-view!)))

(define refresh-timer
  (new timer%
       [notify-callback
        (lambda ()
          (when detections-info-panel
            (refresh-detections-if-needed!))
          (when (and current-block-msg selected-train-id)
            (with-handlers ([exn:fail? (lambda (_) (void))])
              (define tr (nmbs-get-train selected-train-id))
              (when tr
                (send current-block-msg set-label
                      (format "Current block: ~a" (train-current-block tr)))))))]))

(send refresh-timer start 500)

(define (render-detection-row parent block-id occupied?)
  (define row (new horizontal-panel% (parent parent)))
  (new message%
       (parent row)
       (label (format "~a" block-id)))
  (new message%
       (parent row)
       (label (if occupied? "occupied" "not occupied"))))

;; ============================================================
;;  DIALOGEN
;; ============================================================

(define (ask-train-number)
  (define dlg
    (new dialog%
         (label "Trein toevoegen")
         (parent frame)
         (width 280)))

  (new message% (parent dlg) (label "Geef het treinnummer:"))
  (define num-field
    (new text-field% (parent dlg) (label "Nummer: ")))

  (define result #f)

  (define btn-row (new horizontal-panel% (parent dlg)))
  (new button%
       (parent btn-row)
       (label "OK")
       (callback
        (lambda (_b _e)
          (define n (string->number (string-trim (send num-field get-value))))
          (cond
            [(and n (integer? n) (positive? n))
             (set! result (inexact->exact n))
             (send dlg show #f)]
            [else
             (message-box "Fout"
                          "Geef een positief geheel getal in"
                          dlg '(ok))]))))
  (new button%
       (parent btn-row)
       (label "Annuleer")
       (callback (lambda (_b _e) (send dlg show #f))))

  (send dlg show #t)
  result)

(define (ask-train-spawn-blocks)
  (define detection-ids (nmbs-detection-ids))
  (when (null? detection-ids)
    (message-box "Fout" "Geen detectieblokken beschikbaar." frame '(ok))
    (error 'ask-train-spawn-blocks "no detection blocks"))

  (define block-strings (map symbol->string detection-ids))

  (define dlg
    (new dialog%
         (label "Trein toevoegen")
         (parent frame)
         (width 320)))

  (new message% (parent dlg) (label "Previous blok:"))
  (define cb-choice
    (new choice%
         (parent dlg)
         (label "Blok: ")
         (choices block-strings)))

  (new message% (parent dlg) (label "Spawn blok:"))
  (define nb-choice
    (new choice%
         (parent dlg)
         (label "Blok: ")
         (choices block-strings)
         (selection (min 1 (sub1 (length block-strings))))))

  (define result #f)

  (define btn-row (new horizontal-panel% (parent dlg)))
  (new button%
       (parent btn-row)
       (label "OK")
       (callback
        (lambda (_b _e)
          (define cb (list-ref detection-ids (send cb-choice get-selection)))
          (define nb (list-ref detection-ids (send nb-choice get-selection)))
          (if (eq? cb nb)
              (message-box "Fout"
                           "Spawnblok en vorig blok mogen niet hetzelfde zijn."
                           dlg '(ok))
              (begin
                (set! result (list cb nb))
                (send dlg show #f))))))
  (new button%
       (parent btn-row)
       (label "Annuleer")
       (callback (lambda (_b _e) (send dlg show #f))))

  (send dlg show #t)
  result)

(define (ask-destination-block)
  (define detection-ids (nmbs-detection-ids))
  (when (null? detection-ids)
    (message-box "Fout" "Geen detectieblokken beschikbaar." frame '(ok))
    (error 'ask-destination-block "no detection blocks"))

  (define block-strings (map symbol->string detection-ids))

  (define dlg
    (new dialog%
         (label "Rijden naar blok")
         (parent frame)
         (width 280)))

  (new message% (parent dlg) (label "Kies bestemmingsblok:"))
  (define dest-choice
    (new choice%
         (parent dlg)
         (label "Blok: ")
         (choices block-strings)))

  (define result #f)

  (define btn-row (new horizontal-panel% (parent dlg)))
  (new button%
       (parent btn-row)
       (label "OK")
       (callback
        (lambda (_b _e)
          (set! result (list-ref detection-ids (send dest-choice get-selection)))
          (send dlg show #f))))
  (new button%
       (parent btn-row)
       (label "Annuleer")
       (callback (lambda (_b _e) (send dlg show #f))))

  (send dlg show #t)
  result)

;; ============================================================
;; TRAINS UI
;; ============================================================

(define selected-train-id #f)
(define current-block-msg #f)

(define trains-main-panel #f)
(define trains-sidebar-panel #f)
(define trains-detail-panel #f)
(define trains-list-box #f)

(define (current-entity-tab-label)
  (list-ref '("Trains" "Switches" "Signals" "Crossings")
            (send entity-tabs get-selection)))

(define (refresh-trains-list!)
  (when trains-list-box
    (send trains-list-box clear)
    (for ([tid (in-list (nmbs-train-ids))])
      (send trains-list-box append (format "~a" tid)))

    ;; selection reset
    (when selected-train-id
      (define ids (nmbs-train-ids))
      (define idx
        (let loop ([lst ids] [i 0])
          (cond
            [(null? lst) #f]
            [(equal? (car lst) selected-train-id) i]
            [else (loop (cdr lst) (add1 i))])))
      (when idx
        (send trains-list-box select idx)))))

(define (build-trains-view!)
  (clear-panel! entity-content-panel)

  (set! trains-main-panel
        (new horizontal-panel%
             (parent entity-content-panel)))

  ;; Left: list + ADD + REMOVE
  (set! trains-sidebar-panel
        (new vertical-panel%
             (parent trains-main-panel)
             (min-width 220)
             (border border-number)))

  (new message%
       (parent trains-sidebar-panel)
       (label "Trains"))

  (set! trains-list-box
        (new list-box%
             (parent trains-sidebar-panel)
             (label "All trains   ")
             (choices '())
             (callback
              (lambda (lb _evt)
                (define idx (send lb get-selection))
                (define ids (nmbs-train-ids))
                (when (and idx (>= idx 0) (< idx (length ids)))
                  (set! selected-train-id (list-ref ids idx))
                  (clear-panel! trains-detail-panel)
                  (when selected-train-id
                    (render-train-info trains-detail-panel selected-train-id)))))))

  (define trains-buttons-row
    (new horizontal-panel%
         (parent trains-sidebar-panel)))

  (new button%
       (parent trains-buttons-row)
       (label "Add")
       (callback
        (lambda (_btn _evt)
          (define number (ask-train-number))
          (when number
            (define choice (ask-train-spawn-blocks))
            (when choice
              (define previous-block (car choice))
              (define spawn-block    (cadr choice))
              (with-handlers ([exn:fail?
                               (lambda (e)
                                 (message-box "Trein toevoegen"
                                              (exn-message e)
                                              frame '(ok stop)))])
                (define new-id (nmbs-create-train! number spawn-block previous-block))
                (set! selected-train-id new-id)
                (refresh-trains-list!)
                (clear-panel! trains-detail-panel)
                (when selected-train-id
                  (render-train-info trains-detail-panel selected-train-id))
                (add-log! "Train ~a added at block ~a" new-id spawn-block)
                (when (and (eq? current-backend-mode 'hardware)
                           (not (memq new-id '(T-3 T-5 T-7 T-9))))
                  (add-log! "Let op: ~a bestaat niet op de echte hardware (enkel T-3/T-5/T-7/T-9)"
                            new-id))))))))

  (new button%
       (parent trains-buttons-row)
       (label "Remove")
       (callback
        (lambda (_btn _evt)
          (when selected-train-id
            (nmbs-remove-train! selected-train-id)
            (add-log! "Train ~a removed" selected-train-id)
            (set! selected-train-id #f)
            (refresh-trains-list!)
            (clear-panel! trains-detail-panel)))))

  ;; Right: detail
  (set! trains-detail-panel
        (new vertical-panel%
             (parent trains-main-panel)
             (border border-number)))

  (refresh-trains-list!)

  (when (and (not selected-train-id)
             (pair? (nmbs-train-ids)))
    (set! selected-train-id (car (nmbs-train-ids)))
    (send trains-list-box select 0))

  (clear-panel! trains-detail-panel)
  (when selected-train-id
    (render-train-info trains-detail-panel selected-train-id)))

;; ============================================================
;;  SWITCHES VIEW
;; ============================================================

(define selected-switch-id #f)

(define switches-main-panel #f)
(define switches-sidebar-panel #f)
(define switches-detail-panel #f)
(define switches-list-box #f)

;; HELPERS

(define (gui-switch-ui-ids)
  (define real-ids (sort-switch-ids (nmbs-switch-ids)))

  (append
   (filter (lambda (sid) (not (member sid '(S-2 S-3 S-5 S-6))))
           real-ids)
   (if (and (member 'S-2 real-ids) (member 'S-3 real-ids))
       '(S-2-3)
       '())
   (if (and (member 'S-5 real-ids) (member 'S-6 real-ids))
       '(S-5-6)
       '())))

(define (special-switch-spec sid)
  (assoc sid SPECIAL-SWITCH-SPECS))

(define (special-switch-id? sid)
  (not (not (special-switch-spec sid))))

(define (spec-field spec key)
  (cadr (assoc key (cdr spec))))

(define (find-index pred lst)
  (let loop ([lst lst] [i 0])
    (cond
      [(null? lst) #f]
      [(pred (car lst)) i]
      [else (loop (cdr lst) (add1 i))])))

(define (positions-match? xs ys)
  (and (= (length xs) (length ys))
       (andmap equal? xs ys)))

(define (current-special-switch-state-index sid)
  (define spec   (special-switch-spec sid))
  (define parts  (spec-field spec 'parts))
  (define states (spec-field spec 'states))

  (define current-positions
    (map (lambda (part-id)
           (define sw (nmbs-get-switch part-id))
           (if sw
               (switch-position sw)
               #f))
         parts))

  (or (find-index (lambda (st)
                    (positions-match? st current-positions))
                  states)
      0))

(define (set-special-switch-state! sid state-index)
  (define spec   (special-switch-spec sid))
  (define parts  (spec-field spec 'parts))
  (define states (spec-field spec 'states))
  (define order  (spec-field spec 'order))
  (define target-state (list-ref states state-index))

  ;; map: switch-id -> position
  (define assignments
    (map cons parts target-state))


  (for ([swid (in-list order)])
    (define pair (assoc swid assignments))
    (when pair
      (nmbs-set-switch-position! (car pair) (cdr pair)))))

(define (refresh-switches-list!)
  (when switches-list-box
    (send switches-list-box clear)
    (define ui-ids (gui-switch-ui-ids))
    (for ([sid (in-list ui-ids)])
      (send switches-list-box append (format "~a" sid)))

    (when selected-switch-id
      (let loop ([lst ui-ids] [i 0])
        (cond
          [(null? lst) (void)]
          [(equal? (car lst) selected-switch-id)
           (send switches-list-box select i)]
          [else
           (loop (cdr lst) (add1 i))])))))

(define (show-selected-switch!)
  (when switches-detail-panel
    (clear-panel! switches-detail-panel)
    (when selected-switch-id
      (if (special-switch-id? selected-switch-id)
          (render-special-switch-info switches-detail-panel selected-switch-id)
          (render-switch-info switches-detail-panel selected-switch-id)))))

(define (build-switches-view!)
  (clear-panel! entity-content-panel)

  (set! switches-main-panel
        (new horizontal-panel%
             (parent entity-content-panel)
             (stretchable-width #t)
             (stretchable-height #t)))

  ;; left: list
  (set! switches-sidebar-panel
        (new vertical-panel%
             (parent switches-main-panel)
             (min-width 220)
             (stretchable-height #t)
             (border border-number)))

  (new message%
       (parent switches-sidebar-panel)
       (label "Switches"))

  (set! switches-list-box
        (new list-box%
             (parent switches-sidebar-panel)
             (label "All switches")
             (choices '())
             (callback
              (lambda (lb _evt)
                (define ui-ids (gui-switch-ui-ids))
                (define idx (send lb get-selection))
                (when (and idx (>= idx 0) (< idx (length ui-ids)))
                  (set! selected-switch-id (list-ref ui-ids idx))
                  (show-selected-switch!))))))

  (new message%
       (parent switches-sidebar-panel)
       (label "Special: S-2-3 and S-5-6"))

  ;; right: detail
  (set! switches-detail-panel
        (new vertical-panel%
             (parent switches-main-panel)
             (stretchable-width #t)
             (stretchable-height #t)
             (border border-number)))

  (refresh-switches-list!)

  (when (pair? (gui-switch-ui-ids))
    (unless selected-switch-id
      (set! selected-switch-id (car (gui-switch-ui-ids)))
      (send switches-list-box select 0)))

  (show-selected-switch!))

;; ============================================================
;; SIGNALS VIEW
;; ============================================================

(define selected-signal-id #f)

(define signals-main-panel #f)
(define signals-sidebar-panel #f)
(define signals-detail-panel #f)
(define signals-list-box #f)

(define (refresh-signals-list!)
  (when signals-list-box
    (send signals-list-box clear)
    (define ids (nmbs-signal-ids))
    (for ([sid (in-list ids)])
      (send signals-list-box append (format "~a" sid)))

    (when selected-signal-id
      (let loop ([lst ids] [i 0])
        (cond
          [(null? lst) (void)]
          [(equal? (car lst) selected-signal-id)
           (send signals-list-box select i)]
          [else
           (loop (cdr lst) (add1 i))])))))

(define (show-selected-signal!)
  (when signals-detail-panel
    (clear-panel! signals-detail-panel)
    (when selected-signal-id
      (render-signal-info signals-detail-panel selected-signal-id))))

(define (build-signals-view!)
  (clear-panel! entity-content-panel)

  (set! signals-main-panel
        (new horizontal-panel%
             (parent entity-content-panel)
             (stretchable-width #t)
             (stretchable-height #t)))

  ;; left: list + buttons
  (set! signals-sidebar-panel
        (new vertical-panel%
             (parent signals-main-panel)
             (min-width 220)
             (stretchable-height #t)
             (border border-number)))

  (new message%
       (parent signals-sidebar-panel)
       (label "Signals"))

  (set! signals-list-box
        (new list-box%
             (parent signals-sidebar-panel)
             (label "All signals")
             (choices '())
             (callback
              (lambda (lb _evt)
                (define idx (send lb get-selection))
                (define ids (nmbs-signal-ids))
                (when (and idx (>= idx 0) (< idx (length ids)))
                  (set! selected-signal-id (list-ref ids idx))
                  (show-selected-signal!))))))

  (define signals-buttons-row
    (new horizontal-panel%
         (parent signals-sidebar-panel)))

  (new button%
       (parent signals-buttons-row)
       (label "Add")
       (callback
        (lambda (_btn _evt)
          (define new-id (nmbs-create-default-signal!))
          (set! selected-signal-id new-id)
          (refresh-signals-list!)
          (show-selected-signal!)
          (add-log! "Signal ~a added" new-id))))

  (new button%
       (parent signals-buttons-row)
       (label "Remove")
       (callback
        (lambda (_btn _evt)
          (when selected-signal-id
            (nmbs-remove-signal! selected-signal-id)
            (add-log! "Signal ~a removed" selected-signal-id)
            (set! selected-signal-id #f)
            (refresh-signals-list!)
            (clear-panel! signals-detail-panel)))))

  ;; right: detail
  (set! signals-detail-panel
        (new vertical-panel%
             (parent signals-main-panel)
             (stretchable-width #t)
             (stretchable-height #t)
             (border border-number)))

  (refresh-signals-list!)

  (when (pair? (nmbs-signal-ids))
    (unless selected-signal-id
      (set! selected-signal-id (car (nmbs-signal-ids)))
      (send signals-list-box select 0)))

  (show-selected-signal!))

;; ============================================================
;; CROSSING VIEW
;; ============================================================

(define crossings-main-panel #f)
(define crossings-sidebar-panel #f)
(define crossings-detail-panel #f)
(define crossings-list-box #f)
(define selected-crossing-id #f)

(define (refresh-crossings-list!)
  (when crossings-list-box
    (send crossings-list-box clear)
    (define ids (nmbs-crossing-ids))
    (for ([cid (in-list ids)])
      (send crossings-list-box append (format "~a" cid)))

    (when selected-crossing-id
      (let loop ([lst ids] [i 0])
        (cond
          [(null? lst) (void)]
          [(equal? (car lst) selected-crossing-id)
           (send crossings-list-box select i)]
          [else
           (loop (cdr lst) (add1 i))])))))

(define (show-selected-crossing!)
  (when crossings-detail-panel
    (clear-panel! crossings-detail-panel)
    (when selected-crossing-id
      (render-crossing-info crossings-detail-panel selected-crossing-id))))

(define (build-crossings-view!)
  (clear-panel! entity-content-panel)

  (set! crossings-main-panel
        (new horizontal-panel%
             (parent entity-content-panel)
             (stretchable-width #t)
             (stretchable-height #t)))

  ;; left: list + buttons
  (set! crossings-sidebar-panel
        (new vertical-panel%
             (parent crossings-main-panel)
             (min-width 220)
             (stretchable-height #t)
             (border border-number)))

  (new message%
       (parent crossings-sidebar-panel)
       (label "Crossings"))

  (set! crossings-list-box
        (new list-box%
             (parent crossings-sidebar-panel)
             (label "All crossings")
             (choices '())
             (callback
              (lambda (lb _evt)
                (define idx (send lb get-selection))
                (define ids (nmbs-crossing-ids))
                (when (and idx (>= idx 0) (< idx (length ids)))
                  (set! selected-crossing-id (list-ref ids idx))
                  (show-selected-crossing!))))))

  (define crossings-buttons-row
    (new horizontal-panel%
         (parent crossings-sidebar-panel)))

  (new button%
       (parent crossings-buttons-row)
       (label "Add")
       (callback
        (lambda (_btn _evt)
          (define new-id (nmbs-create-default-crossing!))
          (set! selected-crossing-id new-id)
          (refresh-crossings-list!)
          (show-selected-crossing!)
          (add-log! "Crossing ~a added" new-id))))

  (new button%
       (parent crossings-buttons-row)
       (label "Remove")
       (callback
        (lambda (_btn _evt)
          (when selected-crossing-id
            (nmbs-remove-crossing! selected-crossing-id)
            (add-log! "Crossing ~a removed" selected-crossing-id)
            (set! selected-crossing-id #f)
            (refresh-crossings-list!)
            (clear-panel! crossings-detail-panel)))))

  ;; right: detail
  (set! crossings-detail-panel
        (new vertical-panel%
             (parent crossings-main-panel)
             (stretchable-width #t)
             (stretchable-height #t)
             (border border-number)))

  (refresh-crossings-list!)

  (when (pair? (nmbs-crossing-ids))
    (unless selected-crossing-id
      (set! selected-crossing-id (car (nmbs-crossing-ids)))
      (send crossings-list-box select 0)))

  (show-selected-crossing!))

;; ============================================================
;;  INFORMATION RENDER FUNCTIONS
;; ============================================================

;; --- Train ID info ---
(define (render-train-info panel id)
  (define train (nmbs-get-train id))
  (unless train
    (error 'render-train-info (format "Unknown train ~a" id)))

  (new message%
       (parent panel)
       (label (format "Train: ~a" id)))

  (set! current-block-msg
        (new message%
             (parent panel)
             (label (format "Current block: ~a" (train-current-block train)))))

  ;; Direction (radio-box)
  (define dir-selection
    (if (eq? (train-direction train) 'forward) 0 1))

  (new radio-box%
       (parent panel)
       (label "Direction")
       (choices '("forward" "backward"))
       (selection dir-selection)
       (callback
        (lambda (rb _evt)
          (define sel (send rb get-selection))
          (define d (if (zero? sel) 'forward 'backward))
          (nmbs-set-train-direction! id d)
          (add-log! "Train ~a direction -> ~a" id d))))

  ;; Speed (slider)
  (new slider%
       (parent panel)
       (label "Speed")
       (min-value 0)
       (max-value 200)
       (init-value (train-speed train))
       (callback
        (lambda (sld _evt)
          (define v (send sld get-value))
          (nmbs-set-train-speed! id v)
          (add-log! "Train ~a speed -> ~a" id v))))

  ;; Drive to destination
  (new button%
       (parent panel)
       (label "Drive to...")
       (callback
        (lambda (_b _e)
          (define dest
            (with-handlers ([exn:fail? (lambda (_) #f)])
              (ask-destination-block)))
          (when dest
            (with-handlers ([exn:fail?
                             (lambda (e)
                               (message-box "Route fout"
                                            (exn-message e)
                                            frame '(ok stop)))])
              (nmbs-drive-train! id dest)
              (add-log! "Train ~a rijdt naar ~a" id dest))))))
  )

;; --- Switch ID info ---
(define (render-switch-info panel id)
  (define sw (nmbs-get-switch id))
  (unless sw
    (error 'render-switch-info (format "Unknown switch ~a" id)))

  (new message%
       (parent panel)
       (label (format "Switch: ~a" id)))

  (define pos-selection
    (if (= (switch-position sw) 1) 0 1))

  (new radio-box%
       (parent panel)
       (label "Position")
       (choices '("1" "2"))
       (selection pos-selection)
       (callback
        (lambda (rb _evt)
          (define sel (send rb get-selection))
          (define pos (if (zero? sel) 1 2))
          (nmbs-set-switch-position! id pos)
          (add-log! "Switch ~a position -> ~a" id pos)))))

(define (render-special-switch-info panel sid)
  (define spec   (special-switch-spec sid))
  (define labels (spec-field spec 'labels))
  (define init-index (current-special-switch-state-index sid))

  (new message%
       (parent panel)
       (label (format "Switch: ~a" sid)))

  (new radio-box%
       (parent panel)
       (label "Position")
       (choices labels)
       (selection init-index)
       (callback
        (lambda (rb _evt)
          (define sel (send rb get-selection))
          (set-special-switch-state! sid sel)
          (add-log! "Switch ~a -> ~a"
                    sid
                    (list-ref labels sel))))))

;; --- Signal ID info ---
(define (render-signal-info panel id)
  (define sig (nmbs-get-signal id))
  (unless sig
    (error 'render-signal-info (format "Unknown signal ~a" id)))

  (new message%
       (parent panel)
       (label (format "Signal: ~a" id)))

  (define current-code (signal-code sig))
  (define init-index
    (or (list-index (lambda (c) (eq? c current-code)) SIGNAL-CODES)
        0))

  (new choice%
       (parent panel)
       (label "Signal code: ")
       (choices (symbols->strings SIGNAL-CODES))
       (selection init-index)
       (callback 
        (lambda (ch _evt)
          (define idx (send ch get-selection))
          (define new-code (list-ref SIGNAL-CODES idx))
          (nmbs-set-signal-code! id new-code)
          (add-log! "Signal ~a code -> ~a" id new-code)))))

;; --- Crossing ID info ---
(define (render-crossing-info panel id)
  (define cr (nmbs-get-crossing id))
  (unless cr
    (error 'render-crossing-info (format "Unknown crossing ~a" id)))

  (new message%
       (parent panel)
       (label (format "Crossing: ~a" id)))

  (define state-selection
    (if (eq? (crossing-state cr) 'open) 0 1))

  (new radio-box%
       (parent panel)
       (label "State")
       (choices '("open" "closed"))
       (selection state-selection)
       (callback
        (lambda (rb _evt)
          (define sel (send rb get-selection))
          (define new-state (if (zero? sel) 'open 'closed))
          (if (zero? sel)
              (nmbs-open-crossing! id)
              (nmbs-close-crossing! id))
          (add-log! "Crossing ~a state -> ~a" id new-state)))))

;; ============================================================
;;  GENERIEKE UPDATE FUNCTIE + CALLBACKS
;; ============================================================

(define (update-trains-view!)
  (when trains-main-panel
    (refresh-trains-list!)
    (clear-panel! trains-detail-panel)
    (when selected-train-id
      (render-train-info trains-detail-panel selected-train-id))))

;; ============================================================
;;  START
;; ============================================================

(build-trains-view!)
(build-detections-view!)
(send frame show #t)