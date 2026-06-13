#lang racket

(require "graph.rkt")

(provide
 find-route
 switch-position-for-path
 switch-ids-for-setup
 graph-for-setup
 valid-step-for-setup
 graph-other-neighbor)

;; --- Undirected edge lists (one entry per edge) ---

(define HARDWARE-EDGES
  '((1-1  . S-28)  (1-1  . S-10)
    (1-2  . S-27)  (1-2  . S-9)
    (1-3  . S-27)  (1-3  . U-2)
    (1-4  . 1-5)   (1-4  . S-26)
    (1-5  . S-20)
    (1-6  . S-5)   (1-6  . 1-7)
    (1-7  . S-28)
    (1-8  . S-25)
    (2-1  . S-1)
    (2-2  . S-2-3)
    (2-3  . S-12)  (2-3  . S-6)
    (2-4  . S-23)  (2-4  . S-20)
    (2-5  . S-8)
    (2-6  . S-4)
    (2-7  . S-4)
    (2-8  . S-16)
    (U-1  . S-26)  (U-1  . S-28)
    (U-2  . S-24)
    (U-3  . S-9)   (U-3  . S-24)
    (U-4  . S-12)  (U-4  . S-23)
    (U-5  . S-7)   (U-5  . S-25)
    (U-6  . S-2-3) (U-6  . S-1)
    (U-7  . S-16)
    (S-1  . S-25)
    (S-2-3 . S-7)  (S-2-3 . S-8)
    (S-4  . S-8)
    (S-5  . S-6)   (S-5  . S-7)
    (S-6  . S-20)
    (S-9  . S-11)
    (S-10 . S-11)  (S-10 . S-16)
    (S-11 . S-12)
    (S-23 . S-24)
    (S-26 . S-27)))

(define LOOP-EDGES
  '((D1 . D2) (D2 . D3) (D3 . D4) (D4 . D5)
    (D5 . T1) (T1 . D6) (D6 . D7) (D7 . D8)
    (D8 . T2) (T2 . D1)))

(define LOOP-AND-SWITCHES-EDGES
  '((D1 . D2) (D2 . D3) (D3 . D4) (D4 . S1)
    (S1 . U1) (U1 . T1) (T1 . D5) (D5 . D6)
    (D6 . U3) (U3 . S2) (S2 . T2) (T2 . D1)
    (S1 . U2) (U2 . T3) (T3 . S3) (S3 . D7)
    (D7 . U4) (U4 . S2) (S3 . D8) (D8 . D9)))

;; --- Switch end-position tables ---

(define HARDWARE-SWITCH-POSITIONS
  (make-immutable-hash
   (list
    (cons 'S-1   (list (cons '2-1  1) (cons 'S-25 2)))
    (cons 'S-2-3 (list (cons 'S-7  1) (cons '2-2  2) (cons 'S-8  3)))
    (cons 'S-4   (list (cons '2-6  1) (cons '2-7  2)))
    (cons 'S-5   (list (cons '1-6  1) (cons 'S-7  2)))
    (cons 'S-6   (list (cons '2-3  1) (cons 'S-20 2)))
    (cons 'S-7   (list (cons 'U-5  1) (cons 'S-2-3 2)))
    (cons 'S-8   (list (cons '2-5  1) (cons 'S-4  2)))
    (cons 'S-9   (list (cons 'U-3  1) (cons 'S-11 2)))
    (cons 'S-10  (list (cons '1-1  1) (cons 'S-16 2)))
    (cons 'S-11  (list (cons 'S-9  1) (cons 'S-10 2)))
    (cons 'S-12  (list (cons 'U-4  1) (cons '2-3  2)))
    (cons 'S-16  (list (cons 'S-10 1) (cons '2-8  2)))
    (cons 'S-20  (list (cons '1-5  1) (cons 'S-6  2)))
    (cons 'S-23  (list (cons 'S-24 1) (cons 'U-4  2)))
    (cons 'S-24  (list (cons 'U-3  1) (cons 'U-2  2)))
    (cons 'S-25  (list (cons 'U-5  1) (cons 'S-1  2)))
    (cons 'S-26  (list (cons 'U-1  1) (cons 'S-27 2)))
    (cons 'S-27  (list (cons '1-3  1) (cons '1-2  2)))
    (cons 'S-28  (list (cons '1-7  1) (cons 'U-1  2))))))

(define LOOP-AND-SWITCHES-SWITCH-POSITIONS
  (make-immutable-hash
   (list
    (cons 'S1 (list (cons 'U1 1) (cons 'U2 2)))
    (cons 'S2 (list (cons 'U3 1) (cons 'U4 2)))
    (cons 'S3 (list (cons 'D7 1) (cons 'D8 2))))))

(define (edges-for-setup setup)
  (case setup
    [(hardware)          HARDWARE-EDGES]
    [(loop)              LOOP-EDGES]
    [(loop-and-switches) LOOP-AND-SWITCHES-EDGES]
    [else '()]))

(define (switch-table-for-setup setup)
  (case setup
    [(hardware)          HARDWARE-SWITCH-POSITIONS]
    [(loop-and-switches) LOOP-AND-SWITCHES-SWITCH-POSITIONS]
    [else                (make-immutable-hash)]))

(define (switch-ids-for-setup setup)
  (hash-keys (switch-table-for-setup setup)))

(define (graph-for-setup setup)
  (define g (make-graph))
  (for ([e (in-list (edges-for-setup setup))])
    (graph-add-edge! g (car e) (cdr e)))
  g)

(define (graph-other-neighbor setup node not-this)
  (define neighbors (graph-neighbors (graph-for-setup setup) node))
  (findf (lambda (n) (not (equal? n not-this))) neighbors))

(define (valid-step-for-setup setup)
  (define table (switch-table-for-setup setup))
  (lambda (prev node next)
    (define end-positions (hash-ref table node #f))
    (if (not end-positions)
        #t
        (not (and (assoc prev end-positions)
                  (assoc next end-positions))))))

(define (switch-position-for-path setup sw before after)
  (define end-positions (hash-ref (switch-table-for-setup setup) sw #f))
  (cond
    [(not end-positions) #f]
    [else
     (define hit (or (assoc after end-positions)
                     (assoc before end-positions)))
     (and hit (cdr hit))]))

(define (find-route setup start dest)
  (graph-bfs (graph-for-setup setup) start dest
             (valid-step-for-setup setup)))
