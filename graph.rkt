#lang racket

(require racket/list)

(provide
 make-graph
 graph-add-edge!
 graph-neighbors
 graph-bfs)

(define (make-graph)
  (make-hash))

(define (graph-add-edge! g a b)
  (hash-update! g a (lambda (ns) (if (member b ns) ns (cons b ns))) '())
  (hash-update! g b (lambda (ns) (if (member a ns) ns (cons a ns))) '()))

(define (graph-neighbors g node)
  (hash-ref g node '()))

(define (graph-bfs g start goal [valid? #f])
  (if (equal? start goal)
      (list start)
      (let loop ([queue (list (list start))])
        (cond
          [(null? queue) #f]
          [else
           (define path (car queue))
           (define node (last path))
           (define prev (and (> (length path) 1)
                             (list-ref path (- (length path) 2))))
           (define extensions
             (filter (lambda (n)
                       (and (not (member n path))
                            (or (not valid?) (not prev)
                                (valid? prev node n))))
                     (graph-neighbors g node)))
           (define found (findf (lambda (n) (equal? n goal)) extensions))
           (if found
               (append path (list found))
               (loop (append (cdr queue)
                             (map (lambda (n) (append path (list n)))
                                  extensions))))]))))
