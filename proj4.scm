#!/usr/bin/env csi -s

;;(use matchable)
(use typed-records) ;; provides defstruct
(use srfi-69)       ;; provides hash-tables
(use posix)         ;; provides sleep
(use fmt)           ;; provides fmt

(set! *debug* #f)
(define (dprint . args)
  (if *debug*
      (apply print args)))

(define (cartesian-product As Bs)
  (flatten
   (map
    (lambda (a)
      (map
       (lambda (b)
         (cons a b))
       Bs))
    As)))

;;
;; coordinate system is (<row index> . <col index>)
;; 0,0 is bottom left, n-rows, n-cols is top right
;; up: 0,
(define (translate-current-state current-state action keep-outs sinks n-rows n-cols)
  (let* ((blind-next-state
          (case action
            ((up) ;; row++
             (cons (add1 (car current-state)) (cdr current-state)))
            ((down) ;; row--
             (cons (sub1 (car current-state)) (cdr current-state)))
            ((left) ;; col--
             (cons (car current-state) (sub1 (cdr current-state))))
            ((right) ;; col++
             (cons (car current-state) (add1 (cdr current-state))))
            (else
             (print "translate-current-state: BAD ACTION ["action"]")
             (exit 1)))))
    (dprint "CS=["current-state"] ; sinks=["sinks"]")
    (cond
     ((member current-state sinks)
      current-state)
     ((member blind-next-state keep-outs)
      current-state)
     ((< (car blind-next-state) 0)
      current-state)
     ((< (cdr blind-next-state) 0)
      current-state)
     ((>= (car blind-next-state) n-rows)
      current-state)
     ((>= (cdr blind-next-state) n-cols)
      current-state)
     (else
      blind-next-state))))


(define (coalesce-next-state-probs X)
  (let* ((ht (make-hash-table)))
    (for-each (lambda (item)
                (let* ((prob (car item))
                       (next-state (cdr item)))
                  (hash-table-set!
                   ht next-state
                   (+ (hash-table-ref/default ht next-state 0)
                      prob))))
              X)
    (map (lambda (item)
           (cons (cdr item) (car item)))
         (hash-table->alist ht))))
                                   
                                   
              

;; transitions for a state is a hash table indexed by action with values of (list (cons probability next-state) ...)
(define (gridworld-state->transitions-actions-table current-state keep-outs sinks n-rows n-cols
                         #!key (intentional-probability 0.8) (unintentional-probability 0.1))
  (let* ((actions-table (make-hash-table))
         (actions (gridworld-actions)))
    (for-each
     (lambda (intentional-action)
       (hash-table-set!
        actions-table intentional-action
        (cond
         ((member current-state sinks)
          '()
          ;(list (cons 1.0 current-state))
          )
         ((member current-state keep-outs)
          (list (cons 1.0 current-state)))
         (else
          (let* ((unintentional-actions
                  (case intentional-action
                    ((up down)
                     '(left right))
                    ((left right)
                     '(up down)))))
            (coalesce-next-state-probs
             (cons
              (cons intentional-probability 
                    (translate-current-state current-state intentional-action keep-outs sinks n-rows n-cols))
              (map
               (lambda
                   (unintentional-action)
                 (cons unintentional-probability 
                       (translate-current-state current-state unintentional-action keep-outs sinks n-rows n-cols)))
               unintentional-actions))))))))
     actions)
    actions-table))



(defstruct gridworld
    n-rows n-cols
    states
    transitions
    rewards)

(define (gridworld-actions)
  '(up down left right))

(define (init-gridworld n-rows n-cols 
                      #!key
                      (default-reward -0.06)
                      (s0 '(0 . 0))  ;; initial state
                      (keep-outs '())
                      (tester n-rows)
                      (sink-reward-alist `(
                               (( ,(modulo -1 n-rows) . ,(modulo -1 n-cols) ) . 2) ;; opposite corner to s0
                               (( ,(modulo -2 n-rows) . ,(modulo -2 n-cols) ) . 1) ;; opposite corner to s0, moved 1 row and col inward
                               )))

  
  (let* ((all-actions (gridworld-actions))
         (sinks (map car sink-reward-alist))
         (all-cell-states (cartesian-product (iota n-rows) (iota n-cols)))
         (tt-ht (make-hash-table))
         (reward-ht (make-hash-table))
         
         )
    
    (for-each (lambda (cell-state)
                (dprint "\n\nIN cell-state="cell-state)
                (let* ((reward
                        ;; sinks
                        (cond
                         ((alist-ref cell-state sink-reward-alist equal?)
                          (alist-ref cell-state sink-reward-alist equal?))
                         ;;;; blockages
                         ;;((member cell-state keep-outs)
                         ;; #f)
                         
                         ;; default
                         (else
                          default-reward)))
                       (actions-table (gridworld-state->transitions-actions-table cell-state keep-outs sinks n-rows n-cols)))

                  (hash-table-set! reward-ht cell-state reward)
                  (hash-table-set! tt-ht     cell-state actions-table)

                  ;;(add-transitions cell-state gw)
                  ;;(add-rewards 

                  (dprint "Got reward - "reward)
                  (dprint "Got transitions:" (hash-table->alist actions-table))
                  ))
              all-cell-states)
    
    (make-gridworld n-rows: n-rows n-cols: n-cols states: all-cell-states transitions: tt-ht rewards: reward-ht)))


(define (print-tt gw)
  (let* ((states (gridworld-states gw))
         (transitions (gridworld-transitions gw))
         (actions (gridworld-actions)))
    (for-each
     (lambda (state)
       (let* ((actions-table (hash-table-ref transitions state)))
         (for-each
          (lambda (action)
            (for-each
             (lambda (item)
               (let* ((probability (car item))
                      (next-state (cdr item)))
                 (print "T("state","action","next-state") = "probability)))
             (hash-table-ref actions-table action)))
          actions)))
     states)))


(define (vector-rmse avec bvec)
  (expt
   (apply +
          (map (lambda (x) (expt (apply - x) 2))
               (zip
                (vector->list avec)
                (vector->list bvec))))
   0.5))

(define (gridworld-format-vector gw vec #!key (flavor 'num) (cols (gridworld-n-cols gw)) (rows (gridworld-n-rows gw)))
  (let* ((states        (gridworld-states gw))
         (n-states      (length states))
         (state->idx-ht (make-hash-table))
         (state->idx    (lambda (s) (hash-table-ref state->idx-ht s)))) ;; TODO: vectorize hash tables for speedup
    ;; initialize lookup idx by state
    (for-each (lambda (idx)
                (hash-table-set! state->idx-ht (list-ref states idx) idx))
              (iota n-states))

    (let* ((cell-width 8)
           (row-divider
            (conc
             "\n|"
             (string-join
              (map (lambda (colidx)
                     (apply conc (map (lambda (x) "-") (iota (+ cell-width 2)))))
                     (iota cols))
              "+") "|"))
           (row-divider2
            (conc
             "\n+"
             (string-join
              (map (lambda (colidx)
                     (apply conc (map (lambda (x) "-") (iota (+ cell-width 2)))))
                     (iota cols))
              "+") "+"))
           (rv (conc
                row-divider2 "\n"
                (conc (string-join
                 (map (lambda (row)
                        (conc "| "
                              (string-join 
                               (map (lambda (col)
                                      (let* ((state (cons row col))
                                             (idx   (state->idx state))
                                             (val   (vector-ref vec idx)))
                                        (conc
                                         (cond
                                          ((eq? flavor 'num)
                                           (fmt #f (pad/left cell-width (num val 10 2))))
                                          (else
                                           (print "unknown flavor: ["flavor"]")
                                           (exit 1))))))
                                    (iota cols))
                               " | ")
                              " |" (if (eq? row 0) row-divider2 row-divider)))
                      (reverse (iota rows))) ;; bottom row is 0 ; top row is n-rows-1, so print out last row first.
                 "\n")))))
      ;;(print rv)
      ;;(exit 1)
      rv)))
             
  
         

(define (value-iteration gw #!key (reltol 0.0001) (gamma 0.1))
  (let* ((states        (gridworld-states      gw))
         (n-states      (length states)           )
         (transitions   (gridworld-transitions gw))
         (R             (gridworld-rewards     gw))
         (state->idx-ht (make-hash-table))
         (state->idx    (lambda (s) (hash-table-ref state->idx-ht s)))) ;; TODO: vectorize hash tables for speedup

    ;; initialize lookup idx by state
    (for-each (lambda (idx)
                (hash-table-set! state->idx-ht (list-ref states idx) idx))
              (iota n-states))
         
    (let loop ((Ut (make-vector n-states 0)) (round 1))
      (let*   ((Ut+1 (make-vector n-states 0))
               (policy (make-vector n-states 0)))
        (for-each
         (lambda (idx)
           (let* ((s  (list-ref states idx))
                  (r  (hash-table-ref R s))
                  (actions-table (hash-table-ref transitions s))
                  (actions (hash-table-keys actions-table)))

             (vector-set!
              Ut+1 idx 
              (+ r (* gamma
                      (apply max (map
                                  (lambda (action)
                                    (apply
                                     +
                                     (let ((prob+nextstate-pairs (hash-table-ref actions-table action)))
                                       (map
                                        (lambda (prob+nextstate-pair)
                                          (let* ((probability  (car prob+nextstate-pair))
                                                 (next-state   (cdr prob+nextstate-pair))
                                                 (Ut_next-state (vector-ref Ut (state->idx next-state))))
                                            (* probability Ut_next-state)))
                                     prob+nextstate-pairs))))
                                  actions)))))))
         (iota n-states))
        (let* ((rmse (vector-rmse Ut Ut+1)))
          (cond
           ((> reltol rmse)
            (print "converged on round "round)
            (print (gridworld-format-vector gw Ut+1))
            #t
            )
           (else
            ;;(print "Round "round":\n  Ut("Ut") -> Ut+1("Ut+1")")
            
            ;;(sleep 2)
            (loop Ut+1 (add1 round)))))))))





(let* ((gw1 (init-gridworld 3 4) ))
  (value-iteration gw1 gamma: 0.9))



