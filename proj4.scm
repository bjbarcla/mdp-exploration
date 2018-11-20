#!/usr/bin/env csi -s

;;(use matchable)
(use typed-records)  ;; provides defstruct
(use srfi-69)        ;; provides hash tables
(use posix)          ;; provides sleep
(use fmt)            ;; provides string formatter
(use linear-algebra) ;; provide make-matrix invert-matrix and m* (matrix multiplier)
(use matchable)      ;; provides match destructuring operator

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
    sink-reward-alist
    sinks
    keep-outs
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
    
    (make-gridworld n-rows: n-rows n-cols: n-cols states: all-cell-states transitions: tt-ht rewards: reward-ht
                    sinks: sinks sink-reward-alist: sink-reward-alist keep-outs: keep-outs)))


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

(define (vector-stable? pvec cvec)
  (fold (lambda (p c prev)
          (if (not prev)
              #f
              (equal? p c)))
        #t
        (vector->list pvec)
        (vector->list cvec)))


(define (gridworld-format-vector gw vec #!key (flavor 'num) (cols (gridworld-n-cols gw)) (rows (gridworld-n-rows gw)))
  (let* ((states        (gridworld-states gw))
         (n-states      (length states))
         (state->idx-ht (make-hash-table))
         (state->idx    (lambda (s) (hash-table-ref state->idx-ht s)))) ;; TODO: vectorize hash tables for speedup
    ;; initialize lookup idx by state
    (for-each (lambda (idx)
                (hash-table-set! state->idx-ht (list-ref states idx) idx))
              (iota n-states))

    (let* ((cell-width (cond
                        ((eq? flavor 'num) 8)
                        ((eq? flavor 'string) (apply max (map string-length (vector->list vec))))
                        (else
                         (print "2 unknown flavor: ["flavor"]")
                         (exit 1)
                         )))
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
           (row-divider3
            (conc
             "\nI"
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
                                          ((eq? flavor 'string)
                                           (fmt #f (pad cell-width (->string val))))
                                          (else
                                           (print "unknown flavor: ["flavor"]")
                                           (exit 1))))))
                                    (iota cols))
                               " | ")
                              " |" (if (eq? row 0) row-divider3 row-divider)))
                      (reverse (iota rows))) ;; bottom row is 0 ; top row is n-rows-1, so print out last row first.
                 "\n")))))
      ;;(print rv)
      ;;(exit 1)
      rv)))

(define (gridworld-policy-blurb gw Pt)
  (let* ((states            (gridworld-states              gw))
         (n-states          (length states)                   )
         (sinks             (gridworld-sinks               gw))
         (sink-reward-alist (gridworld-sink-reward-alist   gw))
         (keep-outs         (gridworld-keep-outs           gw))
         (state->idx-ht (make-hash-table))
         (state->idx    (lambda (s) (hash-table-ref state->idx-ht s)))) ;; TODO: vectorize hash tables for speedup


    ;; initialize lookup idx by state
    (for-each (lambda (idx)
                (hash-table-set! state->idx-ht (list-ref states idx) idx))
              (iota n-states))

    (let ((policy-str
           (list->vector
            (map (lambda (idx)
                   (let ((action (vector-ref Pt idx))
                         (state  (list-ref states idx)))
                     (cond
                      ((member state keep-outs) "X")
                      ((member state sinks)
                       (->string (alist-ref state sink-reward-alist equal?)))
                      ((eq? 'up    action) "^")
                      ((eq? 'down  action) "v")
                      ((eq? 'left  action) "<")
                      ((eq? 'right action) ">")
                      (else
                       (print "ERROR bad state ["state"] or bad policy action ["action"]")
                       (exit 1)))))
                 (iota n-states)))))
      (gridworld-format-vector gw policy-str flavor: 'string))))

  
(define (argmax-alist al)
  (let loop ((curargmax #f) (curmax #f) (rest-al al))
    (cond
     ((null? rest-al)
      curargmax)
     ((or (not curmax) (> (cdar rest-al) curmax))
      (loop (caar rest-al) (cdar rest-al) (cdr rest-al)))
     (else
      (loop curargmax curmax (cdr rest-al)))))) 

(define (action->string a)
  (case a
    ((up) "^")
    ((down) "v")
    ((left) "<")
    ((right) ">")
    (else
     (print "invalid action ["action"]")
     (exit 1))))


(define (value-iteration gw #!key (reltol 0.0001) (gamma 0.1))
  (let* ((states            (gridworld-states              gw))
         (n-states          (length states)                   )
         (sinks             (gridworld-sinks               gw))
         (sink-reward-alist (gridworld-sink-reward-alist   gw))
         (keep-outs         (gridworld-keep-outs           gw))
         (transitions       (gridworld-transitions         gw))
         (R                 (gridworld-rewards             gw))
         (state->idx-ht     (make-hash-table))
         (state->idx        (lambda (s) (hash-table-ref state->idx-ht s)))) ;; TODO: vectorize hash tables for speedup

    ;; initialize lookup idx by state
    (for-each (lambda (idx)
                (hash-table-set! state->idx-ht (list-ref states idx) idx))
              (iota n-states))
         
    (let loop ((Ut (make-vector n-states 0)) (round 1))
      (let*   ((Ut+1 (make-vector n-states 0))
               ;;(policy-str (make-vector n-states "xxx"))
               (policy     (make-vector n-states #f)))
               
        (for-each
         (lambda (idx)
           (let* ((s  (list-ref states idx))
                  (r  (hash-table-ref R s))
                  (actions-table (hash-table-ref transitions s))
                  (actions (hash-table-keys actions-table))
                  (utils-by-action-alist
                   (map
                    (lambda (action)
                      (cons action (apply
                                    +
                                    (let ((prob+nextstate-pairs (hash-table-ref actions-table action)))
                                      (map
                                       (lambda (prob+nextstate-pair)
                                         (let* ((probability  (car prob+nextstate-pair))
                                                (next-state   (cdr prob+nextstate-pair))
                                                (Ut_next-state (vector-ref Ut (state->idx next-state))))
                                           (* probability Ut_next-state)))
                                       prob+nextstate-pairs)))))
                    actions))
                  (best-action (argmax-alist utils-by-action-alist))
                  (best-next-util (alist-ref best-action utils-by-action-alist))
                  )

             (vector-set! policy idx best-action)
             ;; (vector-set! policy-str idx
             ;;              (cond
             ;;               ((member s sinks)
             ;;                (->string (alist-ref s sink-reward-alist equal?)))
             ;;               ((member s keep-outs)
             ;;                "X")
             ;;               (else
             ;;                (action->string best-action)))

             ;;              )
             (vector-set!
              Ut+1 idx 
              (+ r (* gamma
                      best-next-util)))))
         (iota n-states))
        (let* ((rmse (vector-rmse Ut Ut+1)))
          (cond
           ((> reltol rmse)
            ;;(print "converged on round "round)
            ;;(print (gridworld-format-vector gw Ut+1 flavor: 'num))
            ;;(print (gridworld-policy-blurb  gw policy))
            ;;(print (gridworld-format-vector gw policy-str flavor: 'string))
            (list policy Ut+1 round);;; return value
            )
           (else
            ;;(print "Round "round":\n  Ut("Ut") -> Ut+1("Ut+1")")
            
            ;;(sleep 2)
            (loop Ut+1 (add1 round)))))))))


(define (solve-lineq A B)
  ;; AX=B
  ;; A^-1*A*X = A-1*B
  ;; X = A-1*B
  ;;(print A)";;"B)
  (dprint "begin solv")
  (display ".")

  (let* ((Ainv (invert-matrix A)))
    (when (not Ainv)
      (print "Could not invert "A)
      (exit 1))
    (let* ((X   (m* Ainv B)))
      (dprint "X:"X)
      (dprint "done solv")
      X)))


(define (m+= M r c v)
  (let* ((cur-v (or (matrix-ref M r c)0))
         (new-v (+ cur-v v)))
    (matrix-set! M r c new-v)
    new-v))

(define (evaluate-policy Pt gw gamma)
  ;; returns Ut
  (let* ((states            (gridworld-states              gw))
         (actions           (gridworld-actions)               )
         (n-states          (length states)                   )
         (sinks             (gridworld-sinks               gw))
         (sink-reward-alist (gridworld-sink-reward-alist   gw))
         (keep-outs         (gridworld-keep-outs           gw))
         (transitions       (gridworld-transitions         gw))
         (R                 (gridworld-rewards             gw))
         (A                 (make-matrix n-states n-states   ))
         (B                 (make-matrix n-states 1          ))
         (state->idx-ht     (make-hash-table))
         (state->idx        (lambda (s) (hash-table-ref state->idx-ht s)))) ;; TODO: vectorize hash tables for speedup
    
    ;; initialize lookup idx by state
    (for-each (lambda (idx)
                (hash-table-set! state->idx-ht (list-ref states idx) idx))
              (iota n-states))

    ;; initialize A and B
    (for-each
     (lambda (r)
       (matrix-set! B r 0 0)
       (for-each
        (lambda (c)
          (matrix-set! A r c 0))
        (iota n-states)))
     (iota n-states))

    ;; populate A and B
    ;;
    ;; Ut(s) = R(s) + gamma*sum_over_s'[ T(s,Pt(s),s'))*Ut(s') )
    ;;   rewrite to suit AX = B
    ;; R(s) = Ut(s) - gamma*sum_over_s'[ T(s,Pt(s),s'))*Ut(s') )
    ;;
    (for-each
     (lambda (idx)
       (let* ((state    (list-ref states idx))
              (reward   (hash-table-ref R state))
              (action   (vector-ref Pt idx))
              (actions-table (hash-table-ref transitions state))
              (prob+nextstate-pairs (hash-table-ref actions-table action)))
         (matrix-set! B idx 0 reward) ;; R(s)
         (m+= A idx idx 1)         ;; Ut(s)
         (for-each (lambda (prob+nextstate-pair)
                     (let* ((probability     (car prob+nextstate-pair))
                            (next-state      (cdr prob+nextstate-pair))
                            (next-state-idx  (state->idx next-state)))
                       ;; - gamma*sum_over_s'[ T(s,Pt(s),s'))*Ut(s') )
                       (m+= A idx next-state-idx (* -1 gamma probability))))
                   prob+nextstate-pairs)))
     (iota n-states))
    ;; solve for Ut
    (let* ((Ut (vector-ref (matrix-transpose (solve-lineq A B)) 0)))
      (dprint "Got Ut="Ut)
      
      Ut)))

(define (improve-policy Ut gw)
  ;; Pt+1 = argmax_a[ sum_over( T(s,a,s')*Ut(s') ) ]
  (let* ((states            (gridworld-states gw))
         (actions           (gridworld-actions))
         (transitions       (gridworld-transitions         gw))
         (n-states          (length states))
         (state->idx-ht     (make-hash-table))
         (R                 (gridworld-rewards gw))
         (state->idx        (lambda (s) (hash-table-ref state->idx-ht s)))) ;; TODO: vectorize hash tables for speedup
    ;; initialize lookup idx by state
    (for-each (lambda (idx)
                (hash-table-set! state->idx-ht (list-ref states idx) idx))
              (iota n-states))
    
    (list->vector
     (map (lambda (idx)
            (let* ((s             (list-ref states idx))
                   (actions-table (hash-table-ref transitions s))
                   (actions       (hash-table-keys actions-table))
                   (r             (hash-table-ref R s))
                   (utils-by-action-alist
                    (map
                     (lambda (action)
                       (cons action (apply
                                     +
                                     (let ((prob+nextstate-pairs (hash-table-ref actions-table action)))
                                       (map
                                        (lambda (prob+nextstate-pair)
                                          (let* ((probability  (car prob+nextstate-pair))
                                                 (next-state   (cdr prob+nextstate-pair))
                                                 (Ut_next-state (vector-ref Ut (state->idx next-state))))
                                            (* probability Ut_next-state)))
                                        prob+nextstate-pairs)))))
                     actions))
                   (best-action (argmax-alist utils-by-action-alist)))
              ;; Pt+1 = argmax_a[ sum_over( T(s,a,s')*Ut(s') ) ]
              best-action))
          (iota n-states)))))

(define (policy-iteration gw #!key (gamma 0.1))
  (let* ((states            (gridworld-states              gw))
         (actions           (gridworld-actions)               )
         (n-states          (length states)                   ))
         
    (let loop ((Pt   (list->vector (map (lambda (idx) (car actions)) (iota n-states))))  (round 1))
      (let*   ((Ut   (evaluate-policy Pt gw gamma))
               (Pt+1 (improve-policy Ut gw)))
        (cond
         ((vector-stable? Pt Pt+1)
          (print "")
          ;;(print "Arrived at policy after "round" rounds")

          (list Pt Ut round) ;; return val

          )
         (else (loop Pt+1 (add1 round))))))))

(define (initialize-Qhat gw)
  #f)
(define  (initialize-PIhat gw)
  #f)

(define (simulate-episode gw Qhat PIhat gamma alpha round s0)
  ;; returns Qhat' PIhat'
  #f
  )

(define (episode-num->alpha episode-num)
  (/ 1 episode-num))

(define (decay-epsilon epsilon)
  (* 0.99 epsilon))

(define (Q-learning gw gamma)
  (let* ((Qhat0  (initialize-Qhat gw))
         (PIhat0 (initialize-PIhat gw))
         (epsilon0 1)
         (alpha0 1)
         (s0 '   (0 . 0)))
  (let loop ((Qhat Qhat0) (PIhat PIhat0) (alpha alpha0) (epsilon epsilon0) (episode-num 0))
    (match (simulate-episode gw Qhat PIhat gamma alpha round s0)
      ((QhatP PIhatP)  ;; P suffix means "prime" or "next"
       (cond
        ((vector-stable? PIhatP PIhat)
         (list PIhatP QhatP) ;; return value
        (else
         (let* ((episide-numP (add1 episode-num))
                (alphaP       (/ 1 episode-numP))
                (epsilonP     (decay-epsilon epsilon)))
           (loop QhatP PIhatP alphaP epsilonP episode-numP))))))
      (else
       (print "bad result from simulate-episode; unimplemented probably")
       (exit 1))))))

(define (main)
  (let* ((gamma 0.9)
         (gw1 (init-gridworld 3 4 keep-outs: '((1 . 1))) ))
    
    (match (value-iteration gw1 gamma: gamma)
      ((policy utility rounds)
       (print (gridworld-policy-blurb gw1 policy))
       (print (gridworld-format-vector gw1 utility flavor: 'num)))
      )
    
    (match (policy-iteration gw1 gamma: gamma)
      ((policy utility rounds)
       (print (gridworld-policy-blurb gw1 policy))
       (print (gridworld-format-vector gw1 utility flavor: 'num))))

    (match (Q-learning gw1 gamma)
      ((PIhat Qhat)
       (print (gridworld-policy-blurb gw1 policy))))))

(main)



