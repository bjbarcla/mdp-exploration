
(use typed-records)  ;; provides defstruct
(use srfi-69)        ;; provides hash tables
(use posix)          ;; provides sleep
(use fmt)            ;; provides string formatter
(use linear-algebra) ;; provide make-matrix invert-matrix and m* (matrix multiplier)
(use matchable)      ;; provides match destructuring operator
(use random-bsd)     ;; provides random-real and (randomize <seed>)

(randomize 42) ;; select random numbers reproducibly across runs

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

;; initialize a gridworld having n-rows rows and n-cols cols
;;   initial state at row=0, col=0
;;  
;;   by default:
;;     reward is -0.04
;;     place two sinks:
;;       extreme corner with reward 2
;;       another moved in 1 row and 1 col from extreme corner with reward 1
;;     place one keep-out at row=1, col=1

(define (init-gridworld n-rows n-cols
                        #!key
                      (default-reward -0.04)
                      (s0 '(0 . 0))  ;; initial state
                      (keep-outs '())
                      (sink-reward-alist `(
                               (( ,(modulo -1 n-rows) . ,(modulo -1 n-cols) ) . 2) ;; opposite corner to s0
                               (( ,(modulo -2 n-rows) . ,(modulo -2 n-cols) ) . 1) ;; opposite corner to s0, moved 1 row and col inward
                               )))

  
  (let* ((all-actions (gridworld-actions))
         (sinks (map car sink-reward-alist))
         (all-cell-states (cartesian-product (iota n-rows) (iota n-cols)))
         (tt-ht (make-hash-table))
         (reward-ht (make-hash-table)))
    
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
                  (dprint "Got transitions:" (hash-table->alist actions-table))))
              all-cell-states)
    
    (make-gridworld n-rows: n-rows n-cols: n-cols states: all-cell-states transitions: tt-ht rewards: reward-ht
                    sinks: sinks sink-reward-alist: sink-reward-alist keep-outs: keep-outs)))


(define (print-tt gw)
  ;; print transitions table
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
  ;; compute rms difference between two vectors
  (expt
   (apply +
          (map (lambda (x) (expt (apply - x) 2))
               (zip
                (vector->list avec)
                (vector->list bvec))))
   0.5))

(define (vectors-match? pvec cvec)
  ;; check if two vectors are identical
  (fold (lambda (p c prev)
          (if (not prev)
              #f
              (equal? p c)))
        #t
        (vector->list pvec)
        (vector->list cvec)))


(define (gridworld-vector->org-table gw vec #!key (flavor 'none) (cols (gridworld-n-cols gw)) (rows (gridworld-n-rows gw)))
  (let* ((states (gridworld-states gw))
         (n-states (length states))
         (state->idx-ht  (alist->hash-table
                          (map (lambda (idx)
                                 (cons (list-ref states idx) idx))
                               (iota n-states))))
         (state->idx    (lambda (s) (hash-table-ref state->idx-ht s))))
    (string-join
     (map (lambda (row)
            (conc "|"
                  (string-join
                   (map (lambda (col)
                          (let* ((state (cons row col))
                                 (idx   (state->idx state))
                                 (val   (vector-ref vec idx)))
                            (->string val)))
                        (iota cols))
                   "|") "|"))
          (iota rows))
     "\n")))
               
                  
  

(define (gridworld-format-vector gw vec #!key (flavor 'num) (cols (gridworld-n-cols gw)) (rows (gridworld-n-rows gw)))
  ;; print out a ascii table visualizing some aspect of gridworld represented by vector vec
  ;; contents of vector are shown by an n-rows x n-cols printout with row 0 on bottom
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
  ;; return string dipicting specified policy in state-space grid ascii table
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
  ;; given alist matching arg to value, return arg corresponding to max value
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
    ((up)    "^")
    ((down)  "v")
    ((left)  "<")
    ((right) ">")
    (else
     (print "invalid action ["action"]")
     (exit 1))))


(define (gridworld-U->PI gw U)
  ;; given utility vector, return policy vector
  (let* ((states            (gridworld-states              gw))
         (n-states          (length states)                   )
         (sinks             (gridworld-sinks               gw))
         (sink-reward-alist (gridworld-sink-reward-alist   gw))
         (R                 (gridworld-rewards             gw))
         (transitions       (gridworld-transitions         gw))
         (keep-outs         (gridworld-keep-outs           gw))
         (state->idx-ht  (alist->hash-table
                          (map (lambda (idx)
                                 (cons (list-ref states idx) idx))
                               (iota n-states))))
         (state->idx     (lambda (s) (hash-table-ref state->idx-ht s)))
         (policy            (make-vector n-states #f)))
    
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
                                            (Ut_next-state (vector-ref U (state->idx next-state))))
                                       (* probability Ut_next-state)))
                                   prob+nextstate-pairs)))))
                actions))
              (best-action (argmax-alist utils-by-action-alist))
              (best-next-util (alist-ref best-action utils-by-action-alist))
              )
         (vector-set! policy idx best-action)))
     (iota n-states))
    policy))
         

(define (value-iteration gw #!key (reltol 0.0001) (gamma 0.1))
  ;; perform value-iteration on gridworld, returning policy and utility vectors and a count of rounds required to converge
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
             (vector-set!
              Ut+1 idx 
              (+ r (* gamma
                      best-next-util)))))
         (iota n-states))
        (let* ((rmse (vector-rmse Ut Ut+1)))
          (cond
           ((> reltol rmse)
            (list policy Ut+1 round);;; return value
            )
           (else
            ;;(print "Round "round":\n  Ut("Ut") -> Ut+1("Ut+1")")
            
            ;;(sleep 2)
            (loop Ut+1 (add1 round)))))))))


(define (solve-lineq A B)
  ;; AX=B  ; given matrix A and column vector B: return column vector X
  ;; A^-1*A*X = A-1*B
  ;; X = A-1*B
  (dprint "A="A"; B="B)
  (dprint "begin solv for X")
  ;;(display ".")

  (let* ((Ainv (invert-matrix A)))
    (when (not Ainv)
      (print "Could not invert "A)
      (exit 1))
    (let* ((X   (m* Ainv B)))
      (dprint "X:"X)
      (dprint "done solv")
      X))) ;; return value


(define (m+= M r c v)
  ;; add value v to element(r,c) if matrix M
  (let* ((cur-v (or (matrix-ref M r c)0))
         (new-v (+ cur-v v)))
    (matrix-set! M r c new-v)
    new-v))

(define (evaluate-policy Pt gw gamma)
  ;; returns Ut for policy evaluation step of policy iteration
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
    ;;   rewrite to suit AX = B so we can solve system of lin eq's using linear-algebra
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
      
      Ut))) ;; return value

(define (improve-policy Ut gw)
  ;; perform improve policy step for policy iteration
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
    
    (list->vector ;; start return value
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
          (iota n-states)) ;; return value 
     )))

(define (policy-iteration gw #!key (gamma 0.1))
  ;; perform policy iteration on gridworld, returning policy and utility vectors and a count of rounds required to converge
  (let* ((states            (gridworld-states              gw))
         (actions           (gridworld-actions)               )
         (n-states          (length states)                   ))
         
    (let loop ((Pt   (list->vector (map (lambda (idx) (car actions)) (iota n-states))))  (round 1))
      (let*   ((Ut   (evaluate-policy Pt gw gamma))
               (Pt+1 (improve-policy Ut gw)))
        (cond
         ((vectors-match? Pt Pt+1)
          (print "")
          (list Pt Ut round)) ;; return val
         (else (loop Pt+1 (add1 round))))))))

;; optimistic initialization - Encourages systematic exploration of states and actions
;; Optimistic Initialisation: Model-Free RL
(define (ql-initialize-Qhat gw gamma strategy)
  ;; return Initialized Q estimate matrix for Q-learning  (rows represent states, columns represent actions)
  (let* ((states    (gridworld-states gw))
         (n-states  (length states))
         (r-hash    (gridworld-rewards gw))
         (r-list    (map (lambda (state) (hash-table-ref r-hash state)) states))
         (rmax      (apply max r-list))
         (heaven    (/ rmax (- 1 gamma)))
         (actions   (gridworld-actions))
         (n-actions (length actions))
         (sinks             (gridworld-sinks               gw))
         (sink-reward-alist (gridworld-sink-reward-alist   gw))         
         (Qhat0     (list->vector
                     (map (lambda (state-idx)
                            (let* ((state (list-ref states state-idx))
                                   (reward
                                    (cond
                                     ((member state sinks)
                                      (alist-ref state sink-reward-alist equal?))
                                     ((eq? strategy 'heaven)
                                      heaven)
                                     ((eq? strategy 'zero)
                                      0)
                                     (else
                                      (print "Unknown Qhat init strategy ["strategy"]")
                                      (exit 1)))))
                              (dprint "Reward="reward" state-idx="state-idx" state="state)
                              (list->vector
                               (map (lambda (action-idx)
                                      reward)
                                    (iota n-actions)))))
                          (iota n-states)))))
    Qhat0)) ;; return value

(define  (ql-initialize-PIhat gw)
  ;; return initial policy estimate for Q-learning
  (let* ((states   (gridworld-states gw))
         (n-states (length states)))
    (list->vector ;; return value
     (map 
      (lambda (a)
        'up) ;; default action is up
      (iota n-states)))))


(define (select-from-discrete-random-variable X)
  ;; does what it says on the tin.  Input X is an alist of (probability . value).  sum(probability) assumed to be 1.
  (let* ((rval       (random-real))
         (first-item (car X))
         (rest-items (cdr X)))
    (let loop ((probability (car first-item))
               (value       (cdr first-item))
               (cdf-index   0)
               (rest        rest-items))
      (cond
       ((< rval (+ probability cdf-index))
        value) ;; return value
       ((null? rest)
        (print "ERROR: rval="rval" cdf-index="cdf-index" -- out of random items without selecting an item!")
        (print "X="X)
        (exit 1))
       (else
        (loop (caar rest)
              (cdar rest)
              (+ probability cdf-index)
              (cdr rest)))))))

(define (gridworld-get-next-state gw state action)
  ;; for Q-learning, given a gridworld, return next-state for given current state and action.  
  (let* ((transitions            (gridworld-transitions gw))
         (actions-table          (hash-table-ref transitions state))
         (nextstate-distribution (hash-table-ref actions-table action))) 
    (select-from-discrete-random-variable nextstate-distribution)))

(define (gridworld-Q->U gw Q)
  ;; given Q matrix, return utility vector
  (let* ((states    (gridworld-states gw))
         (n-states  (length states))
         (actions   (gridworld-actions))
         (n-actions (length actions))
         (utility-list
          (map (lambda (state-idx)
                 (let* ((state         (list-ref states state-idx))
                        (Q-state-vec   (vector-ref Q state-idx)))
                   (apply max (vector->list Q-state-vec))))
               (iota n-states)))
         (PI (list->vector utility-list)))
    PI)) ;; return value

(define (gridworld-Q->PI gw Q)
    ;; given Q matrix, return policy vector
  (gridworld-U->PI gw (gridworld-Q->U gw Q)))


(define (ql-simulate-episode gw Qhat PIhat gamma alpha-per-episode epsilon round s0 Q-visits alpha-update-method max-moves)
  ;; simulate an episode for Q-learning
  ;; returns (list Qhat' PIhat')
  (let* ((states         (gridworld-states gw))
         (n-states       (length states))
         (r-hash         (gridworld-rewards gw))
         (actions        (gridworld-actions))
         (n-actions      (length actions))
         (state->idx-ht  (alist->hash-table
                          (map (lambda (idx)
                                 (cons (list-ref states idx) idx))
                               (iota n-states))))
         (state->idx     (lambda (s) (hash-table-ref state->idx-ht s)))
         (action->idx-ht (alist->hash-table
                          (map (lambda (idx)
                                 (cons (list-ref actions idx) idx))
                               (iota n-actions))))
         (action->idx    (lambda (a) (hash-table-ref action->idx-ht a)))                                 
         (sink-idxs      (map state->idx (map car (gridworld-sink-reward-alist gw))))
         (idx-is-sink?   (lambda (idx) (member idx sink-idxs)))
         (r-vec          (list->vector (map (lambda (state) (hash-table-ref r-hash state)) states)))
         ;; cheesy deep copy Qhat -> QhatP (we're gonna mutate this...)
         (QhatP          (list->vector   
                          (map (lambda (vec)
                                 (list->vector (vector->list vec)))
                               (vector->list Qhat)))))
    
    (let loop ((state-idx (state->idx s0))
               (t 0))
      (let* (;; choose epsilon greedy action
             (bankruptcy          (cond
                                  ((not max-moves) #f)
                                  ((> t max-moves) #t)
                                  (else #f)))
             (exploit-action     (vector-ref PIhat state-idx))
             (explore-actions      (lset-difference eq? actions (list exploit-action)))
             (choose-to-explore? (< (random-real) epsilon))
             (action             (if choose-to-explore?
                                    (list-ref explore-actions (random (sub1 n-actions)))
                                    exploit-action))
             (action-idx         (action->idx action))
             ;; get s'
             (state (list-ref states state-idx))
             (next-state (gridworld-get-next-state gw state action))
             (next-state-idx (state->idx next-state))
             ;; set up to learn
             (reward                   (vector-ref r-vec state-idx))
             (next-Qhats-nextstate-vec (vector-ref Qhat next-state-idx))
             (best-next-qval           (apply max
                                              (map (lambda (next-action-idx)
                                                     (vector-ref next-Qhats-nextstate-vec next-action-idx))
                                                   (iota n-actions))))
             (qval-to-learn            (+ reward (* gamma best-next-qval)))
             (old-qval                 (vector-ref (vector-ref Qhat state-idx) action-idx))
             (visits                   (let* ((v (matrix-ref Q-visits state-idx action-idx)))
                                         (matrix-set! Q-visits state-idx action-idx (add1 v))
                                         (add1 v)))
             (alpha-per-visit          (/ 1 visits))
             (alpha                    (case alpha-update-method
                                         ((episodic)
                                          alpha-per-episode)
                                         ((visitation)
                                          alpha-per-visit)
                                         (else
                                          (print "Unknown alpha-update-method: ["alpha-update-method"]")
                                          (exit 1))))
             (new-qval                 (+
                                        (* (- 1 alpha) old-qval)
                                        (* alpha qval-to-learn))))
        ;; learn
        (dprint "Q(s="state",a="action"): old-qval="old-qval" ; new-qval="new-qval)
        (dprint "best-next-qval="best-next-qval" qval-to-learn="qval-to-learn" reward="reward)
        (vector-set! (vector-ref QhatP state-idx) action-idx new-qval)

        (if (eq? 0 (modulo t 1000 ))
            (if (not (eq? t 0 )) (print "episode move #"t)))
        
        ;; proceed
        (cond
         (bankruptcy   ;; have we exceeded predefined max-moves? if so, end episode
          (list QhatP (gridworld-Q->PI gw QhatP))) 
         ((idx-is-sink? next-state-idx) ;; are he headed into a sink?  if so, end episode
          (list QhatP (gridworld-Q->PI gw QhatP)))
         (else  ;; proceed to next-state
          (loop next-state-idx (add1 t))))))))

(define (ql-episode-num->alpha episode-num)
  ;; calculate alpha for Q-learning given current episode number
  (/ 1 episode-num))

(define (ql-decay-epsilon epsilon decay-factor)
  ;; decay epsione for GLIE learning
  (* decay-factor epsilon))

(define (Mzero A)
  ;; create matrix with all elements initialized to 0, matching dimensions of input matrix
  (list->vector
   (map (lambda (row-vector)
          (list->vector
           (map (lambda (val)
                  0)
                (vector->list row-vector))))
        (vector->list A))))
  

(define (ql-learn gw gamma init-strategy
                  #!key
                  (min-episodes 1)
                  (max-episodes #f)
                  (epsilon-decay-factor 0.99)
                  (alpha-update-method #f)
                  (max-moves-per-episode #f))
  ;; perform Q-learning for gridworld gw
  (let* ((startable-states (lset-difference
                            equal?
                            (gridworld-states gw)
                            (append (gridworld-sinks gw) (gridworld-keep-outs gw))))
         (s0-onestate-probability (/ 1 (length startable-states)))
         (s0-distribution (map (lambda (state) (cons s0-onestate-probability state)) startable-states))
         (get-s0 (lambda () (select-from-discrete-random-variable s0-distribution)))
         (Qhat0    (ql-initialize-Qhat gw gamma init-strategy))
         (Q-visits (Mzero Qhat0))
         (PIhat0   (ql-initialize-PIhat gw))
         (epsilon0 1)
         (alpha0   1))

    (let loop ((Qhat Qhat0)
               (PIhat PIhat0)
               (episodic-alpha alpha0)
               (epsilon epsilon0)
               (episode-num 1))
      (match (ql-simulate-episode gw Qhat PIhat gamma episodic-alpha epsilon round (get-s0) Q-visits alpha-update-method max-moves-per-episode)
        ((QhatP PIhatP)  ;; P suffix means "prime" or "next"
         (cond
          ((or
            (and
             (not (< episode-num min-episodes))
             (vectors-match? PIhatP PIhat))

            (and
             max-episodes
             (>= episode-num max-episodes)))

         (list PIhatP QhatP episode-num)) ;; return value
          (else
           (let* ((episode-numP (add1 episode-num))
                  (alphaP       (/ 1 episode-numP))
                ;;(alphaP 1)
                  (epsilonP     (ql-decay-epsilon epsilon epsilon-decay-factor)))
             (loop QhatP PIhatP alphaP epsilonP episode-numP)))))
        (else
         (print "bad result from simulate-episode; unimplemented probably")
         (exit 1))))))

(define (print-Q gw Q)
  ;; display Q matrix for human 
  (let* ((actions (gridworld-actions))
         (states (gridworld-states gw)))
    (print (string-join
            (map (lambda (state-idx)
                   (let* ((state (list-ref states state-idx)))
                     (conc (->string state) ": "
                           (string-join
                            (map (lambda (action-idx)
                                   (let* ((action (list-ref actions action-idx))
                                          (val (matrix-ref Q state-idx action-idx)))
                                     (conc (->string action)"="
                                           (fmt #f (pad/left 5 (num val 10 2)))
                                           )))
                                 (iota (length actions)))
                            " "))))
                 (iota (length states)))
            "\n"))))

(define (gridworld-try-policy gw policy #!key (max-moves #f))
  ;; given a gridworld and policy, run an eposide, return sum of rewards over each state visited and count of moves.  No discounting here.
  (let* ((s0 '(0 . 0))
         (states         (gridworld-states gw))
         (n-states       (length states))
         (state->idx-ht  (alist->hash-table
                          (map (lambda (idx)
                                 (cons (list-ref states idx) idx))
                               (iota n-states))))
         (state->idx     (lambda (s) (hash-table-ref state->idx-ht s)))
         (sinks          (gridworld-sinks gw))
         (keep-outs      (gridworld-keep-outs gw))
         (rewards        (gridworld-rewards gw)))
    (let loop ((current-state s0)  (balance 0.0) (moves 0))
      
      ;;(print current-state)
      (let* ((bankruptcy
              (cond
               ((not max-moves) #f)
               ((> moves max-moves) #t)
               (else #f)))
             (this-reward  (hash-table-ref rewards current-state))
             (new-balance  (+ this-reward balance))
             )
        (cond
         ((or (member current-state sinks) bankruptcy)
          ;;(exit 1)
          (list new-balance moves)) ;; return value
         ((member current-state keep-outs)
          (print "Error: policy led agent into a keepout.  Abort.")
          (exit 1)
          )
         (else
          (let* ((action       (vector-ref policy (state->idx current-state)))
                 (next-state   (gridworld-get-next-state gw current-state action)))
            ;;(print current-state"->"action"->"next-state)
            (loop next-state new-balance (add1 moves)))))))))
                  

(define (gridworld-score-policy gw policy #!key (n 100) (max-moves #f))
  ;; given a gridworld and policy, perform n episodes;  return reward mean, reward stddev, moves per episode mean
  (match (let loop ((remaining n) (scores '()) (moves '()))
           (if (eq? 0 remaining)
               (list scores moves)
               (match (gridworld-try-policy gw policy max-moves: max-moves)
                 ((total-reward move-count)
                  (loop (sub1 remaining) (cons total-reward scores) (cons move-count moves))))))
    ((scores-list moves-list)
     (let* ((mean   (/ (apply + scores-list) n))
            (stddev
             (expt (/ (apply + (map (lambda (x) (expt (- x mean) 2)) scores-list)) n) 0.5))
            (avg-moves (/ (apply + moves-list) n)))
       
       (list mean stddev avg-moves))))) ;; return value
    
