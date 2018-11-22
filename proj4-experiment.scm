(include "proj4-lib.scm")

(define (main)
  (let* ((rundir    (list-ref (argv) 1))
         (pspec     (with-input-from-file "proj4-spec.sexp" read)))
    (change-directory rundir)
    (let* ((spec-file "experiment.sexp")
           (start-t     (current-seconds))
           (max-moves 100)
           (spec        (with-input-from-file spec-file read))
           (algo        (alist-ref 'algo spec))
           (gamma       (alist-ref 'gamma spec))
           (gw-span     (alist-ref 'gridworld spec))
           (n-rows        (car gw-span))
           (n-cols        (cadr gw-span))
           (default-reward (alist-ref 'default-reward spec))
           (gw1 (init-gridworld n-rows n-cols
                                keep-outs: '((1 . 1))
                                default-reward: default-reward
                                s0: '(0 . 0)
                                sink-reward-alist: `(
                                                     (( ,(modulo -1 n-rows) . ,(modulo -1 n-cols) ) . 2) ;; opposite corner to s0
                                                     (( ,(modulo -2 n-rows) . ,(modulo -2 n-cols) ) . 1)))) ;; opposite corner to s0, moved 1 row and col inward
           (hyperparams (alist-ref algo (alist-ref 'algos pspec)))
           (hp-alist    (map (lambda (hp)
                               (cons hp (alist-ref hp spec)))
                             hyperparams)))
      (case algo
        ((value-iteration)
         (match (value-iteration gw1 gamma: gamma)
           ((policy utility rounds)
            (let* ((score (gridworld-score-policy gw1 policy max-moves: max-moves)))
                   
              (with-output-to-file "score.sexp" (lambda () (pp score)))
              (with-output-to-file "U.sexp"  (lambda () (pp utility)))
              (with-output-to-file "U.txt" (lambda ()
                                             (print (gridworld-format-vector gw1 utility flavor: 'num))))
              (with-output-to-file "PI.sexp"  (lambda () (pp policy)))
              (with-output-to-file "PI.txt"  (lambda () 
                                               (print (gridworld-policy-blurb gw1 policy))))
              
              
              (print (gridworld-policy-blurb gw1 policy))
              (print (gridworld-format-vector gw1 utility flavor: 'num))
              (print policy" score (mean stddev avg-moves): "score)))))

        ((policy-iteration)
         (match (policy-iteration gw1 gamma: gamma)
           ((policy utility rounds)
            (let* ((score (gridworld-score-policy gw1 policy max-moves: max-moves)))
                   
              (with-output-to-file "score.sexp" (lambda () (pp score)))
              (with-output-to-file "U.sexp"  (lambda () (pp utility)))
              (with-output-to-file "U.txt" (lambda ()
                                             (print (gridworld-format-vector gw1 utility flavor: 'num))))
              (with-output-to-file "PI.sexp"  (lambda () (pp policy)))
              (with-output-to-file "PI.txt"  (lambda () 
                                               (print (gridworld-policy-blurb gw1 policy))))
              
              
              (print (gridworld-policy-blurb gw1 policy))
              (print (gridworld-format-vector gw1 utility flavor: 'num))
              (print policy" score (mean stddev avg-moves): "score)))))
        ((q-learning)
         (match (ql-learn gw1 gamma 'zero ;; 'zero or 'heaven
                          epsilon-decay-factor: (alist-ref 'epsilon-decay-factor spec)
                          min-episodes: (alist-ref 'n-episodes spec)
                          alpha-update-method: (alist-ref 'alpha-update-method spec) ;; episodic or visitation
                          max-moves-per-episode: (alist-ref 'max-moves-per-episode spec)
                          ;;max-episodes: 1
                          )
           ((policy Q episodes)
            (let* ((score (gridworld-score-policy gw1 policy max-moves: (alist-ref 'max-moves-per-episode spec)))
                   (U (gridworld-Q->U gw1 Q)))
              (with-output-to-file "score.sexp" (lambda () (pp score)))
              (with-output-to-file "U.sexp"  (lambda () (pp U)))
              (with-output-to-file "U.txt" (lambda ()
                                             (print (gridworld-format-vector gw1 U flavor: 'num))))
              (with-output-to-file "PI.sexp"  (lambda () (pp policy)))
              (with-output-to-file "PI.txt"  (lambda () 
                                                (print (gridworld-policy-blurb gw1 policy))))
              (with-output-to-file "Q.sexp"  (lambda () (pp Q)))
              (with-output-to-file "Q.txt" (lambda () (print-Q gw1 Q)))

              
              
              (print "final-Q: ")
              (print-Q gw1 Q)
              (pp Q)
              
              (print "Q-learning episodes: "episodes)
              (print (gridworld-policy-blurb gw1 policy))
              (print "q-learning score (mean stddev avg-moves): "score)
                     
              (print (gridworld-format-vector gw1 U flavor: 'num))
              )))))
      (with-output-to-file "time.txt" (lambda () (print (- (current-seconds) start-t)))))))
                     

(main)
