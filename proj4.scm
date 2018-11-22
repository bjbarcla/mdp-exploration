#!/usr/bin/env csi -s
(include "proj4-lib.scm")

(define (main)
  (let* ((gamma 0.99)
         (gw1 (init-gridworld 8 6
                              keep-outs: '((1 . 1))
                              default-reward: -0.04
                              ) ))
    
    (match (value-iteration gw1 gamma: gamma)
      ((policy utility rounds)
       (print (gridworld-policy-blurb gw1 policy))
       ;;(print (gridworld-format-vector gw1 utility flavor: 'num))

      (print "val-iter score (mean stddev avg-moves): "(gridworld-score-policy gw1 policy max-moves: 100))
      )
      )
    
    ;;(match (policy-iteration gw1 gamma: gamma)
    ;;  ((policy utility rounds)
    ;;   (print (gridworld-policy-blurb gw1 policy))
    ;;   (print (gridworld-format-vector gw1 utility flavor: 'num))))

    (match (ql-learn gw1 gamma 'zero ;; 'zero or 'heaven
                     epsilon-decay-factor: 0.999
                     min-episodes: 10000
                     alpha-update-method: 'visitation ;; episodic or visitation
                     max-moves-per-episode: 100
;                     max-episodes: 1
                     )
      ((policy Q episodes)
       ;;(print "final-Q: ")
       ;;(print-Q gw1 Q)
       ;;(pp Q)

       ;;;(print "Q-learning episodes: "episodes)
       ;;;(print (gridworld-policy-blurb gw1 policy))
       (print (gridworld-policy-blurb gw1 policy))
       (print "q-learning score (mean stddev avg-moves): "(gridworld-score-policy gw1 policy max-moves: 100))

       ;;;(print (gridworld-format-vector gw1 (gridworld-Q->U gw1 Q) flavor: 'num))
       
       ))
    ))

(main)



