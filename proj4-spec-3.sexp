(
 (algos .
  (
  (q-learning . (gridworld decoy-reward goal-reward default-reward
                   gamma n-episodes epsilon-decay-factor
                   max-moves-per-episode alpha-update-method qinit-method))

   (value-iteration .  (gridworld decoy-reward goal-reward default-reward
                     gamma))

   (policy-iteration . (gridworld decoy-reward goal-reward default-reward
                     gamma))

  )
 ) 
 (param-ranges .
  (
  ;;   (gridworld .             ((2 3) (3 4) (5 7) (6 8)))
   (gridworld .             ((3 4) (6 8)))   
   (gamma .                 (0.9 0.8 0.7 0.6 0.5))
   (n-episodes .            (10 100 1000 10000 100000))
   (epsilon-decay-factor .  (0.9 0.99 0.999 0.9999))
   (decoy-reward .          (1))
   (goal-reward .           (2 4))
   (default-reward .        (-0.04))
   (max-moves-per-episode . (50 500))
   (qinit-method          . (zero heaven))
   (alpha-update-method .   (visitation episodic))
  )
 )
)
