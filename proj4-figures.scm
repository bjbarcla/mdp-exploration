(include "proj4-lib.scm")
(use sql-de-lite)
(use fmt)

(define (datum->org-cell x)
  (cond
   ((string? x) x)
   ((and (number? x) (exact? x)) (number->string x))
   ((and (number? x)) (fmt #f 
                           ;;(pad/left 8
                                     (num x 10 2) ;;truncate at hundredths
                           ;;          )
                           ))
   (else (->string x))))

(define (multiline-str->org str)
  (conc
   (string-join
    (map (lambda (s) (conc "    : " s))
         (string-split str "\n"))
    "\n")
   "\n"))


(define (row->org-row row)
  (conc "|"
        (string-join (map datum->org-cell row) "|")
        "|"))

(define (sql->rows db fields query-str bindings)
  (let* ((qs   (conc
                "select "
                (string-join fields ", ")
                " from results where "
                query-str))
         (stm  (sql db qs))
         (rows (apply query fetch-rows stm bindings)))
    rows))
    
(define (rows->orgtable rows)
  (string-join (map row->org-row rows) "\n"))

(define (sql->orgtable db fields query-str bindings  #!key (alt-field-names #f))
  (let* ((rows (cons
                (if alt-field-names alt-field-names fields)
                (sql->rows db fields query-str bindings))))
    (rows->orgtable rows)))

(define (gridworld-rewards-figure gw default-reward)
  (let* ((states  (gridworld-states gw))
         (sink-reward-al (gridworld-sink-reward-alist gw))
         (rewards (gridworld-rewards gw))
         (sinks   (gridworld-sinks gw))
         (keep-outs (gridworld-keep-outs gw))
         (state->idx-ht  (alist->hash-table
                          (map (lambda (idx)
                                 (cons (list-ref states idx) idx))
                               (iota (length states)))))
         (state->idx     (lambda (s) (hash-table-ref state->idx-ht s)))
         (reward-vec (list->vector
                      (map
                       (lambda (idx)
                         (let* ((state (list-ref states idx)))
                           (->string
                            (cond
                             ((member state keep-outs) "X")
                             ((member state sinks) (alist-ref state sink-reward-al equal?))
                             (else default-reward)
                             ))))
                      (iota (length states))))))
    (gridworld-vector->org-table gw reward-vec)
    ;;(gridworld-format-vector gw reward-vec flavor: 'string)
    ))



(define (experiment->learning-org-table rundir episodes score)
  (let* ((lrfile (conc rundir "/score-by-episode.sexp"))
         (max-samples 20)
         (lr (reverse (with-input-from-file lrfile read)))
         (n-lr (length lr))
         (idxs
          (if (< n-lr max-samples)
              (iota n-lr)
              (let* ((step (inexact->exact (round (/ n-lr max-samples))))
                     (idxs (let loop ((tidxs '(0)))
                             (if (> (+ step (car tidxs)) n-lr)
                                 tidxs
                                 (loop (cons (+ step (car tidxs)) tidxs))))))
                idxs))))
    (print "n-lr="n-lr "idxs="idxs)
         
    (rows->orgtable
     (cons (list "Episode number" "score" "stddev" "avg moves per episode")
           (reverse (cons (list episodes score) (map (lambda (x) (list-ref lr x)) idxs)))
           ))))

(define (str->file str filename)
  (with-output-to-file filename (lambda () (print str)))
  (print "Wrote "filename)
  #f)

(define (main)
  (let* ((epoch   (list-ref (argv) 1))
         ;;(results (with-input-from-pipe (conc "zcat "epoch"-results.sexp.gz") read))
         (dbfile  (conc "results-"epoch".sqlite3"))
         (db (open-database dbfile))
         (pspec (with-input-from-file (conc "proj4-spec-"epoch".sexp") read))
         (default-reward (car (alist-ref 'default-reward
                                         (alist-ref 'param-ranges
                                                    pspec))))
         (gw-dims-list '( ( SmallGW . (3 4) ) ( LargeGW . (6 8)) ))
         (gwitems    (map
                      (lambda (item)
                        (let* ((name (car item))
                               (dims (cdr item))
                               (n-rows (car dims))
                               (n-cols (cadr dims)))
                          (cons name
                                (init-gridworld n-rows n-cols
                                                keep-outs: '((1 . 1))
                                                default-reward: default-reward
                                                s0: '(0 . 0)
                                                sink-reward-alist:
                                                `((( ,(modulo -1 n-rows) . ,(modulo -1 n-cols) ) . 2)     ;; opposite corner to s0
                                                  (( ,(modulo -2 n-rows) . ,(modulo -2 n-cols) ) . 1)))))) ;; opposite corner to s0, moved 1 row and col inward
                      gw-dims-list)))
    (str->file
     (string-join
      `(
        
        "* Gridworld Environments"
        ,@(map
           (lambda (gwitem)
             (let* ((gwname (car gwitem)) (gw (cdr gwitem)))
               (conc
                "** "gwname"\n"
                (gridworld-rewards-figure gw default-reward)
                "\n\n")))
           gwitems)
        
        "\n* Value Iteration Results" 
        ,@(map
           (lambda (gwitem)
             (let* ((gwname    (car gwitem))
                    (gw        (cdr gwitem))
                    (gw-sql-name (string-join (map ->string (list (gridworld-n-rows gw) (gridworld-n-cols gw))) "x"))
                    (org-table (sql->orgtable db
                                              '("gamma" "score_mean" "score_avg_moves" "episodes" "time")
                                              "gridworld=? and algo=? and goal_reward=2 order by gamma"
                                              (list gw-sql-name "value-iteration")
                                              alt-field-names: '("gamma" "score" "avg moves per episode" "rounds" "runtime"))))
               
               (conc
                "** "gwname" Performance vs. Gamma\n"
                org-table
                "\n"
                "\n\n")))
           gwitems)
        
        "\n"
        "\n* Policy Iteration Results" 
        ,@(map
           (lambda (gwitem)
             (let* ((gwname    (car gwitem))
                    (gw        (cdr gwitem))
                    (gw-sql-name (string-join (map ->string (list (gridworld-n-rows gw) (gridworld-n-cols gw))) "x"))
                    (org-table (sql->orgtable db
                                              '("gamma" "score_mean" "score_avg_moves" "episodes" "time")
                                              "gridworld=? and algo=? and goal_reward=2 order by gamma"
                                              (list gw-sql-name "policy-iteration")
                                              alt-field-names: '("gamma" "score" "avg moves per episode" "rounds" "runtime"))))
               
               (conc
                "** "gwname" Performance vs. Gamma\n"
                org-table
                "\n"
                "\n\n")))
           gwitems)
        
        "\n* Q-Learning Results"
        ,@(map
           (lambda (gwitem)
             (let* ((gwname    (car gwitem))
                    (gw        (cdr gwitem))
                    (gw-sql-name (string-join (map ->string (list (gridworld-n-rows gw) (gridworld-n-cols gw))) "x"))
                    
                    ;; CREATE TABLE results (epoch, algo, gridworld, decoy_reward, goal_reward, gamma, episodes, epsilon_decay_factor, max_moves_per_episode, alpha_update_method, time, score_mean, score_stddev, score_avg_moves, rundir);
                    (alt-field-names (string-split "score|Q init method|Gamma|Training Episode Count|Epsilon Decay Factor|Alpha Update Method|Max moves to bankruptcy|Average moves per episode|Runtime (seconds)" "|"))
                    (org-table (sql->orgtable db
                                              '("score_mean" "qinit_method" "gamma" "episodes" "epsilon_decay_factor" "alpha_update_method" "max_moves_per_episode"   "score_avg_moves" "time")
                                              "gridworld=? and algo=? and goal_reward=2 order by score_mean desc limit 15"
                                              (list gw-sql-name "q-learning")
                                              alt-field-names: alt-field-names
                                              )))
               
               (conc
                "** "gwname" Q-learned policy scores (top 15)\n"
                org-table
                "\n"
                "\n\n")))
           gwitems)



        "\n* Selected policy and utility maps\n"
        ,@(map
           (lambda (gwitem)
             (let* ((gwname      (car gwitem))
                    (gw          (cdr gwitem))
                    (gw-sql-name (string-join (map ->string (list (gridworld-n-rows gw) (gridworld-n-cols gw))) "x"))
                    (rows        (map
                                  (lambda (algo)
                                    (car
                                     (sql->rows db '("algo" "score_mean" "rundir" "episodes")
                                               "gridworld=? and algo=? and goal_reward=2 order by score_mean desc limit 1"
                                               (list gw-sql-name algo))))
                                  (list "value-iteration" "policy-iteration" "q-learning"))))
               (pp rows)
               (string-join (map
                             (match-lambda
                              ((algo score rundir episodes)
                               (let* ((policy (with-input-from-file (conc rundir "/PI.sexp") read))
                                      (utility (with-input-from-file (conc rundir "/U.sexp") read)))
                                 (conc
                                  "\n** Policy Plot for "algo" experiment for "gwname" with top  score "(->string score)"\n"
                                  (multiline-str->org (gridworld-policy-blurb gw policy))
                                  "\n** Utility Plot for "algo" experiment for "gwname" with top score "(->string score)"\n"
                                  (multiline-str->org (gridworld-format-vector gw utility flavor: 'num))
                                  
                                ))))
                             rows)
                            "\n")))
           gwitems)
        

        
        
        "\n* Selected learning curves"
        ,@(map
           (lambda (gwitem)
             (let* ((gwname    (car gwitem))
                    (gw        (cdr gwitem))
                    (gw-sql-name (string-join (map ->string (list (gridworld-n-rows gw) (gridworld-n-cols gw))) "x"))
                    (rows      (sql->rows db '("score_mean" "rundir" "episodes")
                                          "gridworld=? and algo=? and goal_reward=2 order by score_mean desc limit ?"
                                          (list gw-sql-name "q-learning" 5))))
               (string-join (map
                             (match-lambda
                              ((score rundir episodes)
                               (conc "\n** Learning Curve for Q-learning experiment for "gwname" with score "(->string score)"\n"
                                     (experiment->learning-org-table rundir episodes score))))
                             rows)
                            "\n")))
               gwitems)




        "\n"))
     "figures/figures.org")))
(main)
     
  
