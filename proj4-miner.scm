(include "proj4-lib.scm")
(use sql-de-lite)
(use posix)
;(import chicken scheme)


(define (create-clobber-db dbfile)
  (if (file-exists? dbfile) (delete-file dbfile))
  (let* ((db (open-database dbfile))
         (schema-sql "create table results (epoch, algo, gridworld, decoy_reward, goal_reward, gamma, episodes, epsilon_decay_factor, max_moves_per_episode, alpha_update_method, time, score_mean, score_stddev, score_avg_moves, rundir);")
         (stm (sql db schema-sql))
         )
    (query fetch-alists stm)
    db))
    
      

(let* ((files '("score.sexp" "time.txt" "PI.sexp" "U.sexp"))
       (dbfile "results.sqlite3")
       (db (create-clobber-db dbfile))
       (spec    (with-input-from-file "proj4-spec.sexp" read))
       (results (with-input-from-file (cadr (argv)) read))
       (outfile (list-ref (argv) 2))
       (out
        (with-transaction db
              (lambda ()
                (filter-map (lambda (exp)
                              (let* ((rundir (alist-ref 'rundir exp))
                                     (af (map (lambda (f) (file-exists? (conc rundir "/" f))) files))
                                     (complete? (not (member #f af))))
                                (if complete?
                                    (let* ((time  (with-input-from-file (conc rundir "/time.txt") read))
                                           (score (with-input-from-file (conc rundir "/score.sexp") read))
                                           (PI    (with-input-from-file (conc rundir "/PI.sexp") read))
                                           (U     (with-input-from-file (conc rundir "/U.sexp") read))
                                           (gridworld (alist-ref 'gridworld exp))
                                           (insert-sql (conc "insert into results values("
                                                             (alist-ref 'epoch exp)",'"
                                                             (->string (alist-ref 'algo exp))"', '"
                                                             (conc (->string (car gridworld)) "x" (->string (cadr gridworld))) "', "
                                                             (alist-ref 'decoy-reward exp)", "
                                                             (alist-ref 'goal-reward exp)", "
                                                             (alist-ref 'gamma exp)", "
                                                             (or (alist-ref 'n-episodes exp) 0)", "
                                                             (or (alist-ref 'epsilon-decay-factor exp) 0)", "
                                                             (or (alist-ref 'max-moves-per-episode exp) 0)", '"
                                                             (->string (alist-ref 'alpha-update-method exp))"', "
                                                             time", "
                                                             (car score)", "
                                                             (cadr score)", "
                                                             (caddr score)", '"
                                                             rundir"')"))
                                           )
                                      (query fetch-alists (sql db insert-sql))
                                      (print rundir" "time" "score)
                                      `(
                                        ,@exp
                                        (time . ,time)
                                        (score . ,score)
                                        (PI . ,PI)
                                        (U . ,U)
                                        )
                                      
                                      )
                                    #f)
                                ))
                            results)))))
  
  (close-database db)
  (with-output-to-pipe (conc "gzip -c - > "outfile".gz") (lambda () (pp out)))
  (print "Wrote "outfile".gz"))

