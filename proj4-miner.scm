(include "proj4-lib.scm")
(use sql-de-lite)
(use posix)
;(import chicken scheme)


(define (create-clobber-db dbfile)
  (if (file-exists? dbfile) (delete-file dbfile))
  (let* ((db (open-database dbfile))
         (schema-sql "create table results (epoch, algo, gridworld, decoy_reward, goal_reward, gamma, episodes, epsilon_decay_factor, max_moves_per_episode, alpha_update_method, qinit_method, time, score_mean, score_stddev, score_avg_moves, rundir);")
         (stm (sql db schema-sql))
         )
    (query fetch-alists stm)
    db))
    
      

(let* ((files '("score.sexp" "time.txt" "PI.sexp" "U.sexp"))
       (epoch (list-ref (argv) 3 ))
       (dbfile (conc "results-"epoch".sqlite3"))
       (db (create-clobber-db dbfile))
       (spec    (with-input-from-file (conc "proj4-spec-"epoch".sexp") read))
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
                                           (rf    (conc rundir "/rounds.sexp"))
                                           (rounds (if (file-exists? rf)
                                                       (with-input-from-file rf read)
                                                       0))
                                           (sbef  (conc rundir "/score-by-episode.sexp"))
                                           (sbye  (if (file-exists? sbef)
                                                      (with-input-from-file sbef read)
                                                      #f))
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
                                                             (or (alist-ref 'n-episodes exp) rounds)", "
                                                             (or (alist-ref 'epsilon-decay-factor exp) 0)", "
                                                             (or (alist-ref 'max-moves-per-episode exp) 0)", '"
                                                             (->string (alist-ref 'alpha-update-method exp))"', "
                                                             "'"(->string (alist-ref 'qinit-method exp))"', "
                                                             time", "
                                                             (car score)", "
                                                             (cadr score)", "
                                                             (caddr score)", '"
                                                             rundir"')"))
                                           )
                                      (print insert-sql)
                                      (exec (sql db insert-sql))
                                      (print rundir" "time" "score)
                                      `(
                                        ,@exp
                                        ,@(if (not (eq? rounds 0))
                                               (list (cons 'rounds rounds))
                                               '())
                                        ,@(if sbye
                                              (list (cons 'score-by-episode sbye))
                                              '())
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

