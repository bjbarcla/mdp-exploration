(use sql-de-lite)
(use srfi-13) ;; string-join
(use data-structures) ;; conc
(use extras) ;; pp
;; CREATE TABLE results (epoch, algo, gridworld, decoy_reward, goal_reward, gamma, episodes, epsilon_decay_factor, max_moves_per_episode, alpha_update_method, time, score_mean, score_stddev, score_avg_moves, rundir);

(let* ((dbfile "results-2.sqlite3")
       (db (open-database dbfile))
       (fields '( "gamma"
                  "score_mean"
                  ;;"score_avg_moves"
                  ;;"time"
                  "episodes" ))
       (qs (conc
            "select "
            (string-join fields ", ")
            ;;"*"
            " from results where gridworld=? and algo=? and goal_reward=2 order by gamma"))
       (bindings '("6x8" "value-iteration"))
       ;;(qs "select * from results where algo=? and gridworld=?")
       (stm   (sql db qs))
       (result (apply query fetch-rows stm bindings))
       )
  (pp result))
  
