(include "proj4-lib.scm")
(use sql-de-lite)
(use fmt)

(define (datum->org-cell x)
  (cond
   ((string? x) x)
   ((and (number? x) (exact? x)) (number->string x))
   ((and (number? x)) (fmt #f 
                           ;;(pad/left 8
                                     (num x 10 2)
                           ;;          )
                           ))
   (else (->string x))))

(define (row->org-row row)
  (conc "|"
        (string-join (map datum->org-cell row) "|")
        "|"))

(define (sql->orgtable db fields query-str bindings  #!key (alt-field-names #f))
  (let* ((dbfile "results-2.sqlite3")
       (db (open-database dbfile))
       (qs (conc
            "select "
            (string-join fields ", ")
            " from results where "
            query-str)))
    (print qs)
    (print bindings)
    (let* ((stm            (sql db qs))
           (rows           (apply query fetch-rows stm bindings))
           (n-rows         (length rows))
           (title-org-row  (row->org-row (if alt-field-names alt-field-names fields)))
           (data-org-rows  (map row->org-row rows))
           )
      (string-join
       (cons title-org-row data-org-rows)
       "\n"))))


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



(define (str->file str filename)
  (with-output-to-file filename (lambda () (print str)))
  (print "Wrote "filename)
  #f)


;;(define (value-iteration-org-table gw 
         

(print (argv))
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
      
      "\n"

      ) "\n")
   "figures/figures.org"))

  
