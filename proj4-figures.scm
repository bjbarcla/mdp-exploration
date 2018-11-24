(include "proj4-lib.scm")


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


(let* ((pspec (with-input-from-file "proj4-spec.sexp" read))
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
    (map
     (lambda (gwitem)
       (let* ((gwname (car gwitem))
              (gw (cdr gwitem)))
         (conc
          "* "gwname"\n"
          (gridworld-rewards-figure gw default-reward)
          "\n")))
     gwitems)
    "\n")
   "figures/figures.org"))

  
