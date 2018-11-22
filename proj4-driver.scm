(use srfi-1) ;; provides iota
(use srfi-13) ;; provides string-join
(use matchable)
(use extras) ;; provides pp
(use posix)  ;; provides create-directory


(define (inc-radix idx-l radix-l #!key (so-far '()))
  (cond
   ((null? idx-l)
    #f) ;; overflow
   ((eq? (add1 (car idx-l)) (car radix-l)) ;; carry
    (inc-radix (cdr idx-l) (cdr radix-l) so-far: (cons 0 so-far)))
   (else
    (append
     so-far
     (list (add1 (car idx-l)))
     (cdr idx-l)))))
;;(inc-radix '(0 0 0) '(10 10 10))

(define (exp-al->rundir al algo epoch)
  (conc "results/"epoch"/"algo"/"
        (string-join
         (map (lambda (pair)
                (conc
                 ;;(->string (car pair))
                 ;;"="
                 (cond
                  ((eq? (car pair) 'gridworld)
                   (string-join (map ->string (cdr pair)) "x"))
                  (else
                   (->string (cdr pair))))))
              al)
         "_")))


(define (get-launch-cmd rundir al)
  (conc "launcher.sh --log-file "rundir"/batch.log ./proj4-experiment "rundir))


(define (grid-search param-alist)
  (let* ((keys (map car param-alist))
         (n-keys (length keys))
         (n-idx-list (map (lambda (i) (length (cdr i))) param-alist))
         (lookup-idx (lambda (idx)
                       (map
                        (lambda (pos)
                          (let* ((idx-at-pos (list-ref idx pos))
                                 (key-val-list (list-ref param-alist pos))
                                 (key (car key-val-list))
                                 (val-list (cdr key-val-list))
                                 (val (list-ref val-list idx-at-pos)))
                            (cons key val)))
                        (iota n-keys))))
         (inc-idx (lambda (idx)
                    (inc-radix idx n-idx-list))))
    (let* ((idx0 (map (lambda (x) 0) param-alist)))
      (let loop ((this-idx idx0) (rv '()))
        (cond
         (this-idx
          ;;(callback (lookup-idx this-idx))
          (loop (inc-idx this-idx)
                (cons (lookup-idx this-idx) rv)))
         (else
          rv))))))
        
(define (main)    
  (let* ((epoch           (list-ref (argv) 2))
         (spec-file       (list-ref (argv) 1))
         (spec            (with-input-from-file spec-file read))
         (algos-al        (alist-ref 'algos spec))
         (param-ranges-al (alist-ref 'param-ranges spec))
         (experiment-specs-by-algo
          (map
           (lambda (algo)
             (cons algo
                   (let* ((hyperparams (alist-ref algo algos-al))
                          (hp-grid     (map
                                        (lambda (hp)
                                          (cons hp (alist-ref hp param-ranges-al)))
                                        hyperparams))
                          (experiments (grid-search hp-grid)))
                     (map
                      (lambda (experiment)
                        (let* ((exp-al    `( (epoch  . ,epoch )
                                             (algo   . ,algo  )
                                             ,@experiment ) )
                               (rundir     (exp-al->rundir experiment algo epoch))
                               (launch-cmd (get-launch-cmd rundir exp-al)))
                          `( (launch-cmd . ,launch-cmd )
                             (rundir . ,rundir)
                             ,@exp-al )))
                      experiments))))
           (map car algos-al)))
         (experiment-specs
          (apply append
           (map (lambda (algo)
                  (map (lambda (exp-al)
                         (cons (cons 'algo algo) exp-al))
                       (alist-ref algo experiment-specs-by-algo)))
                (map car experiment-specs-by-algo)))))
    (for-each
     (lambda (exp-al)
       ;;(pp exp-al)
       (let* ((rundir (alist-ref 'rundir exp-al))
              (launch-cmd (alist-ref 'launch-cmd exp-al)))
         ;;(print "rundir="rundir" launch-cmd="launch-cmd)
         ;;(pp (car exp-al))
         (create-directory rundir #t)
         (with-output-to-file (conc rundir "/experiment.sexp") (lambda ()
                                                                 (pp exp-al)))))
     experiment-specs)
    
    (with-output-to-file (conc "results/"epoch"-experiment-specs.sexp") (lambda () (pp experiment-specs)))


    (with-output-to-file (conc "proj4-"epoch".joblist")
      (lambda ()
        (for-each
         (lambda (exp-al)
           ;;(pp exp-al)
           (let* ((rundir (alist-ref 'rundir exp-al))
                  (launch-cmd (alist-ref 'launch-cmd exp-al)))
             (print launch-cmd)))
         experiment-specs)))))

(main)

