(include "proj4-lib.scm")
;(import chicken scheme)
(let* ((files '("score.sexp" "time.txt" "PI.sexp" "U.sexp"))
       (spec    (with-input-from-file "proj4-spec.sexp" read))
       (results (with-input-from-file (cadr (argv)) read))
       (outfile (list-ref (argv) 2))
       (out
        (filter-map (lambda (exp)
               (let* ((rundir (alist-ref 'rundir exp))
                      (af (map (lambda (f) (file-exists? (conc rundir "/" f))) files))
                      (complete? (not (member #f af))))
                 (if complete?
                     (let* ((time  (with-input-from-file (conc rundir "/time.txt") read))
                            (score (with-input-from-file (conc rundir "/score.sexp") read))
                            (PI    (with-input-from-file (conc rundir "/PI.sexp") read))
                            (U     (with-input-from-file (conc rundir "/U.sexp") read))
                            )
                       
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
                    results)))
  (with-output-to-pipe (conc "gzip -c - > "outfile".gz") (lambda () (pp out)))
  (print "Wrote "outfile".gz"))
