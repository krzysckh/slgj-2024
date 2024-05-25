(define-library (puz map-cache)
  (import
   (owl toplevel)
   (owl eval)
   (prefix (owl sys) sys/)
   (raylib)
   (puz map)
   (puz util))

  (export
   load-map-maybe-cache
   )

  (begin
    (define (get-doors Map)
      (display "doors-all")
      (let ((doors-all (map (λ (c)
                              (display (string c))
                              (let ((p0 (find-things c Map 2)))
                                (if (null? p0)
                                    ()
                                    (let ((p1 (find-things (- c 32) Map 2)))
                                      (if (null? p1)
                                          ()
                                          (let ((t0 (if (> (vec2dist (car p0) (car p1))
                                                           (vec2dist (car p0) (cadr p1)))
                                                        (list (car p0) (car p1))
                                                        (list (car p0) (cadr p1))))
                                                (t1 (if (> (vec2dist (cadr p0) (car p1))
                                                           (vec2dist (cadr p0) (cadr p1)))
                                                        (list (cadr p0) (car p1))
                                                        (list (cadr p0) (cadr p1)))))
                                            (list t0 t1)))))))
                            (iota #\a 1 (+ #\z 1)))))
        (print "OK")
        (display "doors")
        (let ((v (let loop ((d doors-all) (acc ()))
                   (display ".")
                   (cond
                    ((null? d) acc)
                    ((null? (car d)) (loop (cdr d) acc))
                    (else
                     (loop (cdr d) (append acc (list (caar d)) (list (cadar d)))))))))
          (print "OK")
          v)))

    (define (get-initial-blocks Map) (find-things #\# Map))

    (define (get-initial-button-states Map)
      (display "initial-button-states")
      (let ((v (map (λ (v)
                      (display ".")
                      (list (cadr (lref (lref Map (cadr v)) (car v))) v #f))
                    (find-things button? Map)))) ;; #f = unpressed
        (print "OK")
        v))

    (define (get-initial-player-pos Map) (find-thing #\@ Map))

    (define (load-map-from-cache fname)
      (print "loading map data from " fname)
      (let* ((f (open-input-file fname))
             (sexp (read f)))
        (close-port f)
        (exported-eval sexp *toplevel*)))

    (define (save-cache Map fname)
      (let* ((cf (string-append fname "-cache"))
             (sexp (list
                    `(map                    . ,Map)
                    `(doors                  . ,(get-doors Map))
                    `(initial-blocks         . ,(get-initial-blocks Map))
                    `(initial-button-states  . ,(get-initial-button-states Map))
                    `(initial-player-pos     . ,(get-initial-player-pos Map))))
             (f (open-output-file cf)))
        (print "save-cache to " cf)
        (write-to f sexp)
        (close-port f)
        sexp))

    (define (load-map-maybe-cache f)
      (let ((cf (string-append f "-cache")))
        (if (sys/file? cf)
            (let ((t1 (cdr (assoc 'mtim (sys/stat f #t))))
                  (t2 (cdr (assoc 'mtim (sys/stat cf #t)))))
              (if (> t1 t2)
                  (begin
                    (print f " newer than cache - cache invalidated")
                    (save-cache (load-map f) f))
                  (load-map-from-cache cf)))
            (begin
              (print "cannot read from map cache - file doesn't exist")
              (save-cache (load-map f) f)))))
    ))
