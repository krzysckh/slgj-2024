(define-library (puz util)
  (import
   (owl toplevel)
   (raylib))

  (export
    width
    height
    n-blocks
    grid-size
    camera:speed-mul
    camera:zoom
    maybe-error-fatal
    maybe-error
    real-v
    real-p
    aq
    flatten
   )

  (begin
    (define width 640)
    (define height width)
    (define n-blocks 10)
    (define grid-size (/ width n-blocks))

    (define camera:speed-mul 3)
    (define camera:zoom 0.8)
    (define maybe-error-fatal #t)

    (define (maybe-error s . l)
      (print "maybe-error: " s)
      (when (not (null? l))
        (print "additional-information:")
        (map (λ (x) (print "  → " x)) l))
      (when maybe-error-fatal
        (exit-owl 1)))

    ;; real "value"
    ;; vp → v
    (define (real-v v)
      (* v grid-size))

    ;; real "point"
    ;; pt → vec2
    (define (real-p v)
      (list (real-v (car v)) (real-v (cadr v))))

    (define (aq sym v) (cdr* (assq sym v)))

    (define (flatten l)
      (cond ((null? l) '())
            ((pair? (car l))
             (append (flatten (car l))
                     (flatten (cdr l))))
            (else (cons (car l) (flatten (cdr l))))))

    ))
