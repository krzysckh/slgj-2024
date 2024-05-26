(define-library (puz maze)
  (import
   (owl toplevel)
   (owl random)
   (raylib)
   (puz util)
   (puz map))

  (export
   create-maze
   )

  (begin
    (define (at m v)
      (if v (lref (lref m (cadr v)) (car v)) -1))

    (define (create-maze w h seed chr)
      (let ((m (let loop ((m (make-list (* h 2) (make-list (* w 2) #\=))) (x 0) (y 0) (r (seed->rands seed)) (ctr 0))
                      (let L ((m m))
                        (draw
                         (clear-background black)
                         (draw-text-simple (string-append "generating maze " (str* x) " " (str* y)) '(0 0) 24 white))
                        (let* ((n0 (if (> x 0)             (list (- x 2) y) #f))
                               (n1 (if (< x (- (* w 2) 2)) (list (+ x 2) y) #f))
                               (n2 (if (> y 0)             (list x (- y 2)) #f))
                               (n3 (if (< y (- (* h 2) 2)) (list x (+ y 2)) #f))
                               (nbs (list n0 n1 n2 n3))
                               (un (filter (λ (v) (and (> (at m v) 0) (not (= (at m v) #\space)))) nbs))
                               (m (lset m y (lset (lref m y) x #\space))))
                          (if (null? un)
                              (lset m y (lset (lref m y) x (list ctr)))
                              (lets ((r v (rand-range r 0 (length un)))
                                     (nb (lref un v))
                                     (∆ (map (λ (x) (/ x 2)) (vec2- nb `(,x ,y))))
                                     (∆x (car ∆))
                                     (∆y (cadr ∆))
                                     (m (lset m (+ y ∆y) (lset (lref m (+ y ∆y)) (+ x ∆x) #\space))))
                                    (L (loop m (car nb) (cadr nb) r (+ ctr 1))))))))))
            ;; (m (lset m y (lset (lref m y) x #\space))))

            (let* ((m (lset m 0 (lset (lref m 0) 0 #\@)))
                   (lens (flatten (map (λ (l) (filter list? l)) m)))
                   (max-path (maxl lens))
                   (m (let loop ((x 0) (y 0))
                        (cond
                         ((>= x (length (car m))) (loop 0 (+ y 1)))
                         ((eqv? (car* (lref (lref m y) x)) max-path)
                          (lset m y (lset (lref m y) x `(#\- ,chr))))
                         (else
                          (loop (+ x 1) y)))))
                   (m (map (λ (l) (map (λ (v) (if (list? v) (if (= (length v) 1) #\space v) v)) l)) m))
                   (m (map (λ (l) (append '(#\=) l '(#\=))) m))
                   (m (append (list (make-list (+ (* w 2) 2) #\=)) m (list (make-list (+ (* w 2) 2) #\=))))
                   (m (map flatten m)))
              m)))
              ;; (write-map (open-output-file (string-append "maze" (string chr) ".text")) m))))

    ))
