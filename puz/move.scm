(define-library (puz move)
  (import
   (owl toplevel)
   (raylib)
   (puz map))

  (export
   button-door-open?
   ppos-legal?
   current-keys

   find-block
   update-buttons
   dispatch-move
   maybe-door
   )

  (begin
    (define (find-block pos blocks . skip-n)
      (let ((skip (if (null? skip-n) -1 (car skip-n))))
        (let loop ((n 0) (blocks blocks))
          (cond
           ((null? blocks) #f)
           ((and (equal? (car blocks) pos) (not (= n skip))) n)
           (else
            (loop (+ n 1) (cdr blocks)))))))

    (define (button-door-open? Map ppos buttons)
      (let* ((x (car ppos))
             (y (cadr ppos))
             (v (assoc (cadr (lref (lref Map y) x)) buttons)))
        (caddr v)))

    (define (ppos-legal? Map ppos ∆ blocks buttons skip-n finish-f)
      (let* ((x (car ppos))
             (y (cadr ppos))
             (ly (length Map))
             (bat (find-block ppos blocks skip-n)))
        (cond
         ((< y 0) blocks)
         ((< x 0) blocks)
         ((>= y (length Map)) blocks)
         ((>= x (length (lref Map y))) blocks) ;; overflowing lref
         ((finish? (lref (lref Map y) x)) (finish-f)) ;; TODO: assuming a block cannot be pushed to finish
         ((and (> bat -1)
               (let ((v (lref (lref Map (+ y (cadr ∆))) (+ x (car ∆)))))
                 (or (door? v) (maze-start? v) (maze-end? v))))
          #f) ;; it is a block & trying to go through doors
         (bat (ppos-legal?
               Map (vec2+ ppos ∆) ∆ (lset blocks bat (vec2+ ppos ∆)) buttons bat finish-f))
         ((maze-start? (lref (lref Map y) x)) blocks)
         ;; ((maze-end? (lref (lref Map y) x)) blocks)
         ((maze-end-of? (lref (lref Map y) x)) blocks)
         ((button? (lref (lref Map y) x)) blocks)
         ((button-target? (lref (lref Map y) x))
          (if (button-door-open? Map ppos buttons) blocks #f))
         ((!button-target? (lref (lref Map y) x))
          (if (button-door-open? Map ppos buttons) #f blocks))
         ((list? (lref (lref Map y) x)) #f)
         ((has? nono-blocks (lref (lref Map y) x)) #f)
         (else
          blocks))))

    (define (get-∆ q)
      (cond
       ((null? q) (values q (list 0 0)))
       ((= (car q) key-a) (values (cdr q) (list -1  0)))
       ((= (car q) key-d) (values (cdr q) (list  1  0)))
       ((= (car q) key-w) (values (cdr q) (list  0 -1)))
       ((= (car q) key-s) (values (cdr q) (list  0  1)))
       (else
        (values q (list 0 0)))))

    (define (update-buttons buttons pos blocks sounds)
      (let ((poss (append blocks (list pos))))
        (map (λ (b)
               (let ((pressed? (has? poss (cadr b)))
                     (was-pressed? (caddr b)))
                 (when (and (not was-pressed?) pressed?)
                   (play-sound (aq 'btndown sounds)))
                 (list (car b) (cadr b) pressed?)))
             buttons)))

    ;; pos q blocks → (values q blocks delta-pos)
    (define (dispatch-move Map pos q blocks buttons sounds finish-f)
      (lets ((q ∆ (get-∆ q))
             (+∆ (vec2+ pos ∆))
             (b (ppos-legal? Map +∆ ∆ blocks buttons -1 finish-f))
             (btns (update-buttons buttons pos blocks sounds)))
            (when (and b (not (equal? blocks b)))
              (play-sound (aq 'mvblock sounds)))
            (if b
                (values +∆ q b btns)
                (values pos q blocks buttons)
                )))

    ;; only queue move keys
    (define keys (list key-a key-d key-w key-s))

    (define (keys-down)
      (filter key-down? keys))

    (define (current-keys)
      (if (or (key-down? key-left-control) (key-down? key-left-shift))
          (keys-down)
          (let loop ((kp (key-pressed)) (acc ()))
            (cond
             ((= kp 0) acc)
             ((has? keys kp) (loop (key-pressed) (append acc (list kp))))
             (else
              (loop (key-pressed) acc))))))

    (define (maybe-door doors ppos sounds)
      (let ((p (assoc ppos doors)))
        (if p
            (begin
              (play-sound (aq 'door sounds))
              (cadr p))
            ppos)))


    ))
