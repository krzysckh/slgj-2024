;; -*- mode: scheme; compile-command: "ol-rl -r g.scm" -*-

(import
 (owl toplevel)
 (owl lazy)
 (raylib))

(define font-f (list->bytevector (file->list "proggy-square.ttf")))

(define width 640)
(define height width)
(define n-blocks 10)
(define grid-size (/ width n-blocks))

(define camera:speed-mul 3)
(define camera:zoom 0.8)

(define maybe-error-fatal #t)

(define Map (map string->list (force-ll (lines (open-input-file "map.text")))))

(define (find-thing c)
  (let loop ((x 0) (y 0))
    (cond
     ((>= y (length Map)) #f)
     ((>= x (length (list-ref Map y))) (loop 0 (+ y 1)))
     ((eqv? (list-ref (list-ref Map y) x) c) (list x y))
     (else
      (lets ((x (+ x 1))
             (x y (if (>= x (length (list-ref Map y))) (values 0 (+ y 1)) (values x y))))
        (loop x y))))))

(define (find-things c)
  (let loop ((x 0) (y 0) (acc ()))
    (cond
     ((>= y (length Map)) acc)
     ((>= x (length (list-ref Map y))) (loop 0 (+ y 1) acc))
     ((eqv? (list-ref (list-ref Map y) x) c) (loop (+ x 1) y (append acc (list (list x y)))))
     (else
      (loop (+ x 1) y acc)))))

(define portals (map (λ (v) (list (find-thing v) (find-thing (- v 32))))
                     (filter find-thing (iota #\a 1 (+ #\z 1)))))

(define initial-blocks (find-things #\#))
(print initial-blocks)

(define (maybe-error s . l)
  (print "maybe-error: " s)
  (when (not (null? l))
    (print "additional-information:")
    (map (λ (x) (print "  → " x)) l))
  (when maybe-error-fatal
    (exit-owl 1)))

(define possible-portal-colors
  (list yellow pink lime skyblue orange green purple
        beige gold brown violet white magenta))

;; (define (flatten x)
;;   (cond
;;    ((null? x) '())
;;    ((pair? x) (append (flatten (car x)) (flatten (cdr x))))
;;    (else
;;     (list x))))

(define initial-player-pos (find-thing #\@))

(define dgl:min (- 0 (* 10 width)))
(define dgl:max (* 20 width))
(define (draw-grid-lines)
  (for-each
   (λ (v)
     (draw-line-simple dgl:min v dgl:max v white)
     (draw-line-simple v dgl:min v dgl:max white))
   (iota dgl:min grid-size dgl:max))) ;; assuming width == height

(define (portal? c)
  (or (and (>= c #\a) (<= c #\z))
      (and (>= c #\A) (<= c #\Z))))

(define (portal-color c)
  (list-ref possible-portal-colors
            (modulo (if (>= c #\a) (- c #\a) (- c #\A)) (length possible-portal-colors))))

(define draw-thing:skip (list #\space #\@ #\#))

(define (draw-thing thing rect)
  (cond
   ((= thing #\=) (draw-rectangle rect blue))
   ((portal? thing) (draw-rectangle rect (portal-color thing)))

   ((has? draw-thing:skip thing) 0)
   (else
    (maybe-error "cannot draw-thing" (string thing)))))

(define iota-length-map (iota 0 1 (length Map)))
(define (draw-map)
  (for-each
   (λ (n)
     (let ((line (list-ref Map n))
           (y (* grid-size n)))
       (for-each
        (λ (v) (draw-thing (list-ref line v) (list (* grid-size v) y grid-size grid-size)))
        (iota 0 1 (length line)))))
   iota-length-map))

;; real "value"
;; vp → v
(define (real-v v)
  (* v grid-size))

;; real "point"
;; pt → vec2
(define (real-p v)
  (list (real-v (car v)) (real-v (cadr v))))

(define (draw-player pos)
  (draw-rectangle-rounded
   (list (real-v (car pos)) (real-v (cadr pos)) grid-size grid-size)
   0.6 10 red))

(define (camera ppos camera-pos)
  (let* ((real-pos (real-p ppos))
         (camera-pos (vec2move-towards
                     camera-pos
                     real-pos
                     (* (frame-time) camera:speed-mul (vec2dist camera-pos real-pos)))))
    (values
     (list
      (list (- (/ width 2)  (* camera:zoom (/ grid-size 2)))
            (- (/ height 2) (* camera:zoom (/ grid-size 2))))
      camera-pos 0 camera:zoom)
     camera-pos)))

;; TODO: particle w wątkach?
;; TODO: R - restart

;; block player cannot move through
(define nono-blocks (list #\=))

(define (dispatch-move:find-block pos blocks . skip-n)
  (let ((skip (if (null? skip-n) -1 (car skip-n))))
    (let loop ((n 0) (blocks blocks))
      (cond
       ((null? blocks) #f)
       ((and (equal? (car blocks) pos) (not (= n skip))) n)
       (else
        (loop (+ n 1) (cdr blocks)))))))

(define (dispatch-move:ppos-legal? ppos ∆ blocks skip-n)
  (let* ((x (car ppos))
         (y (cadr ppos))
         (ly (length Map))
         (bat (dispatch-move:find-block ppos blocks skip-n)))
    (cond
     ((< y 0) blocks)
     ((< x 0) blocks)
     ((>= y (length Map)) blocks)
     ((>= x (length (list-ref Map y))) blocks) ;; overflowing list-ref
     (bat (dispatch-move:ppos-legal?
           (vec2+ ppos ∆) ∆ (lset blocks bat (vec2+ ppos ∆)) bat))
     ((has? nono-blocks (list-ref (list-ref Map y) x)) #f)
     (else
      blocks))))

(define (dispatch-move:get-∆ q)
  (cond
   ((null? q) (values q (list 0 0)))
   ((= (car q) key-a) (values (cdr q) (list -1  0)))
   ((= (car q) key-d) (values (cdr q) (list  1  0)))
   ((= (car q) key-w) (values (cdr q) (list  0 -1)))
   ((= (car q) key-s) (values (cdr q) (list  0  1)))
   (else
    (values q (list 0 0)))))

;; pos q blocks → (values q blocks delta-pos)
(define (dispatch-move pos q blocks)
  (lets ((q ∆ (dispatch-move:get-∆ q))
         (+∆ (vec2+ pos ∆))
         (b (dispatch-move:ppos-legal? +∆ ∆ blocks -1)))

    (if b
        (values +∆ q b)
        (values pos q blocks)
        )))

;; only queue move keys
(define queue:keys (list key-a key-d key-w key-s))

(define (queue:keys-down)
  (filter key-down? queue:keys))

(define (queue:current-keys)
  (if (key-down? key-left-control)
      (queue:keys-down)
      (let loop ((kp (key-pressed)) (acc ()))
        (cond
         ((= kp 0) acc)
         ((has? queue:keys kp) (loop (key-pressed) (append acc (list kp))))
         (else
        (loop (key-pressed) acc))))))

(define (maybe-portal ppos)
  (let ((p (assoc ppos portals)))
    (if p (cadr p) ppos)))

(define (draw-blocks blocks)
  (for-each
   (λ (b) (draw-rectangle (real-p b) (list grid-size grid-size) darkbrown))
   blocks))

(define (main _)
  (set-target-fps! 30)
  (with-window
   width height "λ-test"
   (let ((font (list->font (bytevector->list font-f) ".ttf" 64 1024)))
     (let loop ((ppos initial-player-pos)
                (camera-pos (real-p initial-player-pos))
                (key-queue ())
                (blocks initial-blocks)
                (undo ())
                (debug #f))
       (lets ((ppos-prev ppos)
              (key-queue (append key-queue (queue:current-keys)))
              (ppos key-queue blocks (dispatch-move ppos key-queue blocks))
              (ppos (maybe-portal ppos))
              (debug (if (key-pressed? key-g) (not debug) debug))
              (camera camera-pos (camera ppos camera-pos))
              ;; maybe do undo?
              (ppos blocks undo (if (key-pressed? key-u)
                                    (let ((lu (list-ref undo (max 0 (- (length undo) 2)))))
                                      (values (car lu) (cadr lu) (ldel undo (- (length undo) 1))))
                                    (values ppos blocks undo))))
         (draw
          (clear-background black)
          (when debug
            (draw-text font (str* (length undo)) '(0 0) 64 0 white))

          (with-camera2d
           camera
           (begin ;; TODO: ugly hack - fix with-camera2d macro
             (when debug (draw-grid-lines))
             (draw-map)
             (draw-blocks blocks)
             (draw-player ppos)
             (draw-text font "helo" '(0 0) 64 0 white)))
          (draw-fps '(0 0)))

         (let ((undo (if (equal? (last undo ()) (list ppos blocks))
                         undo
                         (append undo (list (list ppos blocks))))))
           (if (window-should-close?)
               0
               (loop
                ppos
                camera-pos
                key-queue
                blocks
                undo
                debug))))))))

main
