;; -*- mode: scheme; compile-command: "ol-rl -r g.scm" -*-

(import
 (owl toplevel)
 (owl lazy)
 (raylib))

(define font-f (list->bytevector (file->list "assets/proggy-square.ttf")))
(define bg-f   (list->bytevector (file->list "assets/AngbandTk/dg_extra132.gif")))
(define door-f (list->bytevector (file->list "assets/AngbandTk/dg_dungeon32.gif")))

;; block player cannot move through
(define nono-blocks    (list #\= #\|))
(define replace-blocks (list #\= #\space))

(define (door? c)
  (and (>= c #\a) (<= c #\z)))

(define (door-ending? c)
  (and (>= c #\A) (<= c #\Z)))

;; (define (set-outside line)
;;   (let loop ((x (- (length line) 1)) (n 1) (line line))
;;     (print line ": " x)
;;     (cond
;;      ((< x 0) line)
;;      ((or (has? nono-blocks (list-ref line x)) (door? (list-ref line x)))
;;            (if (if (>= (+ x 1) (length line)) #t (not (has? nono-blocks (list-ref line (+ x 1)))))
;;                (loop (- x 1) (+ n 1) line)
;;                (loop (- x 1) n line)))
;;      ((or (has? '(#\@ #\# #\space) (list-ref line x)) (door-ending? (list-ref line x)))
;;       (if (even? n)
;;           (loop (- x 1) n line)
;;           (loop (- x 1) n (lset line x #\_))))
;;      (else (error "fuck " (list-ref line x))))))

;; TODO: point-in-polygon every point to find where to draw floor textures
(define (load-map f)
  ;; (map set-outside
       (map string->list (force-ll (lines (open-input-file "map.text"))))
       ;; )
  )

(define Map (load-map "map.text"))

(map print (map list->string Map))

(define width 640)
(define height width)
(define n-blocks 10)
(define grid-size (/ width n-blocks))

(define camera:speed-mul 3)
(define camera:zoom 0.8)

(define maybe-error-fatal #t)

(define doors-open
  '((4 0) (5 0) (7 0)
    (1 1) (2 1) (4 1) (5 1) (6 1) (7 1)
    (0 2) (1 2) (3 2) (4 2)
    (4 3) (5 3) (7 3) (8 3)
    (1 4) (2 4) (3 4) (4 4) (5 4) (6 4)))

(define doors-closed
  '((3 0)
    (0 1) (3 1)
    (3 3) (6 3)
    (0 4) (3 4) (6 4)))

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

(define doors (map (λ (v) (list (find-thing v) (find-thing (- v 32))))
                     (filter find-thing (iota #\a 1 (+ #\z 1)))))

(define initial-blocks (find-things #\#))

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
;; pt → vec2
(define (real-p v)
  (list (real-v (car v)) (real-v (cadr v))))

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

(define (door-tile c)
  (list-ref doors-open (modulo (- c #\a) (length doors-open))))

;; assuming tile-size = 32
;; texture (tile-x tile-y) fin-rect
(define (draw-tile txt tile rect)
  (draw-texture-pro
   txt
   (list (* 32 (car tile)) (* 32 (cadr tile)) 32 32)
   rect
   '(0 0)
   0 white))

(define draw-thing:skip (list))

(define (draw-thing thing rect textures)
  (cond
   ((or (= thing #\space) (= thing #\@)
        (door-ending? thing) (= thing #\#))
    (draw-rectangle rect black)) ;; TODO: darken this (draw-tile (cadr (assq 'door textures)) '(3 6) rect (λ (v)
   ((= thing #\=) (draw-tile (cadr (assq 'door textures)) '(0 0) rect))
   ((= thing #\|) (draw-tile (cadr (assq 'door textures)) (car doors-closed) rect))
   ((door? thing) (draw-tile (cadr (assq 'door textures)) (door-tile thing) rect))
   ((and (>= thing #\A) (<= thing #\Z)) 0)

   ((has? draw-thing:skip thing) 0)
   (else
    (maybe-error "cannot draw-thing" (string thing)))))

(define iota-length-map (iota 0 1 (length Map)))
(define (draw-map textures)
  (for-each
   (λ (n)
     (let ((line (list-ref Map n))
           (y (* grid-size n)))
       (for-each
        (λ (v) (draw-thing (list-ref line v) (list (* grid-size v) y grid-size grid-size) textures))
        (iota 0 1 (length line)))))
   iota-length-map))

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

(define (maybe-door ppos)
  (let ((p (assoc ppos doors)))
    (if p (cadr p) ppos)))

(define (draw-blocks blocks)
  (for-each
   (λ (b) (draw-rectangle (real-p b) (list grid-size grid-size) darkbrown))
   blocks))

(define (draw-background-textures txts)
  (for-each
   (λ (y)
     (for-each
      (λ (x)
        (draw-tile (cadr (assq 'door txts)) '(2 0) (list x y grid-size grid-size)))
      (iota 0 grid-size width)))
   (iota 0 grid-size height)))

(define (main _)
  (set-target-fps! 30)
  (with-window
   width height "λ-test"
   (let* ((font (list->font (bytevector->list font-f) ".ttf" 64 1024))
          (door-tiles (image->texture (list->image ".gif" (bytevector->list door-f))))
          (bg-tiles   (image->texture (list->image ".gif" (bytevector->list bg-f))))
          (textures `((bg   ,bg-tiles)
                      (door ,door-tiles))))
     (for-each (λ (t) (set-texture-filter! (cadr t) texture-filter-bilinear)) textures)
     (let loop ((ppos initial-player-pos)
                (camera-pos (real-p initial-player-pos))
                (key-queue ())
                (blocks initial-blocks)
                (undo ())
                (debug #f))
       (lets ((ppos-prev ppos)
              (key-queue (append key-queue (queue:current-keys)))
              (ppos key-queue blocks (dispatch-move ppos key-queue blocks))
              (ppos (maybe-door ppos))
              (debug (if (key-pressed? key-g) (not debug) debug))
              (camera camera-pos (camera ppos camera-pos))
              ;; maybe do undo?
              (ppos blocks undo (if (key-pressed? key-u)
                                    (let ((lu (list-ref undo (max 0 (- (length undo) 2)))))
                                      (values (car lu) (cadr lu) (ldel undo (- (length undo) 1))))
                                    (values ppos blocks undo))))
         (draw
          (clear-background black)
          (draw-background-textures textures)
          (when debug
            (draw-text font (str* (length undo)) '(0 0) 64 0 white))

          (with-camera2d
           camera
           (begin ;; TODO: ugly hack - fix with-camera2d macro
             (when debug (draw-grid-lines))
             (draw-map textures)
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
