;; -*- mode: scheme; compile-command: "ol-rl -r g.scm" -*-

(import
 (owl toplevel)
 (owl lazy)
 (owl random)
 (raylib))

(define font-f (list->bytevector (file->list "assets/proggy-square.ttf")))
(define bg-f   (list->bytevector (file->list "assets/AngbandTk/dg_grounds32.gif")))
(define door-f (list->bytevector (file->list "assets/AngbandTk/dg_dungeon32.png")))
(define edg-f  (list->bytevector (file->list "assets/AngbandTk/dg_edging232.png")))

(define snd-btndown-f (list->bytevector (file->list "assets/btndown.wav")))
(define snd-mvblock-f (list->bytevector (file->list "assets/blockmove.wav")))
(define snd-undo-f    (list->bytevector (file->list "assets/undo.wav")))
(define snd-door-f    (list->bytevector (file->list "assets/door.wav")))

(define additional-rand-blocks '((5 8) (8 8)))

;; block player cannot move through
(define nono-blocks    (list #\= #\|))
(define replace-blocks (list #\= #\space))

(define (symthing? sym)
  (λ (thing)
    (if (symbol? (car* thing))
        (eqv? (car* thing) sym)
        #f)))

(define drawme?         (symthing? 'drawme))
(define button?         (symthing? 'btn))
(define button-target?  (symthing? 'btn-target))
(define !button-target? (symthing? '!btn-target))
(define normal-text?    (symthing? '%-text))
(define small-text?     (symthing? '^-text))
(define (door? c) (and (>= c #\a) (<= c #\z)))
(define (door-ending? c) (and (>= c #\A) (<= c #\Z)))

(define (aq sym v) (cdr* (assq sym v)))

(define (fix-length m ml)
  (append m (make-list (- ml (length m)) #\space)))

(define rand-block-% 20)
(define (add-random-blocks m)
  (letrec ((f (λ (r line acc)
                (cond
                 ((null? line) (values r acc))
                 ((= (car line) #\=)
                  (lets ((R (/ rand-block-% 100))
                         (r v (rand-range r (- (numerator R) 1) (denominator R))))
                    (if (= v (numerator R))
                        (lets ((r v (rand-range r 0 (length additional-rand-blocks))))
                          (f r (cdr line)
                             (append acc (list (append '(drawme door) (lref additional-rand-blocks v))))))
                        (f r (cdr line) (append acc (list (car line)))))))
                 (else
                  (f r (cdr line) (append acc (list (car line)))))))))
    (let loop ((r (seed->rands (time-ms))) (m m) (acc ()))
      (if (null? m)
          acc
          (lets ((r l (f r (car m) ())))
            (loop r (cdr m) (append acc (list l))))))))

(define (floodfill-emptyness m pt)
  (cond
   ((< (car pt) 0) m)
   ((< (cdr pt) 0) m)
   ((>= (cdr pt) (length m)) m)
   ((>= (car pt) (length (lref m (cdr pt)))) m)
   ((list? (lref (lref m (cdr pt)) (car pt))) m)
   ((not (= (lref (lref m (cdr pt)) (car pt)) #\space)) m)
   (else
    (let* ((m (lset m (cdr pt) (lset (lref m (cdr pt)) (car pt) #\_)))
           (m (floodfill-emptyness m `(,(car pt) . ,(+ (cdr pt) 1))))
           (m (floodfill-emptyness m `(,(car pt) . ,(- (cdr pt) 1))))
           (m (floodfill-emptyness m `(,(- (car pt) 1) . ,(cdr pt))))
           (m (floodfill-emptyness m `(,(+ (car pt) 1) . ,(cdr pt)))))
      m))))

(define (gettext c l acc)
  (if (eqv? c (car l))
      (values l (list->string acc))
      (gettext c (cdr l) (append acc `(,(car l))))))

(define (find-multichar l)
  (let loop ((l l) (acc ()))
    (cond
     ((null? l) acc)
     ((list? (car l)) (loop (cdr l) (append acc (list (car l)))))
     ((= (car l) #\.) (loop (cddr l) (append acc (list `(btn ,(cadr l))))))
     ((= (car l) #\|) (loop (cddr l) (append acc (list `(btn-target ,(cadr l))))))
     ((= (car l) #\!) (loop (cddr l) (append acc (list `(!btn-target ,(cadr l))))))
     ((has? '(#\^ #\%) (car l))
      (lets ((ls s (gettext (car l) (cdr l) ())))
        (loop (cdr ls) (append acc (list `(,(string->symbol (string-append (string (car l)) "-text")) ,s))))))
     (else
      (loop (cdr l) (append acc (list (car l))))))))

;; TODO: point-in-polygon every point to find where to draw floor textures
(define (load-map f)
  (let* ((m (map string->list (force-ll (lines (open-input-file "map.text")))))
         (m (map (λ (l) (append '(#\space) l '(#\space))) m))
         (ml (maxl (map length m)))
         (m (map (λ (x) (fix-length x ml)) m)) ;
         (m (append (list (make-list ml #\space)) m (list (make-list ml #\space))))
         (m (add-random-blocks m))
         (m (map find-multichar m))
         (m (floodfill-emptyness m '(0 . 0))))
    m))

(define Map (load-map "map.text"))

(map print (map list->string (map (λ (l) (map (λ (x) (if (list? x) #\= x)) l)) Map)))

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
    (1 4) (2 4)))

;; TODO: closed doors + keys

;; (define doors-closed
;;   '((3 0)
;;     (0 1) (3 1)
;;     (3 3) (6 3)
;;     (0 4) (3 4) (6 4)))

;; c = char | (f(x) → #t|#f) → (...)
(define (find-things c)
  (let ((f (if (function? c)
               c
               (λ (x) (eqv? x c)))))
    (let loop ((x 0) (y 0) (acc ()))
      (cond
       ((>= y (length Map)) acc)
       ((>= x (length (lref Map y))) (loop 0 (+ y 1) acc))
       ((f (lref (lref Map y) x)) (loop (+ x 1) y (append acc (list (list x y)))))
       (else
        (loop (+ x 1) y acc))))))

(define (find-thing c)
  (car (find-things c)))

(define doors-all
  (map (λ (c)
         (let ((p0 (find-things c))
               (p1 (find-things (- c 32))))
           (if (or (null? p0) (null? p1))
               ()
               (let ((t0 (if (> (vec2dist (car p0) (car p1))
                                (vec2dist (car p0) (cadr p1)))
                             (list (car p0) (car p1))
                             (list (car p0) (cadr p1))))

                     (t1 (if (> (vec2dist (cadr p0) (car p1))
                                (vec2dist (cadr p0) (cadr p1)))
                             (list (cadr p0) (car p1))
                             (list (cadr p0) (cadr p1)))))
                 (list t0 t1)))))
       (iota #\a 1 (+ #\z 1))))

(define doors
  (let loop ((d doors-all) (acc ()))
    (cond
     ((null? d) acc)
     ((null? (car d)) (loop (cdr d) acc))
     (else
      (loop (cdr d) (append acc (list (caar d)) (list (cadar d))))))))

(define initial-blocks (find-things #\#))
(define initial-button-states
  (map (λ (v) (list (cadr (lref (lref Map (cadr v)) (car v))) v #f))
       (find-things button?))) ;; #f = unpressed

(print "door wormholes: " doors)
(print "initial-button-states: " initial-button-states)

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
  (lref doors-open (modulo (- c #\a) (length doors-open))))

;; assuming tile-size = 32
;; texture (tile-x tile-y) fin-rect
(define (draw-tile txt tile rect)
  (draw-texture-pro
   txt
   (list (* 32 (car tile)) (* 32 (cadr tile)) 32 32)
   rect
   '(0 0)
   0 white))

(define draw-thing:skip '(#\_))

(define (draw-thing thing rect textures btns)
  (cond
   ((drawme? thing)
    (draw-tile (aq (lref thing 1) textures) (cddr thing) rect))
   ((button? thing)
    (draw-tile (aq 'bg textures) '(8 0) rect)
    (draw-tile (aq 'edg textures) '(6 8) rect))
   ((button-target? thing)
    (if (caddr (assq (cadr thing) btns))
        (begin
          (draw-tile (aq 'bg textures) '(8 0) rect)
          (draw-tile (aq 'door textures) '(4 4) rect))  ;; door open
        (draw-tile (aq 'door textures) '(3 4) rect)))   ;; door closed
   ((!button-target? thing) ;; like button-target, but the other way around
    (if (caddr (assq (cadr thing) btns))
        (draw-tile (aq 'door textures) '(3 4) rect)
        (begin
          (draw-tile (aq 'bg textures) '(8 0) rect)
          (draw-tile (aq 'door textures) '(4 4) rect))))
   ((normal-text? thing)
    (draw-text (aq 'font textures) (cadr thing) `(,(car rect) ,(cadr rect)) 32  0 white))
   ((small-text? thing)
    (draw-text (aq 'font textures) (cadr thing) `(,(car rect) ,(cadr rect)) 16 0 white))
   ((list? thing) ;; catch-all list error thinghy
    (maybe-error "cannot draw-thing" thing))
   ((or (= thing #\space) (= thing #\@)
        (door-ending? thing) (= thing #\#))
    (draw-tile (aq 'bg textures) '(8 0) rect))
   ((= thing #\=) (draw-tile (aq 'door textures) '(2 3) rect))
   ;; ((= thing #\|) (draw-tile (cdr (assq 'door textures)) (car doors-closed) rect))
   ((door? thing) (draw-tile (aq 'door textures) (door-tile thing) rect))
   ((and (>= thing #\A) (<= thing #\Z)) 0)

   ((has? draw-thing:skip thing) 0)
   (else
    (maybe-error "cannot draw-thing" (string thing)))))

(define (draw-map ppos textures buttons)
  (for-each
   (λ (n)
     (let ((line (lref Map n))
           (y (* grid-size n)))
       (for-each
        (λ (v) (draw-thing (lref line v) (list (* grid-size v) y grid-size grid-size) textures buttons))
        (iota 0 1 (length line)))))
   (iota (max 0 (- (cadr ppos) 16)) 1 (min (length Map) (+ (cadr ppos) 16)))))

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

(define (dispatch-move:button-door-open? ppos buttons)
  (let* ((x (car ppos))
         (y (cadr ppos))
         (v (assoc (cadr (lref (lref Map y) x)) buttons)))
    (caddr v)))

(define (dispatch-move:ppos-legal? ppos ∆ blocks buttons skip-n)
  (let* ((x (car ppos))
         (y (cadr ppos))
         (ly (length Map))
         (bat (dispatch-move:find-block ppos blocks skip-n)))
    (cond
     ((< y 0) blocks)
     ((< x 0) blocks)
     ((>= y (length Map)) blocks)
     ((>= x (length (lref Map y))) blocks) ;; overflowing lref
     (bat (dispatch-move:ppos-legal?
           (vec2+ ppos ∆) ∆ (lset blocks bat (vec2+ ppos ∆)) buttons bat))
     ((button? (lref (lref Map y) x)) blocks)
     ((button-target? (lref (lref Map y) x))
      (if (dispatch-move:button-door-open? ppos buttons) blocks #f))
     ((!button-target? (lref (lref Map y) x))
      (if (dispatch-move:button-door-open? ppos buttons) #f blocks))
     ((list? (lref (lref Map y) x)) #f)
     ((has? nono-blocks (lref (lref Map y) x)) #f)
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

(define (dispatch-move:update-buttons buttons pos blocks sounds)
  (let ((poss (append blocks (list pos))))
    (map (λ (b)
           (let ((pressed? (has? poss (cadr b)))
                 (was-pressed? (caddr b)))
             (when (and (not was-pressed?) pressed?)
               (play-sound (aq 'btndown sounds)))
             (list (car b) (cadr b) pressed?)))
         buttons)))

;; pos q blocks → (values q blocks delta-pos)
(define (dispatch-move pos q blocks buttons sounds)
  (lets ((q ∆ (dispatch-move:get-∆ q))
         (+∆ (vec2+ pos ∆))
         (b (dispatch-move:ppos-legal? +∆ ∆ blocks buttons -1))
         (btns (dispatch-move:update-buttons buttons pos blocks sounds)))
    (when (and b (not (equal? blocks b)))
      (play-sound (aq 'mvblock sounds)))
    (if b
        (values +∆ q b btns)
        (values pos q blocks buttons)
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

(define (maybe-door ppos sounds)
  (let ((p (assoc ppos doors)))
    (if p
        (begin
          (play-sound (aq 'door sounds))
          (cadr p))
        ppos)))

(define (draw-blocks blocks textures)
  (for-each
   (λ (b)
     (draw-tile
      (aq 'bg textures)
      '(0 18)
      (list (+ 4 (real-v (car b)))
            (+ 4 (real-v (cadr b)))
            (- grid-size 8)
            (- grid-size 8))))
   blocks))

(define (draw-background-textures txts)
  (for-each
   (λ (y)
     (for-each
      (λ (x)
        (draw-tile (aq 'door txts) '(2 0) (list x y grid-size grid-size)))
      (iota 0 grid-size width)))
   (iota 0 grid-size height)))

;; (play-sound (wave->sound (bytevector->wave ".wav" snd-btndown-f))))

(define (load-sounds)
  (let ((snd-btndown (wave->sound (bytevector->wave ".wav" snd-btndown-f)))
        (snd-mvblock (wave->sound (bytevector->wave ".wav" snd-mvblock-f)))
        (snd-undo    (wave->sound (bytevector->wave ".wav" snd-undo-f)))
        (snd-door    (wave->sound (bytevector->wave ".wav" snd-door-f)))
        )
    `((btndown . ,snd-btndown)
      (mvblock . ,snd-mvblock)
      (undo    . ,snd-undo)
      (door    . ,snd-door)
      )))

(define (load-textures)
  (let ((font (bytevector->font font-f ".ttf" 64 1024))
        (door-tiles (image->texture (list->image ".png" door-f)))
        (bg-tiles   (image->texture (list->image ".gif" bg-f)))
        (edg-tiles  (image->texture (list->image ".png" edg-f))))
    `((font . ,font)
      (bg   . ,bg-tiles)
      (door . ,door-tiles)
      (edg  . ,edg-tiles))))

(define (main _)
  (set-target-fps! 30)
  (with-window
   width height "λ-test"
   (let* ((_ (init-audio-device)) ;; lol!
          (sounds (load-sounds))
          (textures (load-textures)))
     (for-each (λ (t) (set-texture-filter! (cdr t) texture-filter-bilinear)) textures)
     (let loop ((ppos initial-player-pos)
                (camera-pos (real-p initial-player-pos))
                (key-queue ())
                (blocks initial-blocks)
                (buttons initial-button-states)
                (undo ())
                (debug #f))
       (lets ((ppos-prev ppos)
              (key-queue (append key-queue (queue:current-keys)))
              (ppos key-queue blocks buttons (dispatch-move ppos key-queue blocks buttons sounds))
              (ppos (maybe-door ppos sounds))
              (debug (if (key-pressed? key-g) (not debug) debug))
              (camera camera-pos (camera ppos camera-pos))
              ;; maybe do undo?
              (ppos blocks undo (if (key-pressed? key-u)
                                    (let ((lu (lref undo (max 0 (- (length undo) 2)))))
                                      (play-sound (aq 'undo sounds))
                                      (values (car lu) (cadr lu) (ldel undo (- (length undo) 1))))
                                    (values ppos blocks undo))))
         (draw
          (clear-background black)
          (draw-background-textures textures)

          (with-camera2d
           camera
           (begin ;; TODO: ugly hack - fix with-camera2d macro
             (when debug (draw-grid-lines))
             (draw-map ppos textures buttons)
             (draw-blocks blocks textures)
             (draw-player ppos)))

          (when debug
            (draw-text (aq 'font textures) (str* buttons) '(0 0) 32 0 white))

          ;; the shadow thingy
          (draw-rectangle
           `(0 0 ,width ,height)
           (color 0 0 0 (clamp 0 255 (- (floor (/ (vec2dist (real-p ppos) camera-pos) 4)) 25))))
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
                buttons
                undo
                debug))))))))

main
