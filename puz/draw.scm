(define-library (puz draw)
  (import
   (owl toplevel)
   (raylib)
   (puz util)
   (puz map))

  (export
   dgl:min
   dgl:max
   draw-blocks
   draw-background-textures
   draw-grid-lines
   door-tile
   draw-tile
   draw-thing:skip
   draw-thing
   draw-map
   draw-player
   camera
   )

  (begin
    (define draw-around-size 16)

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
       ((or (maze-start? thing) (maze-end? thing) (maze-end-of? thing))
        (draw-tile (aq 'door textures) '(8 3) rect))
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
       ((finish? thing) (draw-tile (aq 'door textures) '(7 4) rect))
       ((and (>= thing #\A) (<= thing #\Z)) 0)

       ((has? draw-thing:skip thing) 0)
       (else
        (maybe-error "cannot draw-thing" (string thing)))))

    (define (draw-map Map ppos textures buttons)
      (for-each
       (λ (n)
         (let ((line (lref Map n))
               (y (* grid-size n)))
           (for-each
            (λ (v) (draw-thing (lref line v) (list (* grid-size v) y grid-size grid-size) textures buttons))
            (iota (max 0 (- (car ppos) draw-around-size)) 1 (min (+ (car ppos) draw-around-size) (length line))))))
       (iota (max 0 (- (cadr ppos) draw-around-size)) 1 (min (length Map) (+ (cadr ppos) draw-around-size)))))

    (define (draw-player pos textures)
      (draw-tile (aq 'uniq textures) '(0 0) `(,(real-v (car pos) ) ,(real-v (cadr pos)) ,grid-size ,grid-size)))

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


    ))
