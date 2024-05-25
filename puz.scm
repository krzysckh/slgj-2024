;; -*- mode: scheme; compile-command: "ol-rl -r puz.scm" -*-

(import
 (owl toplevel)
 (owl lazy)
 (owl random)
 (raylib))

(import
 (puz assets)
 (puz util)
 (puz map)
 (puz move)
 (puz draw)
 (puz map-cache)
 (puz maze)
 )

(define n-mazes 3)

(create-maze 32 16 2137 #\1)
;; (create-maze 16 16 128 #\2)
;; (create-maze 32 32 999 #\3)

(define Maps
  (list
   (load-map-maybe-cache "map.text")
   (load-map-maybe-cache "maze1.text")
   ;; (load-map-maybe-cache "maze2.text")
   ;; (load-map-maybe-cache "maze3.text")
   ))

;; TODO: particle w wątkach?
;; TODO: R - restart

(define (finish textures sounds)
  (lets ((text "TODO: finish")
         (fnt (aq 'font textures))
         (w h (measure-text fnt text 24 0)))
    (with-mainloop
     (draw
      (clear-background black)
      (draw-text
       (aq 'font textures)
       text
       `(,(- (/ width 2) (/ w 2)) ,(- (/ height 2) (/ h 2)))
       24 0 white)))
    (exit-owl 0)))
;; (play-sound (wave->sound (bytevector->wave ".wav" snd-btndown-f))))

;; TODO: reload blocks doors etc after changing the map
(define (maybe-change-map ppos mapq)
  (let ((x (car ppos))
        (y (cadr ppos))
        (Map (aq 'map (lref Maps (car mapq)))))
    (let ((v (lref (lref Map y) x)))
      (cond
       ((maze-start? v)
        (let ((M (lref Maps (- (cadr v) 48))))
          (values (aq 'initial-player-pos M) (append `(,(- (cadr v) 48)) mapq))))
       ((maze-end-of? v)
        (let* ((M (aq 'map (lref Maps (cadr mapq))))
               (P (find-thing (λ (x) (equal? x (list 'maze-end (cadr v)))) M)))
          (values (list (+ (car P) 1) (cadr P)) (cdr mapq))))
       (else
        (values ppos mapq))))))

(define (main _)
  (set-target-fps! 30)
  (with-window
   width height "puz"
   (let* ((_ (init-audio-device)) ;; lol!
          (sounds (load-sounds))
          (textures (load-textures))
          (finish-f (λ () (finish textures sounds)))) ;; wow
     (for-each (λ (t) (set-texture-filter! (cdr t) texture-filter-bilinear)) textures)
     (let loop ((ppos (aq 'initial-player-pos (car Maps)))
                (camera-pos (real-p (aq 'initial-player-pos (car Maps))))
                (key-queue ())
                (blocks (aq 'initial-blocks (car Maps)))
                (buttons (aq 'initial-button-states (car Maps)))
                (undo ())
                (mapq `(0))
                (debug #f))
       (lets ((ppos-prev ppos)
              (key-queue (append key-queue (current-keys)))
              (ppos key-queue blocks buttons
                (dispatch-move (aq 'map (lref Maps (car mapq))) ppos key-queue blocks buttons sounds finish-f))
              (ppos (maybe-door (aq 'doors (lref Maps (car mapq))) ppos sounds))
              (ppos mapq (maybe-change-map ppos mapq))
              (debug (if (key-pressed? key-g) (not debug) debug))
              (camera camera-pos (camera ppos camera-pos))
              ;; maybe do undo?
              (ppos blocks mapq undo (if (key-pressed? key-u)
                                         (let ((lu (lref undo (max 0 (- (length undo) 2)))))
                                           (play-sound (aq 'undo sounds))
                                           (values (car lu) (cadr lu) (caddr lu) (ldel undo (- (length undo) 1))))
                                         (values ppos blocks mapq undo))))
         (draw
          (clear-background black)
          (draw-background-textures textures)

          (with-camera2d
           camera
           (begin ;; TODO: ugly hack - fix with-camera2d macro
             (when debug (draw-grid-lines))
             (draw-map (aq 'map (lref Maps (car mapq))) ppos textures buttons)
             (draw-blocks blocks textures)
             (draw-player ppos textures)))

          (when debug
            (draw-text (aq 'font textures) (str* buttons) '(0 0) 32 0 white))

          ;; the shadow thingy
          (draw-rectangle
           `(0 0 ,width ,height)
           (color 0 0 0 (clamp 0 255 (- (floor (/ (vec2dist (real-p ppos) camera-pos) 4)) 25))))
          (draw-fps '(0 0)))

         (let ((undo (if (equal? (last undo ()) (list ppos blocks mapq))
                         undo
                         (append undo (list (list ppos blocks mapq))))))
           (if (window-should-close?)
               0
               (loop
                ppos
                camera-pos
                key-queue
                blocks
                buttons
                undo
                mapq
                debug))))))))

main
