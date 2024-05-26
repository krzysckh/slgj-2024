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

;; (create-maze 16 16 128 #\2)
;; (create-maze 32 32 999 #\3)

(define-syntax at-runtime
  (syntax-rules ()
    ((at-runtime mf)
     (λ ()
       (set-target-fps! (<< 2 32))
       (let ((v (save-cache (load-map-from-memory mf) #f)))
         (set-target-fps! target-fps)
         v)))))

(define Maps-init
  (list
   (load-map-maybe-cache "map.text")
   (at-runtime (create-maze 8 8 (time-ms) #\1))
   (at-runtime (create-maze 32 16 (time-ms) #\2))
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
(define (maybe-change-map Maps ppos mapq blocksq buttonsq)
  (let ((x (car ppos))
        (y (cadr ppos))
        (Map (aq 'map (lref Maps (car mapq)))))
    (let ((v (lref (lref Map y) x)))
      (cond
       ((maze-start? v)
        (let* ((M (lref Maps (- (cadr v) 48)))
               (M (if (function? M) (M) M)))
          (values (lset Maps (- (cadr v) 48) M)
                  (aq 'initial-player-pos M)
                  (append `(,(- (cadr v) 48)) mapq)
                  (append '(()) blocksq)
                  (append '(()) buttonsq))))
       ((maze-end-of? v)
        (let* ((M (aq 'map (lref Maps (cadr mapq))))
               (P (find-thing (λ (x) (equal? x (list 'maze-end (cadr v)))) M)))
          (values Maps (list (+ (car P) 1) (cadr P)) (cdr mapq) (cdr blocksq) (cdr buttonsq))))
       (else
        (values Maps ppos mapq blocksq buttonsq))))))

(define (main _)
  (set-target-fps! target-fps)
  (with-window
   width height "puz"
   (let* ((_ (init-audio-device)) ;; lol!
          (sounds (load-sounds))
          (textures (load-textures))
          (finish-f (λ () (finish textures sounds)))) ;; wow
     (for-each (λ (t) (set-texture-filter! (cdr t) texture-filter-bilinear)) textures)
     (let loop ((ppos (aq 'initial-player-pos (car Maps-init)))
                (camera-pos (real-p (aq 'initial-player-pos (car Maps-init))))
                (key-queue ())
                (blocksq (list (aq 'initial-blocks (car Maps-init))))
                (buttonsq (list (aq 'initial-button-states (car Maps-init))))
                (undo ())
                (mapq `(0))
                (Maps Maps-init)
                (debug #f))
       (lets ((ppos-prev ppos)
              (key-queue (append key-queue (current-keys)))
              (blocks (car blocksq))
              (buttons (car buttonsq))
              (ppos (maybe-door (aq 'doors (lref Maps (car mapq))) ppos sounds))
              (Maps ppos mapq blocksq buttonsq (maybe-change-map Maps ppos mapq blocksq buttonsq))
              (ppos key-queue blocks buttons
                (dispatch-move (aq 'map (lref Maps (car mapq))) ppos key-queue (car blocksq) (car buttonsq) sounds finish-f))
              (debug (if (key-pressed? key-g) (not debug) debug))
              (camera camera-pos (camera ppos camera-pos))
              ;; maybe do undo?
              (ppos blocks mapq undo (if (key-pressed? key-u)
                                         (let ((lu (lref undo (max 0 (- (length undo) 2)))))
                                           (play-sound (aq 'undo sounds))
                                           (values (car lu) (cadr lu) (caddr lu) (ldel undo (- (length undo) 1))))
                                         (values ppos blocks mapq undo)))
              (blocksq (lset blocksq 0 blocks))
              (buttonsq (lset buttonsq 0 buttons)))
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
                blocksq
                buttonsq
                undo
                mapq
                Maps
                debug))))))))

main
