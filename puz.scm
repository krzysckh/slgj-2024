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
 )

(define Mapd (load-map-maybe-cache "map.text"))

(define Map                    (aq 'map Mapd))
(define doors                  (aq 'doors Mapd))
(define initial-blocks         (aq 'initial-blocks Mapd))
(define initial-button-states  (aq 'initial-button-states Mapd))
(define initial-player-pos     (aq 'initial-player-pos Mapd))

;; (define doors-closed
;;   '((3 0)
;;     (0 1) (3 1)
;;     (3 3) (6 3)
;;     (0 4) (3 4) (6 4)))

;; c = char | (f(x) → #t|#f) → (...)

;; TODO: particle w wątkach?
;; TODO: R - restart

(define (finish textures sounds)
  (let* ((text "TODO: finish, but that's that. that's the game")
         (w (measure-text text)))
    (with-mainloop
     (draw
      (clear-background black)
      (draw-text
       (aq 'font textures)
       text
       `(,(- (/ width 2) (/ w 2)) ,(- (/ height 2) (/ 24 2)))
       24 white)))
    (exit-owl 0)))
;; (play-sound (wave->sound (bytevector->wave ".wav" snd-btndown-f))))

(define (main _)
  (set-target-fps! 30)
  (with-window
   width height "λ-test"
   (let* ((_ (init-audio-device)) ;; lol!
          (sounds (load-sounds))
          (textures (load-textures))
          (finish-f (λ () finish textures sounds))) ;; wow
     (for-each (λ (t) (set-texture-filter! (cdr t) texture-filter-bilinear)) textures)
     (let loop ((ppos initial-player-pos)
                (camera-pos (real-p initial-player-pos))
                (key-queue ())
                (blocks initial-blocks)
                (buttons initial-button-states)
                (undo ())
                (debug #f))
       (lets ((ppos-prev ppos)
              (key-queue (append key-queue (current-keys)))
              (ppos key-queue blocks buttons (dispatch-move Map ppos key-queue blocks buttons sounds finish-f))
              (ppos (maybe-door doors ppos sounds))
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
             (draw-map Map ppos textures buttons)
             (draw-blocks blocks textures)
             (draw-player ppos textures)))

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
