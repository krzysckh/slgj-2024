(define-library (puz assets)
  (import
   (owl toplevel)
   (raylib))

  (export
    font-f bg-f door-f edg-f uniq-f snd-btndown-f snd-mvblock-f snd-undo-f snd-door-f
    load-sounds load-textures
   )

  (begin
    (define font-f (list->bytevector (file->list "assets/proggy-square.ttf")))
    (define bg-f   (list->bytevector (file->list "assets/AngbandTk/dg_grounds32.gif")))
    (define door-f (list->bytevector (file->list "assets/AngbandTk/dg_dungeon32.png")))
    (define edg-f  (list->bytevector (file->list "assets/AngbandTk/dg_edging232.png")))
    (define uniq-f (list->bytevector (file->list "assets/AngbandTk/dg_uniques32.gif")))

    (define snd-btndown-f (list->bytevector (file->list "assets/btndown.wav")))
    (define snd-mvblock-f (list->bytevector (file->list "assets/blockmove.wav")))
    (define snd-undo-f    (list->bytevector (file->list "assets/undo.wav")))
    (define snd-door-f    (list->bytevector (file->list "assets/door.wav")))
    (define snd-walk-f    (list->bytevector (file->list "assets/walk.wav")))

    (define amb-f (list->bytevector (file->list "assets/amb0.ogg")))

    (define (load-sounds)
      (let ((snd-btndown (wave->sound (bytevector->wave ".wav" snd-btndown-f)))
            (snd-mvblock (wave->sound (bytevector->wave ".wav" snd-mvblock-f)))
            (snd-undo    (wave->sound (bytevector->wave ".wav" snd-undo-f)))
            (snd-door    (wave->sound (bytevector->wave ".wav" snd-door-f)))
            (snd-walk    (wave->sound (bytevector->wave ".wav" snd-walk-f)))
            (amb         (bytevector->music-stream ".ogg" amb-f))
            )
        `((btndown . ,snd-btndown)
          (mvblock . ,snd-mvblock)
          (undo    . ,snd-undo)
          (door    . ,snd-door)
          (walk    . ,snd-walk)
          (amb     . ,amb)
          )))

    (define (load-textures)
      (let ((font (bytevector->font font-f ".ttf" 64 1024))
            (door-tiles (image->texture (list->image ".png" door-f)))
            (bg-tiles   (image->texture (list->image ".gif" bg-f)))
            (edg-tiles  (image->texture (list->image ".png" edg-f)))
            (uniq-tiles (image->texture (list->image ".gif" uniq-f)))
            )
        `((font . ,font)
          (bg   . ,bg-tiles)
          (door . ,door-tiles)
          (edg  . ,edg-tiles)
          (uniq . ,uniq-tiles)
          )))
    ))
