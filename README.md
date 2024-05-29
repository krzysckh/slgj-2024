## puz

This game was written in owl lisp with [raylib-owl](https://github.com/krzysckh/raylib-owl/) for the Spring Lisp Game Jam 2024.

You can play or download this game [on itch.io](https://krzysckh.itch.io/puz).

### Dev dependencies
- Required
  * [raylib-owl](https://github.com/krzysckh/raylib-owl/) built with raylib configured with OGG, WAV, PNG and GIF format support,
  * raylib headers
  * a local C compiler
- Optional
  * a mingw-w64 cross-compiler
  * wine with 32-bit support
  * emscripten (tested on 3.1.6)

### Running with `ol-rl`:

```sh
$ ol-rl -r puz.scm
```

### Compiling with `ol-rl`:

```sh
$ ol-rl -o puz.c puz.scm
$ cc -o puz puz.c -lraylib -lm -lpthread
```

### Map format:

* let C = `/[a-zA-Z]/`
* let A = `/[!-~]/`
* let S = `/[a-zA-Z]*/`
* let N = `/[0-9]+/`

|object  |description                                                                                                |
|--------|-----------------------------------------------------------------------------------------------------------|
|@       |you                                                                                                        |
|_       |background (no block at all)                                                                               |
|=       |level border (may be drawn as a fancy textured block)                                                      |
|(space) |level space or background (will get floodfilled with bg at compile-time)                                   |
|C       |always open doors: uppercase = where to TP; lowercase = from where to tp + where to draw the door texture. |
|\#      |movable box                                                                                                |
|.A      |button with id = A                                                                                         |
|\|A     |openable doors, open only when button with corresponding id = A is pressed (by player or a box)            |
|!A      |closeable doors, open only when button with corresponding id = A is **not** pressed                        |
|%S%     |normal text (with size = 32)                                                                               |
|^S^     |small text (with size = 16)                                                                                |
|&N      |start of a maze with map id = N                                                                            |
|*N      |end of a maze with map id = N                                                                              |
|$       |The end                                                                                                    |
