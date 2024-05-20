#!/bin/sh

set -xe

# TODO: win x64

[ -f ol-rl.exe ] || wget https://pub.krzysckh.org/ol-rl.exe
[ -f libraylib5-winlegacy.a ] || wget https://pub.krzysckh.org/libraylib5-winlegacy.a
[ -f libraylib5-web.a ] || wget https://pub.krzysckh.org/libraylib5-web.a

MAIN="g.scm"
TARGET="g"

OLFLAGS="" # -O2?

CC=clang
MCC32=i686-w64-mingw32-gcc

local_arch=`$CC -dumpmachine`
mingw_arch=`$MCC32 -dumpmachine`

CFLAGS="-I/usr/local/include"
LDFLAGS="-L/usr/local/lib -lraylib -lm"

build_local() {
  ol-rl $OLFLAGS -o g.c $MAIN
  $CC -o "$TARGET-$local_arch" $CFLAGS g.c $LDFLAGS
}

build_mingw() {
  wine ol-rl.exe $OLFLAGS -o g-win.c $MAIN
  $MCC32 -o "$TARGET-$mingw_arch.exe" $CFLAGS g-win.c -L. -l:libraylib5-winlegacy.a \
    -lm -lopengl32 -lwinmm -lgdi32 -lws2_32 -static
}

build_web() {
  ol-rl $OLFLAGS -o g.c $MAIN
  emcc -O1 -DPLATFORM_WEB -I/usr/local/include g.c \
       libraylib5-web.a -o $TARGET-`emcc -dumpmachine`.html \
       -s USE_GLFW=3 -s ERROR_ON_UNDEFINED_SYMBOLS=0 \
       -s ALLOW_MEMORY_GROWTH=1 -s ASYNCIFY -s ASSERTIONS=0
}

build_local
build_mingw
build_web
