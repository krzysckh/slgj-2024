#!/bin/sh

set -xe

[ -f ol-rl.exe ] || wget https://pub.krzysckh.org/ol-rl.exe
[ -f libraylib5-winlegacy.a ] || wget https://pub.krzysckh.org/libraylib5-winlegacy.a
[ -f libraylib5.a ] || wget https://pub.krzysckh.org/libraylib5.a
[ -f libraylib5-web.a ] || wget https://pub.krzysckh.org/libraylib5-web.a
[ -f ol-rl-x86_64-linux-gnu ] || wget https://pub.krzysckh.org/ol-rl-x86_64-linux-gnu

MAIN="g.scm"
TARGET="slgj2024"

OLFLAGS="" # -O2?

CC=clang
MCC32=i686-w64-mingw32-gcc
MCC=x86_64-w64-mingw32-gcc

CFLAGS="-I/usr/local/include"
LDFLAGS="-L/usr/local/lib -lraylib -lm"

rm -rf build
mkdir -p build

build_local() {
  ARCH=`$CC -dumpmachine`
  ol-rl $OLFLAGS -o g.c $MAIN
  $CC -o "$TARGET-$ARCH" $CFLAGS g.c $LDFLAGS
  mv "$TARGET-$ARCH" build/
}

build_mingw32() {
  ARCH=`$MCC32 -dumpmachine`
  wine ol-rl.exe $OLFLAGS -o g-win.c $MAIN
  $MCC32 -o "$TARGET-$ARCH.exe" $CFLAGS g-win.c -L. -l:libraylib5-winlegacy.a \
    -lm -lopengl32 -lwinmm -lgdi32 -lws2_32 -static
  mv "$TARGET-$ARCH.exe" build/
}

build_mingw() {
  ARCH=`$MCC -dumpmachine`
  wine ol-rl.exe $OLFLAGS -o g-win.c $MAIN
  $MCC -o "$TARGET-$ARCH.exe" $CFLAGS g-win.c -L. -l:libraylib5.a \
    -lm -lopengl32 -lwinmm -lgdi32 -lws2_32 -static
  mv "$TARGET-$ARCH.exe" build/
}

build_web() {
  chmod +x ./ol-rl-x86_64-linux-gnu
  ./ol-rl-x86_64-linux-gnu $OLFLAGS -o g.c $MAIN
  ARCH=`emcc -dumpmachine`
  emcc -O1 -DPLATFORM_WEB -I/usr/local/include g.c \
       libraylib5-web.a -o $TARGET-$ARCH.html \
       -s USE_GLFW=3 -s ERROR_ON_UNDEFINED_SYMBOLS=0 \
       -s ALLOW_MEMORY_GROWTH=1 -s ASYNCIFY -s ASSERTIONS=0 || true
  mv $TARGET-$ARCH.html index.html

  zip "$TARGET-$ARCH.zip" index.html "$TARGET-$ARCH.js" "$TARGET-$ARCH.wasm"
  mv "$TARGET-$ARCH.zip" build/
}

# build_local
build_mingw32
build_mingw
build_web
