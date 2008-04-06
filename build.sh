#!/bin/sh -x
hsc2hs -I/usr/local/include/GraphicsMagick Images_in.hs -o Images.hs
hsc2hs -I/usr/local/include/GraphicsMagick FFIHelpers_in.hs -o FFIHelpers.hs
# pthread has to be before X11, else it gets the wrong version of something
FLAGS="-i$HOME/HsLib -lGraphicsMagick -Wall -Werror -debug \
-I/usr/local/include/GraphicsMagick -L/usr/local/lib -L/usr/lib \
-lGraphicsMagick -llcms -ltiff -lfreetype -ljasper -ljpeg -lpng \
-lwmflite -lSM -lICE -lX11 -lbz2 -lxml2 -lz -lm -lpthread -fglasgow-exts" 
ghc --make $FLAGS -c Images.hs
#ar cr libhsmagick.a Images.o Types.o Magick.o


