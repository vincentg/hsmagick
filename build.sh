#!/bin/sh -x
SRCDIR=Graphics/Transform/Magick
hsc2hs -I/usr/local/include/GraphicsMagick $SRCDIR/Images.hsc -o $SRCDIR/Images.hs
hsc2hs -I/usr/local/include/GraphicsMagick $SRCDIR/FFIHelpers.hsc -o $SRCDIR/FFIHelpers.hs
# pthread has to be before X11, else it gets the wrong version of something
FLAGS="-i$HOME/HsLib -lGraphicsMagick -Wall -Werror -debug \
-I/usr/local/include/GraphicsMagick -L/usr/local/lib -L/usr/lib \
-lGraphicsMagick -llcms -ltiff -lfreetype -ljasper -ljpeg -lpng \
-lwmflite -lSM -lICE -lX11 -lbz2 -lxml2 -lz -lm -lpthread -fglasgow-exts" 
$HOME/GhcTrees/complete-2008-work/compiler/ghc-inplace --make $FLAGS -c Graphics.Transform.Magick.Images 
ar cr libhsmagick.a $SRCDIR/Images.o $SRCDIR/Types.o $SRCDIR/Magick.o


