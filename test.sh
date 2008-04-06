#!/bin/sh
hsc2hs -I/usr/local/include/GraphicsMagick Images_in.hs -o Images.hs
hsc2hs -I/usr/local/include/GraphicsMagick FFIHelpers_in.hs -o FFIHelpers.hs
hsc2hs -I/usr/local/include/GraphicsMagick test_in.hs -o test.hs
if [ $? -eq 0 ]; then
    FLAGS="-i$HOME/HsLib -lGraphicsMagick -Wall -Werror -debug \
	-I/usr/local/include/GraphicsMagick -L/usr/local/lib -L/usr/lib \
	-lGraphicsMagick -llcms -ltiff -lfreetype -ljasper -ljpeg -lpng \
	-lwmflite -lSM -lICE -lX11 -lbz2 -lxml2 -lz -lm -lpthread -fglasgow-exts"

    ghc  $FLAGS -i$HOME/HsLib -o test --make test.hs -prof -no-recomp
    if [ $? -eq 0 ]; then
	./test +RTS -xc -RTS
    else
	echo "\/\/\/\/\/ compilation had errors \/\/\/\/\/"
        exit 1
    fi
else
  echo "\/\/\/\/\/ hsc2hs had errors \/\/\/\/\/"
  exit 2
fi

