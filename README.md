This is an incomplete set of FFI bindings for the GraphicsMagick
library. You will need to install GraphicsMagick first -- see:
   
   http://www.graphicsmagick.org/


I have tested these bindings with GraphicsMagick 1.2, which is
available as a "snapshot" from the above URL. The library will not
build with GraphicsMagick 1.1.10 or 1.1.11 (the most recent
released version as of this writing). If it's important to you to
be able to use this library with a stable released version of
GraphicsMagick, let me know.

The Haddock documentation only includes type signatures so far, and
that's not good enough. However, the API calls should mostly map onto
those documented at:

   http://www.graphicsmagick.org/www/api.html

Tests can be found in:
   Graphics.Transform.Magick.Test
However, right now all tests are commented out since I didn't have
time to change them to use non-hard-wired file names. I hope it should
be obvious what my intentions were. A less ad hoc test framework would
be better yet.

Please submit bug reports, questions, feedback, complaints, praise,
and especially patches (including documentation patches) to the 
maintainer at:

     vincent_AT_xenbox.fr

I did a lot of this work during the second Haskell Hackathon (Hac II
'07) in Freiburg in September 2007. I'd like to thank all the
attendees at Hac II for their moral support, particularly Duncan
Coutts for help with the FFI, as well as: Mark Jones, the members of
the Portland Functional Programming Study Group, and David MacIver,
for their encouragement.

  -- Tim Chevalier
     Portland, Oregon
     April 6, 2008

=== Contributors ===

Thanks to "nonowarn", "Steffen Siering", "ricree", "nek0", "Sean Hess"
for contributing patches.

Installation
------------

This package requires graphicsmagic, lcms, jasper, and libwmf

OSX Installation instructions (tested on 10.10)

    brew install GraphicsMagick lcms jasper libwmf

    cabal install hsmagick


