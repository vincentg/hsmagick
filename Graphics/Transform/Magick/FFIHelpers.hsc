{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE PatternSignatures, FlexibleInstances #-}
module Graphics.Transform.Magick.FFIHelpers(withExceptions,
                  withExceptions_,
                  setField,
                  (-->),
                  setFilename,
                  getFilename,
                  setPage,
                  doTransform,
                  doTransformIO,
                  doTransformIO_XY,
                  doTransformIO_XY_real,
                  sideEffectingOp,
                  linkImagesTogether,
                  mkNewExceptionInfo,
                  mkNewImageInfo,
                  toCEnum,
                  hImageRows,
                  hImageColumns,
                  maybeToPtr,
                  mkNewUnloadedImage) where

#include <magick/api.h>

import Graphics.Transform.Magick.Types
import Graphics.Transform.Magick.Magick
import Graphics.Transform.Magick.Errors
import Graphics.Transform.Magick.Util

import Control.Exception
import Prelude hiding (maximum, minimum)

-- functions to help with doing FFI 

-------------- Strings/char arrays  
-- This is really terrible. How to avoid these casts?
pokeStringIntoCharArray :: Ptr CharArray -> String -> IO ()
pokeStringIntoCharArray ptr s = go (castPtr ptr) s
               where go :: Ptr CChar -> String -> IO ()
                     go p [] = poke p nullChar
                     go p (c:cs) = do
                        debug 3 $ "p = " ++ show p ++ " c = " ++ show c
                        poke p (castCharToCChar c)
                        go (p `plusPtr` charSize) cs

peekStringFromCharArray :: Ptr CharArray -> IO String
peekStringFromCharArray ptr = 
   (debug 3 $ "peekStringFromCharArray: ptr = " ++ show ptr) 
   >> go (castPtr ptr) ""
      where go :: Ptr CChar -> String -> IO String
            go p s = do
              debug 3 $ "p = " ++ show p
              c <- (liftM castCCharToChar) $ peek p
              debug 3 $ " c = " ++ show c
              if c == '\0'
                then return s
                else go (p `plusPtr` charSize) (s ++ [c])
 
charSize :: Int
charSize = sizeOf (undefined::CChar)
nullChar :: CChar
nullChar = castCharToCChar '\0'

-------- sets a field in something Storable --------
setField :: Storable a => (a -> a) -> Ptr a -> IO ()
setField modify p = peek p >>= ((poke p).modify)
(-->) :: Storable a => Ptr a -> (a -> b) -> b
(-->) p sel = unsafePerformIO $ peek p >>= (return.sel)
---------------------------------

-- Function for handling exceptions from GraphicsMagick calls.
-- Takes an IO action (that's presumably a call to a GraphicsMagick function),
-- an error message to print if something goes wrong, and a function to 
-- determine whether the result of the call was erroneous, as well as a pointer
-- to the exception info that the action will set.
-- The checker function is assumed to return True if there was an error.
withExceptions :: IO a -> String -> (a -> Bool) -> (Ptr ExceptionInfo) -> IO a
withExceptions action errMsg checker excPtr = do
  result <- action
  if (checker result)
    then do
       -- this prints out GraphicsMagick's message
       tellUser "hsMagick: caught a GraphicsMagick exception as follows: "
       catch_exception excPtr
       signalException errMsg
    else return result
-- Same as withExceptions, but throws away the result
withExceptions_ :: IO a -> String -> (a -> Bool) -> Ptr ExceptionInfo -> IO ()
withExceptions_ action errMsg checker excPtr =
  withExceptions action errMsg checker excPtr >> return ()
 
-- Note: for a plain Image -> Exception -> Image function, we should
-- call doTransform. For transformations that take extra arguments,
-- we use doTransformIO.

-- doTransform takes an image transformation that takes an 
-- image pointer and an exception pointer as arguments, and applies it
-- to the given HImage.
-- It's assumed that the transformer returns null if an error occurs,
-- so this checks for null and looks at the exception field.
doTransform :: (Ptr HImage_ -> Ptr ExceptionInfo 
                             -> IO (Ptr HImage_)) -> HImage -> HImage
doTransform transform hImage = 
  doTransformIO (transform (getImage hImage) excInfo) hImage
       where excInfo = getExceptionInfo hImage      

-- doTransformIO takes an arbitrary IO action that returns an HImage_
-- pointer, and returns a new HImage with the image field of the given
-- HImage set to it, checking for exceptions.
-- It's assumed that the IO action returns a null pointer to signal
-- an exception.
doTransformIO :: IO (Ptr HImage_) -> HImage -> HImage
doTransformIO act hImage =  
  setImage hImage (unsafePerformIO
                    (withExceptions act
                      -- TODO: better messages
                      "error doing image transformation"
                      (== nullPtr)
                      excInfo))
       where excInfo = getExceptionInfo hImage      

doTransformIO_XY :: (Integral a, Integral b) =>
  (Ptr HImage_ -> a -> a -> Ptr ExceptionInfo -> IO (Ptr HImage_)) ->
  HImage -> b -> b -> HImage 
doTransformIO_XY transform hImage x_ y_ =
  doTransformIO (transform (getImage hImage) (fromIntegral x_) (fromIntegral y_)
                   (getExceptionInfo hImage))
      hImage

-- Ugh.
doTransformIO_XY_real :: (Real b, Fractional a) =>
  (Ptr HImage_ -> a -> a -> Ptr ExceptionInfo -> IO (Ptr HImage_)) ->
  HImage -> b -> b -> HImage 
doTransformIO_XY_real transform hImage x_ y_ =
  doTransformIO (transform (getImage hImage) (realToFrac x_) (realToFrac y_)
                   (getExceptionInfo hImage))
      hImage

------------------ creating image sequences -----------
linkImagesTogether :: [HImage] -> IO ()
linkImagesTogether [] = signalException $ "internal error: linkImagesTogether:"
                         ++ " empty list"
linkImagesTogether (img:images) = do
         foldM (\ bigImage smallImage -> do
                    (#poke Image, next) (getImage bigImage) 
                         (getImage smallImage)
                    return smallImage) 
                    img
                    images
         debug 3 $ "Checking assertion..."
         -- check that: all images but the last one have a non-null "next"
         -- ptr, and also, the last one has a null "next" ptr
         allGood <- allM nextImageNotNull (butLast images)
         lastNull <- (liftM not) (nextImageNotNull (last images)) 
         assertM (allGood && lastNull)
              "flattenImage: internal error: couldn't create sequence"
             where nextImageNotNull hImage = do
                      debug 3 $ "peeking: " ++ show (getImage hImage)
                      nextIm <- (#peek Image, next) (getImage hImage)
                      debug 3 $ "peeked! " ++ show nextIm
                      return $ nextIm /= nullPtr
     

------------------ instances --------------------------
--- (should this be in this module? Who knows?)

instance Storable FilterTypes where
    sizeOf _ = sizeOf (undefined::CUInt)
    alignment _ = alignment (undefined::CUInt)
    peek ptr = do
       -- is this use of cast right?
       (theInt::CUInt) <- peek (castPtr ptr)
       return $ toEnum (fromIntegral theInt)
    poke ptr val = poke (castPtr ptr) (fromEnum val)

-- TODO:
-- could this be auto-generated? boilerplate sux...
-- (fundeps? instance Enum a => Storable a...)
instance Storable CompositeOp where
    sizeOf _ = sizeOf (undefined::CUInt)
    alignment _ = alignment (undefined::CUInt)
    peek ptr = do
       -- is this use of cast right?
       (theInt::CUInt) <- peek (castPtr ptr)
       return $ toEnum (fromIntegral theInt)
    poke ptr val = poke (castPtr ptr) (fromEnum val)

instance Storable ImageCharacteristics where
  sizeOf _    = (sizeOf (undefined::CUInt)) * 5
  alignment _ = alignment (undefined::CUInt)
  peek ptr = do
      cmyk'       <- (#peek ImageCharacteristics, cmyk) ptr
      grayscale'  <- (#peek ImageCharacteristics, grayscale) ptr
      mONOCHROME' <- (#peek ImageCharacteristics, monochrome) ptr
      opaque'     <- (#peek ImageCharacteristics, opaque) ptr
      palette'    <- (#peek ImageCharacteristics, palette) ptr
      return $ ImageC { cmyk=toEnum cmyk', grayscale=toEnum grayscale',
                        mONOCHROME=toEnum mONOCHROME', opaque=toEnum opaque',
                        palette=toEnum palette'}
  poke ptr i = do
      (#poke ImageCharacteristics, cmyk) ptr (fromEnum$ cmyk i)
      (#poke ImageCharacteristics, grayscale) ptr (fromEnum$ grayscale i)
      (#poke ImageCharacteristics, monochrome) ptr (fromEnum$ mONOCHROME i)
      (#poke ImageCharacteristics, opaque) ptr (fromEnum$ opaque i)
      (#poke ImageCharacteristics, palette) ptr (fromEnum$ palette i)

instance Storable ImageStatistics where
    sizeOf _    = 4 * sizeOf (undefined::ImageChannelStatistics)
    alignment _ = alignment (undefined::ImageChannelStatistics)
    peek ptr = do
      red'       <- (#peek ImageStatistics, red) ptr
      green'     <- (#peek ImageStatistics, green) ptr
      blue'      <- (#peek ImageStatistics, blue) ptr
      opacity'   <- (#peek ImageStatistics, opacity) ptr
      return $ ImageS { red_=red', green_=green', blue_=blue', opacity_=opacity' }
    poke ptr i = do
      (#poke ImageStatistics, red) ptr (red_ i)
      (#poke ImageStatistics, green) ptr (green_ i)
      (#poke ImageStatistics, blue) ptr (blue_ i)
      (#poke ImageStatistics, opacity) ptr (opacity_ i)

instance Storable ImageChannelStatistics where
    sizeOf _    = 5 * sizeOf (undefined::CDouble)
    alignment _ = alignment (undefined::CDouble)
    peek ptr = do
      (maximum'::CDouble)            <- (#peek ImageChannelStatistics, maximum) ptr
      (minimum'::CDouble)            <- (#peek ImageChannelStatistics, minimum) ptr
      (mean'::CDouble)               <- (#peek ImageChannelStatistics, mean) ptr
      (standard_deviation'::CDouble) <- (#peek ImageChannelStatistics, standard_deviation) ptr
      (variance'::CDouble)           <- (#peek ImageChannelStatistics, variance) ptr
      return $ ImageCS { maximum=realToFrac maximum', minimum=realToFrac minimum',
                         mean=realToFrac mean', standard_deviation=realToFrac standard_deviation',
                         variance=realToFrac variance' }
    poke ptr i = do
      (#poke ImageChannelStatistics, maximum) ptr (maximum i)
      (#poke ImageChannelStatistics, minimum) ptr (minimum i)
      (#poke ImageChannelStatistics, mean) ptr (mean i)
      (#poke ImageChannelStatistics, standard_deviation) ptr (standard_deviation i)
      (#poke ImageChannelStatistics, variance) ptr (variance i)


instance Storable ExceptionInfo where
  sizeOf _    = 32 -- TODO
  alignment _ = alignment (undefined::CULong)
  peek ptr = do
      severity'     <- (#peek ExceptionInfo, severity     ) ptr
      reason'       <- (#peek ExceptionInfo, reason       ) ptr
      description'  <- (#peek ExceptionInfo, description  ) ptr
      error_number' <- (#peek ExceptionInfo, error_number ) ptr
      mODULE'       <- (#peek ExceptionInfo, module       ) ptr
      function'     <- (#peek ExceptionInfo, function     ) ptr
      line'         <- (#peek ExceptionInfo, line         ) ptr
      signature__'  <- (#peek ExceptionInfo, signature    ) ptr
      return $ ExceptionInfo { severity=severity',
                               reason=reason',
                               description=description',
                               error_number=error_number',
                               mODULE=mODULE',
                               function=function',
                               line=line',
                               signature__=signature__'}
  poke ptr e = do
      (#poke ExceptionInfo, severity      ) ptr (severity e    )
      (#poke ExceptionInfo, reason        ) ptr (reason e      )
      (#poke ExceptionInfo, description   ) ptr (description e )
      (#poke ExceptionInfo, error_number  ) ptr (error_number e)
      (#poke ExceptionInfo, module        ) ptr (mODULE e      )
      (#poke ExceptionInfo, function      ) ptr (function e    )
      (#poke ExceptionInfo, line          ) ptr (signature__ e )

-- it's unfortunate that we have to write this twice
-- (maybe there's some wackier type system feature that
-- would let us not do so)
instance Storable (PixelPacket Word8) where
   sizeOf _ = 4*(sizeOf(undefined::Word8))
   alignment _ = alignment (undefined::Word8)
   peek ptr = do
      red'     <- (#peek PixelPacket, red) ptr
      green'   <- (#peek PixelPacket, green) ptr
      blue'    <- (#peek PixelPacket, blue) ptr
      opacity' <- (#peek PixelPacket, opacity) ptr
      return $ PixelPacket{ red=red', green=green',
                            blue=blue', opacity=opacity' }
   poke ptr p = do
      (#poke PixelPacket, red) ptr (red p)
      (#poke PixelPacket, blue) ptr (blue p)
      (#poke PixelPacket, green) ptr (green p)
      (#poke PixelPacket, opacity) ptr (opacity p)

instance Storable CharArray where
  sizeOf _ = maxTextExtent
  alignment _ = 1
  peek _ = error "CharArray: peek is not implemented"
  poke _ _ = error "CharArray: poke is not implemented"

instance Storable HImageInfo where
  sizeOf _ = (#size ImageInfo)
  alignment _ = alignment (undefined::CULong)
  peek ptr = do
    -- again, ugh
      compression' <- (#peek ImageInfo, compression) ptr
      temporary'   <- (#peek ImageInfo, temporary) ptr
      adjoin'      <- (#peek ImageInfo, adjoin) ptr
      antialias'   <- (#peek ImageInfo, antialias) ptr
      subimage'    <- (#peek ImageInfo, subimage) ptr
      subrange'    <- (#peek ImageInfo, subrange) ptr
      depth'       <- (#peek ImageInfo, depth) ptr
      size'        <- (#peek ImageInfo, size) ptr
      tile'        <- (#peek ImageInfo, tile) ptr
      page'        <- (#peek ImageInfo, page) ptr
      interlace'   <- (#peek ImageInfo, interlace) ptr
      endian'      <- (#peek ImageInfo, endian) ptr
      units'       <- (#peek ImageInfo, units) ptr
      quality'     <- (#peek ImageInfo, quality) ptr
      sampling_factor' <- (#peek ImageInfo, sampling_factor) ptr
      server_name' <- (#peek ImageInfo, server_name) ptr
      font'        <- (#peek ImageInfo, font) ptr
      texture'     <- (#peek ImageInfo, texture) ptr
      density'     <- (#peek ImageInfo, density) ptr
      pointsize'   <- (#peek ImageInfo, pointsize) ptr
      fuzz'        <- (#peek ImageInfo, fuzz) ptr
      pen'         <- (#peek ImageInfo, pen) ptr
      background_color' <- (#peek ImageInfo, background_color) ptr
      border_color' <- (#peek ImageInfo, border_color) ptr
      matte_color' <- (#peek ImageInfo, matte_color) ptr
      dither'      <- (#peek ImageInfo, dither) ptr
      monochrome'  <- (#peek ImageInfo, monochrome) ptr
      colorspace'  <- (#peek ImageInfo, colorspace) ptr
      tYPE'        <- (#peek ImageInfo, type) ptr
      group'       <- (#peek ImageInfo, group) ptr
      verbose'     <- (#peek ImageInfo, verbose) ptr
      view'        <- (#peek ImageInfo, view) ptr
      progress'    <- (#peek ImageInfo, progress) ptr
      authenticate' <- (#peek ImageInfo, authenticate) ptr
      client_data' <- (#peek ImageInfo, client_data) ptr
      stream'      <- (#peek ImageInfo, stream) ptr
      file'        <- (#peek ImageInfo, file) ptr
      magick'      <- peekStringFromCharArray $ (#ptr ImageInfo, magick) ptr
      filename'    <- peekStringFromCharArray $ (#ptr ImageInfo, filename) ptr
      cache'       <- (#peek ImageInfo, cache) ptr
      definitions' <- (#peek ImageInfo, definitions) ptr
      attributes'  <- (#peek ImageInfo, attributes) ptr
      ping'        <- (#peek ImageInfo, ping) ptr
      preview_type' <- (#peek ImageInfo, preview_type) ptr
      affirm'      <- (#peek ImageInfo, affirm) ptr
      blob'        <- (#peek ImageInfo, blob) ptr
      lENGTH'      <- (#peek ImageInfo, length) ptr
      unique'      <- (#peek ImageInfo, unique) ptr
      zero'        <- (#peek ImageInfo, zero) ptr
      signature'   <- (#peek ImageInfo, signature) ptr
      return $ HImageInfo{compression=compression',
                          temporary=temporary',
                          adjoin=adjoin',
                          antialias=antialias',
                          subimage=subimage',
                          subrange=subrange',
                          depth=depth',
                          size=size',
                          tile=tile',
                          page=page',
                          interlace=interlace',
                          endian=endian',
                          units=units',
                          quality=quality',
                          sampling_factor=sampling_factor',
                          server_name=server_name',
                          font=font',
                          texture=texture',
                          density=density',
                          pointsize=pointsize',
                          fuzz=fuzz',
                          pen=pen',
                          background_color=background_color',
                          border_color=border_color',
                          matte_color=matte_color',
                          dither=dither',
                          monochrome=monochrome',
                          colorspace=colorspace',
                          tYPE=tYPE',
                          group=group',
                          verbose=verbose',
                          view=view',
                          progress=progress',
                          authenticate=authenticate',
                          client_data=client_data',
                          stream=stream',
                          file=file',
                          magick=magick',
                          filename=filename',
                          cache=cache',
                          definitions=definitions',
                          attributes=attributes',
                          ping=ping',
                          preview_type=preview_type',
                          affirm=affirm',
                          blob=blob',
                          lENGTH=lENGTH',
                          unique=unique',
                          zero=zero',
                          signature=signature'}
  poke ptr hImageInfo = do
      -- ugh, boilerplate. is there a way to auto-generate this?
      (#poke ImageInfo, compression) ptr (compression hImageInfo)
      (#poke ImageInfo, temporary) ptr (temporary hImageInfo)
      (#poke ImageInfo, adjoin) ptr (adjoin hImageInfo)
      (#poke ImageInfo, antialias) ptr (antialias  hImageInfo)
      (#poke ImageInfo, subimage) ptr (subimage hImageInfo)
      (#poke ImageInfo, subrange) ptr (subrange hImageInfo)
      (#poke ImageInfo, depth)    ptr (depth hImageInfo)
      (#poke ImageInfo, size) ptr (size  hImageInfo)
      (#poke ImageInfo, tile) ptr (tile hImageInfo)
      (#poke ImageInfo, page) ptr (page hImageInfo)
      (#poke ImageInfo, interlace) ptr (interlace hImageInfo)
      (#poke ImageInfo, endian     ) ptr (endian hImageInfo)
      (#poke ImageInfo, units      ) ptr (units hImageInfo)
      (#poke ImageInfo, quality    ) ptr (quality hImageInfo    )
      (#poke ImageInfo, sampling_factor) ptr (sampling_factor hImageInfo)
      (#poke ImageInfo, server_name) ptr (server_name hImageInfo)
      (#poke ImageInfo, font       ) ptr (font hImageInfo)
      (#poke ImageInfo, texture    ) ptr (texture hImageInfo    )
      (#poke ImageInfo, density    ) ptr (density hImageInfo    )
      (#poke ImageInfo, pointsize  ) ptr (pointsize hImageInfo  )
      (#poke ImageInfo, fuzz       ) ptr (fuzz hImageInfo       )
      (#poke ImageInfo, pen        ) ptr (pen hImageInfo        )
      (#poke ImageInfo, background_color) ptr (background_color hImageInfo)
      (#poke ImageInfo, border_color) ptr (border_color hImageInfo)
      (#poke ImageInfo, matte_color) ptr (matte_color hImageInfo)
      (#poke ImageInfo, dither     ) ptr (dither hImageInfo    )
      (#poke ImageInfo, monochrome ) ptr (monochrome hImageInfo )
      (#poke ImageInfo, colorspace ) ptr (colorspace hImageInfo)
      (#poke ImageInfo, type       ) ptr (tYPE hImageInfo       )
      (#poke ImageInfo, group      ) ptr (group hImageInfo      )
      (#poke ImageInfo, verbose    ) ptr (verbose hImageInfo   )
      (#poke ImageInfo, view       ) ptr (view  hImageInfo     )
      (#poke ImageInfo, authenticate) ptr (authenticate hImageInfo)
      (#poke ImageInfo, client_data) ptr (client_data hImageInfo)
      (#poke ImageInfo, stream     ) ptr (stream hImageInfo    )
      (#poke ImageInfo, file       ) ptr (file hImageInfo       )
      -- the two char-array things: magick and filename
      pokeStringIntoCharArray ((#ptr ImageInfo, magick) ptr) (magick hImageInfo)
      pokeStringIntoCharArray ((#ptr ImageInfo, filename) ptr) (filename hImageInfo)
      --
      (#poke ImageInfo, cache      ) ptr (cache hImageInfo   )
      (#poke ImageInfo, definitions) ptr (definitions hImageInfo)
      (#poke ImageInfo, attributes ) ptr (attributes hImageInfo)
      (#poke ImageInfo, ping       ) ptr (ping       hImageInfo)
      (#poke ImageInfo, preview_type) ptr (preview_type hImageInfo)
      (#poke ImageInfo, affirm     ) ptr (affirm hImageInfo)
      (#poke ImageInfo, blob       ) ptr (blob       hImageInfo)
      (#poke ImageInfo, length     ) ptr (lENGTH     hImageInfo)
      (#poke ImageInfo, unique     ) ptr (unique     hImageInfo)
      (#poke ImageInfo, zero       ) ptr (zero       hImageInfo)
      (#poke ImageInfo, signature  ) ptr (signature  hImageInfo)

instance Storable HImage_ where
  sizeOf _ = (#size Image)
  alignment _ = alignment (undefined::CULong)
  peek ptr = do
      storage_class' <- (#peek Image, storage_class) ptr
      colorspace_'   <- (#peek Image, colorspace) ptr
      compression_'  <- (#peek Image, compression) ptr
      dither_'       <- (#peek Image, dither) ptr
      matte'         <- (#peek Image, matte) ptr
      columns'       <- (#peek Image, columns) ptr
      rows'          <- (#peek Image, rows) ptr
      colors'        <- (#peek Image, colors) ptr
      depth_'        <- (#peek Image, depth) ptr
      colormap'      <- (#peek Image, colormap) ptr
      background_color_' <- (#peek Image, background_color) ptr
      border_color_' <- (#peek Image, border_color) ptr
      matte_color_'  <- (#peek Image, matte_color) ptr
      gamma'         <- (#peek Image, gamma) ptr
      chromaticity'  <- (#peek Image, chromaticity) ptr
      orientation'   <- (#peek Image, orientation) ptr
      rendering_intent' <- (#peek Image, rendering_intent) ptr
      units_'        <- (#peek Image, units) ptr
      montage'       <- (#peek Image, montage) ptr
      directory'     <- (#peek Image, directory) ptr
      geometry'      <- (#peek Image, geometry) ptr
      offset'        <- (#peek Image, offset) ptr
      x_resolution'  <- (#peek Image, x_resolution) ptr
      y_resolution'  <- (#peek Image, y_resolution) ptr
      page_'         <- (#peek Image, page) ptr
      tile_info'     <- (#peek Image, tile_info) ptr
      blur'          <- (#peek Image, blur) ptr
      fuzz_'         <- (#peek Image, fuzz) ptr
      fILTER'        <- (#peek Image, filter) ptr
      interlace_'    <- (#peek Image, interlace) ptr
      endian_'       <- (#peek Image, endian) ptr
      gravity'       <- (#peek Image, gravity) ptr
      compose'       <- (#peek Image, compose) ptr
      dispose'       <- (#peek Image, dispose) ptr
      scene'         <- (#peek Image, scene) ptr
      delay'         <- (#peek Image, delay) ptr
      iterations'    <- (#peek Image, iterations) ptr
      total_colors'  <- (#peek Image, total_colors) ptr
      start_loop'    <- (#peek Image, start_loop) ptr
      eRROR'         <- (#peek Image, error) ptr
      timer'         <- (#peek Image, timer) ptr
      client_data_'  <- (#peek Image, client_data) ptr
      filename_'     <- peekStringFromCharArray ((#ptr Image, filename) ptr)
      magick_filename' <- peekStringFromCharArray ((#ptr Image, magick_filename) ptr)
      magick_'       <- peekStringFromCharArray ((#ptr Image, magick) ptr)
      magick_rows'   <- (#peek Image, magick_rows) ptr
      exception'     <- (#peek Image, exception) ptr
      previous'      <- (#peek Image, previous) ptr
      next'          <- (#peek Image, next) ptr
      profiles'      <- (#peek Image, profiles) ptr
      is_monochrome' <- (#peek Image, is_monochrome) ptr
      is_grayscale'  <- (#peek Image, is_grayscale) ptr
      taint'         <- (#peek Image, taint) ptr
      clip_mask'     <- (#peek Image, clip_mask) ptr
      cache_'        <- (#peek Image, cache) ptr
      attributes_'   <- (#peek Image, attributes) ptr
      ascii85'       <- (#peek Image, ascii85) ptr
      blob_'         <- (#peek Image, blob) ptr
      reference_count' <- (#peek Image, reference_count) ptr
      semaphore'     <- (#peek Image, semaphore) ptr
      logging'       <- (#peek Image, logging) ptr
      list'          <- (#peek Image, list) ptr
      signature_'    <- (#peek Image, signature) ptr
      return $ HImage_ {
                   storage_class=storage_class',
                   colorspace_=colorspace_',
                   compression_=compression_',
                   dither_=dither_',
                   matte=matte',
                   columns=columns',
                   rows=rows',
                   colors=colors',
                   depth_=depth_',
                   colormap=colormap',
                   background_color_=background_color_',
                   border_color_=border_color_',
                   matte_color_=matte_color_',
                   gamma=gamma',
                   chromaticity=chromaticity',
                   orientation=orientation',
                   rendering_intent=rendering_intent',
                   units_=units_',
                   montage=montage',
                   directory=directory',
                   geometry=geometry',
                   offset=offset',
                   x_resolution=x_resolution',
                   y_resolution=y_resolution',
                   page_=page_',
                   tile_info=tile_info',
                   blur=blur',
                   fuzz_=fuzz_',
                   fILTER=fILTER',
                   interlace_=interlace_',
                   endian_=endian_',
                   gravity=gravity',
                   compose=compose',
                   dispose=dispose',
                   scene=scene',
                   delay=delay',
                   iterations=iterations',
                   total_colors=total_colors',
                   start_loop=start_loop',
                   eRROR=eRROR',
                   timer=timer',
                   client_data_=client_data_',
                   filename_=filename_',
                   magick_filename=magick_filename',
                   magick_=magick_',
                   magick_rows=magick_rows',
                   exception=exception',
                   previous=previous',
                   next=next',
                   profiles=profiles',
                   is_monochrome=is_monochrome',
                   is_grayscale=is_grayscale',
                   taint=taint',
                   clip_mask=clip_mask',
                   cache_=cache_',
                   attributes_=attributes_',
                   ascii85=ascii85',
                   blob_=blob_',
                   reference_count=reference_count',
                   semaphore=semaphore',
                   logging=logging',
                   list=list',
                   signature_=signature_'
                 }
  poke ptr hImage = do
               (#poke Image, storage_class) ptr (storage_class hImage)
               (#poke Image, colorspace) ptr (colorspace_ hImage)
               (#poke Image, compression) ptr (compression_ hImage)
               (#poke Image, dither) ptr (dither_ hImage)
               (#poke Image, matte) ptr (matte hImage)
               (#poke Image, columns) ptr (columns hImage)
               (#poke Image, rows) ptr (rows hImage)
               (#poke Image, colors) ptr (colors hImage)
               (#poke Image, depth) ptr (depth_ hImage)
               (#poke Image, colormap) ptr (colormap hImage)
               (#poke Image, background_color) ptr (background_color_ hImage)
               (#poke Image, border_color) ptr (border_color_ hImage)
               (#poke Image, matte_color) ptr (matte_color_ hImage)
               (#poke Image, gamma) ptr (gamma hImage)
               (#poke Image, chromaticity) ptr (chromaticity hImage)
               (#poke Image, orientation) ptr (orientation hImage)
               (#poke Image, rendering_intent) ptr (rendering_intent hImage)
               (#poke Image, units) ptr (units_ hImage)
               (#poke Image, montage) ptr (montage hImage)
               (#poke Image, directory) ptr (directory hImage)
               (#poke Image, geometry) ptr (geometry hImage)
               (#poke Image, offset) ptr (offset hImage)
               (#poke Image, x_resolution) ptr (x_resolution hImage)
               (#poke Image, y_resolution) ptr (y_resolution hImage)
               (#poke Image, page) ptr (page_ hImage)
               (#poke Image, tile_info) ptr (tile_info hImage)
               (#poke Image, blur) ptr (blur hImage)
               (#poke Image, fuzz) ptr (fuzz_ hImage)
               (#poke Image, filter) ptr (fILTER hImage)
               (#poke Image, interlace) ptr (interlace_ hImage)
               (#poke Image, endian) ptr (endian_ hImage)
               (#poke Image, gravity) ptr (gravity hImage)
               (#poke Image, compose) ptr (compose hImage)
               (#poke Image, dispose) ptr (dispose hImage)
               (#poke Image, scene) ptr (scene hImage)
               (#poke Image, delay) ptr (delay hImage)
               (#poke Image, iterations) ptr (iterations hImage)
               (#poke Image, total_colors) ptr (total_colors hImage)
               (#poke Image, start_loop) ptr (start_loop hImage)
               (#poke Image, error) ptr (eRROR hImage)
               (#poke Image, timer) ptr (timer hImage)
               (#poke Image, client_data) ptr (client_data_ hImage)
               pokeStringIntoCharArray ((#ptr Image, filename) ptr) (filename_ hImage)
               pokeStringIntoCharArray ((#ptr Image, magick_filename) ptr) (magick_filename hImage)
               pokeStringIntoCharArray ((#ptr Image, magick) ptr) (magick_ hImage)
               (#poke Image, magick_rows) ptr (magick_rows hImage)
               (#poke Image, exception) ptr (exception hImage)
               (#poke Image, previous) ptr (previous hImage)
               (#poke Image, next) ptr (next hImage)
               (#poke Image, profiles) ptr (profiles hImage)
               (#poke Image, is_monochrome) ptr (is_monochrome hImage)
               (#poke Image, is_grayscale) ptr (is_grayscale hImage)
               (#poke Image, taint) ptr (taint hImage)
               (#poke Image, clip_mask) ptr (clip_mask hImage)
               (#poke Image, cache) ptr (cache_ hImage)
               (#poke Image, attributes) ptr (attributes_ hImage)
               (#poke Image, ascii85) ptr (ascii85 hImage)
               (#poke Image, blob) ptr (blob_ hImage)
               (#poke Image, reference_count) ptr (reference_count hImage)
               (#poke Image, semaphore) ptr (semaphore hImage)
               (#poke Image, logging) ptr (logging hImage)
               (#poke Image, list) ptr (list hImage)
               (#poke Image, signature) ptr (signature_ hImage)

instance Storable Rectangle where
   sizeOf _ = (2*(sizeOf(undefined::CUInt))) + 
              (2*(sizeOf(undefined::CInt)))
   alignment _ = alignment (undefined::CInt)
   peek ptr = do
      width'  <- (#peek RectangleInfo, width)  ptr
      height' <- (#peek RectangleInfo, height) ptr
      x'      <- (#peek RectangleInfo, x)      ptr
      y'      <- (#peek RectangleInfo, y)      ptr
      return $ Rectangle{ width=width', height=height',
                          x=x', y=y'}
   poke ptr rect = do
      (#poke RectangleInfo, width)  ptr (width rect)
      (#poke RectangleInfo, height) ptr (height rect)
      (#poke RectangleInfo, x)      ptr (x rect)
      (#poke RectangleInfo, y)      ptr (y rect)

instance Storable AffineMatrix where
   sizeOf _ = (#size AffineMatrix)
   alignment _ = alignment (undefined::CDouble)
   peek ptr = do
     sx' <- (#peek AffineMatrix, sx) ptr
     rx' <- (#peek AffineMatrix, rx) ptr
     ry' <- (#peek AffineMatrix, ry) ptr
     sy' <- (#peek AffineMatrix, sy) ptr
     tx' <- (#peek AffineMatrix, tx) ptr
     ty' <- (#peek AffineMatrix, ty) ptr
     return $ AffineMatrix { sx=sx', rx=rx', ry=ry', sy=sy', tx=tx', ty=ty' }
   poke ptr mat = do
      (#poke AffineMatrix, sx) ptr (sx mat)
      (#poke AffineMatrix, rx) ptr (rx mat)
      (#poke AffineMatrix, ry) ptr (ry mat)
      (#poke AffineMatrix, sy) ptr (sy mat)
      (#poke AffineMatrix, tx) ptr (tx mat)
      (#poke AffineMatrix, ty) ptr (ty mat)

-- shouldn't really have this magick number here
maxTextExtent :: Int
maxTextExtent = 2053

hImageRows, hImageColumns :: HImage -> Word
hImageRows    = fromIntegral.columns.unsafePerformIO.peek.getImage
hImageColumns = fromIntegral.rows.unsafePerformIO.peek.getImage

--------------- Filename handling

class HasFilename a where
  setFilename :: a -> FilePath -> IO ()
  getFilename :: a -> FilePath

instance HasFilename ImageNotLoaded where
   getFilename (ImageNotLoaded{ imageInfo = iInfo}) = iInfo-->filename
   setFilename (ImageNotLoaded{ imageInfo = iInfo}) s =
     setField (\ info -> info{filename=s}) iInfo

instance HasFilename HImage where
   getFilename(HImage{ image=p, otherInfo=other }) = 
      let filename1  = p-->filename_
          filename2  = getFilename other in
        assert (filename1 == filename2) filename1
   setFilename(HImage{ image=p, otherInfo=other }) s = 
       setFilename other s >>
       setField (\ im -> im{filename_=s}) p

------------- Page setting
setPage :: HImage -> Rectangle -> IO ()
setPage hImage rect = (#poke Image, page) (getImage hImage) rect

------------- Dealing with side-effecting GraphicsMagick functions
sideEffectingOp :: (HImage -> IO CUInt) -> HImage -> HImage
sideEffectingOp impureFun = (\ hImage -> unsafePerformIO $ do
   newImage <- cloneImage hImage
   withExceptions_ (impureFun newImage) "hsMagick: Error doing transformation"
     (== 0) (getExceptionInfo newImage)
   return newImage)

--------- Utils
-- The type emphasizes that we're doing something wantonly
-- non-referentially-transparent
cloneImage :: HImage -> IO HImage
cloneImage hImage = do
   clonedImagePtr      <- cloneImagePtr (getImage hImage)
   clonedImageInfo     <- clone_image_info (getImageInfo hImage)
   clonedExceptionInfo <- mkNewExceptionInfo
   return $ mkImage clonedImagePtr (mkUnloadedImage clonedImageInfo clonedExceptionInfo)
                             -- 0 and 0 say that the cloned image should have the same
                             -- size as the original. 1 says this should be an orphan 
                             -- image (not part of a list.)
     where cloneImagePtr p = withExceptions (clone_image p 0 0 1 (getExceptionInfo hImage))
                                       "cloneImagePtr: error cloning image"
                                       (== nullPtr)
                                       (getExceptionInfo hImage)
----------- Exceptions
mkNewExceptionInfo :: IO (Ptr ExceptionInfo)
mkNewExceptionInfo = do
  infoPtr <- malloc
  get_exception_info infoPtr
  return infoPtr
----------- Image info
mkNewImageInfo :: IO (Ptr HImageInfo)
mkNewImageInfo = clone_image_info nullPtr
----------- Both
mkNewUnloadedImage :: ImageNotLoaded
mkNewUnloadedImage = unsafePerformIO $ do
  e <- mkNewExceptionInfo
  i <- mkNewImageInfo
  return $ mkUnloadedImage i e
----------- Type conversion
-- meant to convert an integ>ral type to a C enum type
toCEnum :: (Enum a, Num b) => a -> b
toCEnum = fromIntegral.fromEnum
----------- dealing with pointers whose values may not be present
maybeToPtr :: Storable a => Maybe a -> Ptr a -> IO (Ptr a)
maybeToPtr Nothing _      = return nullPtr
maybeToPtr (Just stuff) p = poke p stuff >> return p
