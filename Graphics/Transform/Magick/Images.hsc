module Graphics.Transform.Magick.Images(initializeMagick, readImage, writeImage, pingImage,
              readInlineImage,
              getFilename,
              blobToImage,
              imageToBlob,
             -- transformations
              flipImage,
              flopImage,
              rotateImage,
              affineTransform,
              shearImage,
              chopImage,
              cropImage,
              flattenImage,
              mosaic,
              rollImage,
              shaveImage,
              -- resizing
              scaleImage,
              magnifyImage,
              minifyImage,
              sampleImage,
              thumbnailImage,
              resizeImage,
              -- enhancements
              contrastImage,
              equalizeImage,
              gammaImage,
              levelImage,
              levelImageChannel,
              modulateImage,
              negateImage,
              normalizeImage,
             -- constitution
              constituteImage,
              dispatchImage,
              --exportPixelImageArea,
              importPixelImageArea,
             -- composition
              compositeImage,
             -- image methods
              allocateImage,
              destroyImage,
              setImageColormap,
              newImageColormap,
              appendImages,
              averageImages,
              cycleColormapImage,
              destroyImage,
--              describeImage,
             -- Stuff what displays stuff
              animateImages) where

#include <magick/api.h>

import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS

import Graphics.Transform.Magick.Magick
import Graphics.Transform.Magick.Types
import Graphics.Transform.Magick.FFIHelpers
import Graphics.Transform.Magick.Errors
import Graphics.Transform.Magick.Util

import Data.Char
import Data.List
import System.Directory

-- The externally-visible Haskell API for GraphicsMagick.

-- API:
--------- Reading/writing
readImage        :: FilePath -> IO HImage
writeImage       :: FilePath -> HImage -> IO ()
pingImage        :: FilePath -> IO HImage
initializeMagick :: IO ()
--------- Transformations
flipImage, flopImage    :: HImage -> HImage
rotateImage             :: Double -> HImage -> HImage
affineTransform         :: AffineMatrix -> HImage -> HImage
shearImage              :: Double -> Double -> HImage -> HImage
chopImage, cropImage    :: Rectangle -> HImage -> HImage
flattenImage            :: [HImage] -> HImage
mosaic                  :: [(HImage, Rectangle)] -> HImage
rollImage               :: Int -> Int -> HImage -> HImage
shaveImage              :: Rectangle -> HImage -> HImage
--------- Resizing
scaleImage, sampleImage, thumbnailImage   :: Word -> Word -> HImage -> HImage
magnifyImage, minifyImage                 :: HImage -> HImage
resizeImage :: Int -> Int -> FilterTypes -> Double -> HImage -> HImage
--------- Enhancements
contrastImage                 :: Contrast -> HImage -> HImage
equalizeImage, normalizeImage :: HImage -> HImage
gammaImage                    :: PixelPacket Double -> HImage -> HImage
levelImage                    :: Level -> HImage -> HImage
levelImageChannel             :: ChannelType -> Level -> HImage -> HImage
modulateImage                 :: Modulation -> HImage -> HImage
negateImage                   :: Negation -> HImage -> HImage
--------- Constitution
-- This type says: if I can store blobs of type a as pixels of type b,
-- and b is a Storable thing, then I can constitute an image from a list
-- of blobs of type a. Gotta love Haskell!
constituteImage :: (StorablePixel a b) => PixMap -> [[a]] -> HImage
-- Not quite as nice, because we have to tell the GraphicsMagick library
-- the StorageType so that it knows what type of pixels to put into the
-- the array it's returning.
dispatchImage :: (StorablePixel a b) => PixMap -> StorageType -> Rectangle -> 
                   HImage -> [[a]]
{-
TODO
exportPixelImageArea :: (StorablePixel a b) => QuantumType2 -> Word -> 
     Maybe ExportPixelAreaOptions -> HImage -> [[a]]
-}
-- TODO: this requires that the pixels are unsigned chars. Is there a better way?
importPixelImageArea :: QuantumType2 -> Word -> [[Word8]] -> 
     Maybe ImportPixelAreaOptions -> HImage -> HImage
readInlineImage      :: String   -> HImage
------------- Composition
compositeImage :: CompositeOp -> Int -> Int -> HImage -> HImage -> HImage
------------- Image methods
-- returns a new image, initialized with default values
allocateImage      :: ImageNotLoaded -> HImage
setImageColormap   :: Word32 -> HImage -> HImage
newImageColormap   :: Word32 -> HImage 
appendImages       :: ImageOrder -> [HImage] -> HImage
averageImages      :: [HImage] -> HImage
cycleColormapImage :: Int -> HImage -> HImage 
destroyImage       :: HImage -> IO ()
-- TODO.
-- describeImage      :: Verbosity -> HImage -> String
------------- Stuff what displays stuff
animateImages      :: [HImage] -> IO ()
--------------------------------------------------------------
------------------- Reading/writing images -------------------
--------------------------------------------------------------

----------------- readImage -------------------
-- readImage: reads in an image from a file.

readImage = genericReadImage read_image

--------------- writeImage --------------------
-- writeImage: writes the given image to the given file path
-- TODO: has the side effect that it writes the filepath into the image filename
-- fields. is this the right thing?

writeImage fp hImage = withForeignPtr (getImage hImage) $ \img_ptr -> do
  -- hmm, side-effect the image info or make a copy of it?
  setFilename hImage fp 
  debug 2 $ "About to write image..."
  excInfo <- nonFinalizedExceptionInfo ((#ptr Image, exception) img_ptr)
  -- write_image signals an exception by returning 0
  withExceptions_ (withForeignPtr (getImageInfo hImage) (\ii -> 
                      (write_image ii img_ptr)))
                   "writeImage: error writing image"
                   (== 0) excInfo
  debug 2 $ "Wrote the image!"
  ex <- doesFileExist fp
  debug 3 $ fp ++ (if ex then " exists " else " doesn't exist")
  
------------- pingImage -----------------------

pingImage = genericReadImage ping_image

------------- composition ---------------------

compositeImage op x_offset y_offset canvas_image comp_image = sideEffectingOp
  (\ canvasIm -> withExceptions (
      withForeignPtr (getImage canvasIm) $ \canvasImPtr ->
      withForeignPtr (getImage comp_image) $ \comp_image_ptr ->
        composite_image canvasImPtr (toCEnum op) comp_image_ptr
                        (fromIntegral x_offset) (fromIntegral y_offset))
       "compositeImage: error compositing image" (== 0)
       (getExceptionInfo canvasIm)) canvas_image

------------- image methods -------------------
allocateImage imgNotLoaded = unsafePerformIO $ do
   imagePtr <- withForeignPtr (imageInfo imgNotLoaded) allocate_image
   if(imagePtr == nullPtr)
     then (signalException "allocateImage returned null")
     else return $ mkImage imagePtr imgNotLoaded

-- optionaly let user destroy image and free memory immediately
destroyImage :: HImage -> IO ()
destroyImage (HImage img (ImageNotLoaded info exc)) = do
  finalizeForeignPtr img
  finalizeForeignPtr info
  finalizeForeignPtr exc

setImageColormap clrs hImage = sideEffectingOp 
  (\ im -> applyImageFn1 im allocate_image_colormap (fromIntegral clrs))
  hImage

newImageColormap clrs = unsafePerformIO $ do
  let hImage = allocateImage mkNewUnloadedImage
  withExceptions_ (applyImageFn1 hImage allocate_image_colormap (fromIntegral clrs)) 
    "setImageColormap: error setting colormap" (== 0)
    (getExceptionInfo hImage)
  return hImage

-- should require list to be nonempty
appendImages order images@(img:_) = unsafePerformIO $ do
  linkImagesTogether images
  iPtr <- withExceptions (applyImageFn1' img append_images (toCEnum order))
            "appendImage: error appending"
            (== nullPtr) (getExceptionInfo img)
  return $ setImage img iPtr
appendImages _ [] = unsafePerformIO $ signalException "appendImages: empty list"

-- TODO:
-- should require a nonempty list
-- TODO:
-- hmm, appendImages and averageImages look a lot alike...
averageImages images@(img:_) = unsafePerformIO $ do
  linkImagesTogether images
  iPtr <- withExceptions (applyImageFn' img average_images id)
            "averageImages: error averaging" (== nullPtr) (getExceptionInfo img)
  return $ setImage img iPtr
averageImages []  = unsafePerformIO $ signalException "averageImages: empty list"

-- TODO: should really abstract the patterns of "returns boolean" and
-- "may return null pointer"
cycleColormapImage amount img = sideEffectingOp
  (\ im -> applyImageFn1 im cycle_colormap_image (fromIntegral amount))
  img

destroyImage img = destroy_image $ getImage img

{- 
TODO.
describeImage verbosity img = unsafePerformIO $ do
-- the API requires a file in which to dump the description -- grr
  tmpDir    <- getTemporaryDirectory
  (fp, hdl) <- openTempFile tmpDir "hsMagick.tmp"
  hClose hdl
  withCString (\ fileStr -> withCString (\ modeStr -> do
     filePtr <- fopen fileStr modeStr
     withExceptions_ (describe_image (getImage img) filePtr (toCEnum verbosity))
       "describeImage: error describing" (== 0) (getExceptionInfo img)
     fclose filePtr
     readFile fp))
-}
  
------------- Stuff what displays stuff
animateImages images@(img:_) = do
  linkImagesTogether images
  withExceptions_ (withForeignPtr (getImageInfo img) (\ii ->
                    (applyImageFn img (animate_images ii) id)))
     "animateImages: error animating" (== 0) (getExceptionInfo img)
animateImages [] = return ()
------------- genericReadImage - not exported
genericReadImage :: (Ptr HImageInfo -> Ptr ExceptionInfo -> IO (Ptr HImage_)) 
                    -> FilePath -> IO HImage
genericReadImage reader fp = 
   genericReadOp ((flip setFilename) fp) reader 
     "readImage: error reading image"

genericReadOp :: (ImageNotLoaded -> IO ()) -> 
   (Ptr HImageInfo -> Ptr ExceptionInfo -> IO (Ptr HImage_)) -> 
    String -> IO HImage
genericReadOp prepareImageInfo theAction errStr = do
   infoPtr <- mkNewExceptionInfo
   image_info <- mkNewImageInfo
   let theImage = mkUnloadedImage image_info infoPtr
   prepareImageInfo theImage
   iPtr <- withForeignPtr image_info $ \ii_ptr -> 
           withForeignPtr infoPtr    $ \exc_ptr ->
             withExceptions (theAction ii_ptr exc_ptr) 
                           errStr (== nullPtr) infoPtr
   return $ mkImage iPtr theImage

----------------------------------------------  
-----------------------------------------------
------------------ initializeMagick --------------
-- Initializes state in the Magick library, but I'm not sure where/when it needs to be called.
-- initialize_magick takes an argv pointer, but just passing null seems to work
initializeMagick = initialize_magick nullPtr

--------------------------------------------------------------
------------------- Transformations -------------------
--------------------------------------------------------------

----------------- Simple transformations
-- vertical flip.
flipImage    = doTransform flip_image
-- horizontal flip (flop).
flopImage    = doTransform flop_image
-- double size
magnifyImage = doTransform magnify_image
-- halve size
minifyImage  = doTransform minify_image
--------------------------------------------

-- rotates an image by an arbitrary number of degrees
rotateImage degrees hImage = doTransformIO
  (applyImageFn1' hImage rotate_image (realToFrac degrees))
  hImage

affineTransform affineMatrix hImage = unsafePerformIO $ do
  (matrixPtr::ForeignPtr AffineMatrix) <- mallocForeignPtr
  withForeignPtr matrixPtr $
    (\ matrixP -> do
          poke matrixP affineMatrix
          return $ doTransformIO
                    (applyImageFn1' hImage affine_transform matrixP)
                    hImage)

-- cuts the specified rectangle out of the image,
-- and squishes the remaining part to fill it
chopImage = rectOp chop_image
-- returns an image consisting of the specified
-- rectangle from the original image
cropImage = rectOp crop_image
-- returns an image consisting of the original image with the specified
-- rectangle shaved from it
shaveImage = rectOp shave_image

rectOp :: ((Ptr HImage_) -> Ptr Rectangle -> Ptr ExceptionInfo -> 
              IO (Ptr HImage_))
           -> Rectangle -> HImage -> HImage
rectOp fun rect im = unsafePerformIO $ withRectangle rect fun im

-- takes a list of images and returns a single image consisting of all of them 
-- overlaid over each other
-- TODO: require a nonempty list
flattenImage []     = unsafePerformIO $ 
                        signalException "flattenImage: list cannot be empty"
-- TODO: it's somewhat sketchy to do the side-effecting we do here
-- (mutating the next fields of the images). rethink that
flattenImage images@(img:_) = unsafePerformIO $ do
         debug 3 $ "Linking images..."
         linkImagesTogether images
         let res = doTransform flatten_images img
         debug 3 $ res `seq` "FlattenImage: done!"
         return res
        
mosaic [] = unsafePerformIO $ signalException $ "mosaic: list cannot be empty"
mosaic imagesAndRects@((img,_):_) = unsafePerformIO $ do
   let images = fst $ unzip imagesAndRects
   linkImagesTogether images
   mapM_ (uncurry setPage) imagesAndRects
   return $ doTransform mosaic_images img

rollImage xOffset yOffset hImage = doTransformIO_XY roll_image 
                                    hImage xOffset yOffset

scaleImage xFactor yFactor hImage = doTransformIO_XY scale_image
                           hImage xFactor yFactor

sampleImage xFactor yFactor hImage = doTransformIO_XY sample_image
                           hImage xFactor yFactor

thumbnailImage xFactor yFactor hImage = doTransformIO_XY thumbnail_image
                           hImage xFactor yFactor

shearImage xFactor yFactor hImage = doTransformIO_XY_real shear_image
                                     hImage xFactor yFactor

-- the stupid argument names are due to these names being already taken
-- as record fields.
resizeImage cols rws fltr blr hImage = 
   doTransformIO (applyImageFn' hImage resize_image $ \f -> f 
                    (fromIntegral cols) 
                     (fromIntegral rws) (toCEnum fltr) 
                     (realToFrac blr))
      hImage
------------ enhancements
-- TODO: the contrastImage call only increases or decreases by a
-- given increment. perhaps want to change our API to specify
-- an amount of contrast
contrastImage increaseOrDecrease hImage = sideEffectingOp 
  (\ im -> applyImageFn1 im contrast_image sharpen) hImage 
     where sharpen = case increaseOrDecrease of
                       IncreaseContrast -> 1
                       DecreaseContrast -> 0

equalizeImage = simpleOp equalize_image
normalizeImage = simpleOp normalize_image

gammaImage (PixelPacket { red=gRed, green=gGreen, blue=gBlue }) hImage = 
  sideEffectingOp (\ im -> applyImageFn im gamma_image $ withCString levelStr) 
       hImage
    where levelStr = commaSep [gRed, gGreen, gBlue]

levelImage (Level { black=lBlack, mid=lMid, white=lWhite }) hImage =
    sideEffectingOp (\ im -> 
      applyImageFn im level_image $ withCString levelStr)
         hImage
             where levelStr = commaSep [lBlack, lMid, lWhite]

levelImageChannel chanTy (Level { black=lBlack, mid=lMid, white=lWhite }) 
  hImage = sideEffectingOp (\ im -> 
            applyImageFn im level_image_channel $ \ f ->
    f (toCEnum chanTy) (realToFrac lBlack) 
      (realToFrac lMid) (realToFrac lWhite)) hImage

modulateImage (Modulation{ brightness=b, saturation=s, hue=h }) hImage =
    sideEffectingOp (\ im ->
      applyImageFn im modulate_image $ withCString modStr) hImage
     where modStr = commaSep [b, s, h]

negateImage whatToNegate hImage = 
    (sideEffectingOp (\ im -> applyImageFn1 im negate_image whatToDo) hImage)
       where whatToDo = case whatToNegate of
                          AllPixels -> 0
                          GrayscalePixels -> 1  
------------- Constitution
-- TODO: we should require pixels to be a non-empty list
-- This constructs an image from a list of scanlines.
-- A scanline is a list of pixels.
-- A pixel is anything that can be stored as one of the C types
-- that can be a pixel.
-- All of the scanlines should have the same length, but I don't
-- know how to enforce that.
-- TODO: a pixel is really a triple (R,G,B) or a quadruple (C,M,Y,K) or...
-- depending on the color space. as is, each scanline is just a flat list
-- now. but we could do it in a more strongly typed way.
constituteImage pixMap pixels = unsafePerformIO $ do
   eInfo <- mkNewExceptionInfo
   debug 3 $ "width = " ++ show wdth ++ " height = " ++ show hght ++ " sz = " ++ (show (pixelSize pixMap) ++ " len = " ++ show (length aScanline))
   iPtr <- withExceptions (withArray (map marshalPixel (concat pixels)) (\ pixelArray ->
      withCString (show pixMap) $  
        (\ mapStr -> withForeignPtr eInfo $
                       constitute_image 
                         wdth
                         -- this is kind of weak... the pixmap
                         -- says how many numbers represent each pixel. seems bad.
                         -- we should have a better type system for this.
                         hght
                         mapStr 
                         (toCEnum (storageType (head aScanline)))
                         pixelArray))) "constituteImage: error" (== nullPtr) eInfo
   iInfo <- mkNewImageInfo
   return $ mkImage iPtr (mkUnloadedImage iInfo eInfo) 
   -- TODO: freeing pixelArray and other memory?
        where aScanline = head pixels
              wdth      = (fromIntegral $ (length aScanline) `div` (pixelSize pixMap))
              hght      = fromIntegral $ length pixels 
-- TODO: could we add a field in HImage for the pixMap and avoid the need to pass that?
-- TODO: a fun QuickCheck property to add would be:
--    forall pm blobs i . blobs == dispatchImage (pm all (constituteImage pm blobs i))
--      where all is a rectangle representing the entire image                             
dispatchImage pixMap storType (Rectangle{ width=cols, height=rws, 
                                 x=x_offset, y=y_offset}) hImage = 
   unsafePerformIO $ 
     (allocaArray len (\ pixelArray ->
       withCString (show pixMap) $
         (\ mapStr -> do
             withExceptions_ (applyImageFn' hImage dispatch_image $ \f -> 
               f (fromIntegral x_offset) (fromIntegral y_offset) 
                  (fromIntegral cols) (fromIntegral rws) mapStr 
                  (toCEnum storType) pixelArray) 
                "dispatchImage: error" (== 0) 
                (getExceptionInfo hImage)
             pixelList <- peekArray (fromIntegral len) pixelArray
             let blobs = map unmarshalPixel pixelList
             return $ groups cols blobs))) 
            where len = (fromIntegral cols*fromIntegral rws*pixelSize pixMap)

{-
TODO: Seems to have disappeared from library
-- note: the exportInfo structure that export_image_pixel_area initializes
-- only contains the number of bytes exported, which we use to determine
-- the length of the list exportPixelImageArea returns -- so we don't need
-- to return it as well.
-- TODO: quantumSize shouldn't be necessary
-- TODO: have a test that uses a non-null options structure,
-- and use exportPixelAreaOptionsInit
exportPixelImageArea quantumType quantumSize options hImage = 
   unsafePerformIO $ 
     (allocaArray (fromIntegral (quantumSize * imagePixels)) 
       (\ pixelArray -> (alloca (\ exportInfo -> (alloca (\ optionsPtr -> do
          optsPtr <- maybeToPtr options optionsPtr
          withExceptions_ (export_image_pixel_area (getImage hImage) (toCEnum quantumType) (fromIntegral quantumSize) pixelArray optsPtr exportInfo) "exportPixelImageArea: error exporting" (== 0) (getExceptionInfo hImage)
          bytes_exported <- (#peek ExportPixelAreaInfo, bytes_exported) exportInfo
          pixelList <- peekArray bytes_exported pixelArray
          let blobs = map unmarshalPixel pixelList
          return $ groups cols blobs))))))
             where rws  = hImageRows hImage
                   cols = hImageColumns hImage
                   imagePixels = rws*cols
-}

-- this may very well be wrong
importPixelImageArea quantumType quantumSize pixels options hImage = 
   sideEffectingOp (\ theImage -> 
     (withArray (map (fromIntegral.ord) (unlines (map (map (chr.fromIntegral)) pixels)))
        (\ pixelArray -> (alloca (\ importInfo -> (alloca (\ optionsPtr -> do
           optsPtr <- maybeToPtr options optionsPtr
           -- this side-effects the image, so we need to make a copy
           res <- (applyImageFn theImage import_image_pixel_area $ \f -> 
             f (toCEnum quantumType) (fromIntegral quantumSize) pixelArray optsPtr
             importInfo) 
           bytes_imported <- (#peek ImportPixelAreaInfo, bytes_imported) importInfo
           assertM (bytes_imported == length pixels) 
                ("importPixelImageArea: internal error, not all pixels were imported: only " ++ show bytes_imported ++ " bytes were imported")
           return res))))))) hImage

readInlineImage base64content = unsafePerformIO $ do
  debug 47 $ "cleanedUpString = " ++ cleanedUpString
  genericReadOp (const (return ())) 
   (\ image_info exception_info ->
       (withCString cleanedUpString (\ content_str ->
           read_inline_image image_info content_str exception_info)))
   "readInlineImage: error reading inline content"
      where cleanedUpString = insertComma (deleteNewlines 
                   (deleteEqualsSignLine base64content))
            -- this ensures we can read data from uuencode -m without
            -- munging it somewhere else. I'm not sure whether the final
            -- version of the library should do this.
            deleteEqualsSignLine s | last (lines s) == "====" = 
              unlines (butLast (lines s))
            deleteEqualsSignLine s = s
            deleteNewlines = filter (/= '\n')
            insertComma s | ',' `elem` s = s
            insertComma s | null (", " `intersect` (nub s)) = (',':s)
            insertComma s = 
               case (lines s) of
                 (firstLine:secondLine:restLines) -> 
                    unlines (firstLine:((',':secondLine):restLines))
                 _ -> s

blobToImage :: BS.ByteString -> HImage
blobToImage bs = unsafePerformIO $ do
    genericReadOp (const (return ()))
      (\image_info exception_info ->
        BS.unsafeUseAsCStringLen bs (\(ptr, len) ->
          blob_to_image image_info (castPtr ptr) (fromIntegral len) 
                        exception_info))
      "blobToImage: error loading image from blob"

imageToBlob :: HImage -> BS.ByteString
imageToBlob img = unsafePerformIO $ 
    withTmpImageInfo $ \imgInfo ->
    alloca $ \sizePtr -> do
        excInfo <- mkNewExceptionInfo
        dat <- withExceptions (applyImageFn1' img (image_to_blob imgInfo) sizePtr)
                               "imageToBlob: unable to encode image"
                               (==nullPtr)
                               excInfo
        len <- fromIntegral `fmap` peek sizePtr
        BS.unsafePackCStringFinalizer (castPtr dat) len (free dat)

--------- helpers (private) ------------
simpleOp :: (Ptr HImage_ -> IO CUInt) -> HImage -> HImage
simpleOp op im = sideEffectingOp 
                  (\hImage -> 
                    withForeignPtr (getImage hImage) $ \ii_ptr -> 
                      op ii_ptr) im

withRectangle :: Rectangle -> 
 (Ptr HImage_ -> Ptr Rectangle -> Ptr ExceptionInfo -> IO (Ptr HImage_)) ->
 HImage -> IO HImage
withRectangle rect transform hImage = do
  -- Does this actually free the memory? 
  -- Steffen: Yes, this will free the memory
  (rectPtr::ForeignPtr Rectangle) <- mallocForeignPtr
  -- This was causing a segfault so it\'s temporarily commented out.
  -- TODO: Worry about memory freeing.
  -- Steffen: this is not needed, mallocForeignPtr already installs a
  --          correct finalizer
  --addForeignPtrFinalizer p_free rectPtr
  withForeignPtr rectPtr $ 
    (\ rectP -> do 
       poke rectP rect
       return $ doTransformIO
                  (applyImageFn1' hImage transform rectP)
                  hImage)
