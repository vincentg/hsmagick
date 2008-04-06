module Types where

import Foreign
import Foreign.C.Types
import Foreign.C.String

-- types used for representing data from GraphicsMagick

-- Types for external use. These are part of the Haskell
-- GraphicsMagick interface.

-- image is a Maybe Ptr because we create the imageInfo first,
-- then use it to create an Image. It's Maybe so we can call setFilename
-- in between (which wants an HImage) and we don't want it to find a null
-- pointer and try to set its filename.
data HImage = HImage {image::Maybe (Ptr HImage_), 
                      imageInfo::Ptr HImageInfo,
                      exceptionInfo::Ptr ExceptionInfo}

data FilterTypes = 
  UndefinedFilter
  | PointFilter
  | BoxFilter
  | TriangleFilter
  | HermiteFilter
  | HanningFilter
  | HammingFilter
  | BlackmanFilter
  | GaussianFilter
  | QuadraticFilter
  | CubicFilter
  | CatromFilter
  | MitchellFilter
  | LacrosFilter
  | BesselFilter
  | SincFilter

-- All types below should only be used internally to the library.

type ImagePtr = Ptr Image
type Image = Word32
data ExceptionInfo = ExceptionInfo
data CharArray = CharArray 
type CompressionType = Word32
type InterlaceType = Word32
type EndianType = Word32
type ResolutionType = Word32
type PixelPacket = Word32
type ColorspaceType = Word32
type ImageType = Word32
type StreamHandler = Word32
type PreviewType = Word32
type ClassType = Word32
type ChromaticityInfo = Word32
type OrientationType = Word32
type RenderingIntent = Word32
type RectangleInfo = Word32
type GravityType = Word32
type CompositeOperator = Word32
type DisposeType = Word32
type ErrorInfo = Word32
type TimerInfo = Word32
type CacheInfoPtr = Word32
type ImageAttributePtr = Word32
type Ascii85InfoPtr = Word32
type BlobInfoPtr = Word32
type SemaphoreInfoPtr = Word32


-- Correspondences:
{- HImage_ <=> Image
   HImageInfo <=> ImageInfo 
-}




-- This is from magick/image.h in GraphicsMagick 1.2. It may be subject to change!

-- default values?
-- some of these need fixing. ex., verbose should be
-- a boolean, but the Storable instance should convert it appropriately.
data HImageInfo = HImageInfo {
 compression :: CompressionType,
 temporary   :: CUInt,
 adjoin      :: CUInt,
 antialias   :: CUInt,
 subimage    :: CULong,
 subrange    :: CULong,
 depth       :: CULong,
 size        :: CString,
 tile        :: CString,
 page        :: CString,
 interlace   :: InterlaceType,
 endian      :: EndianType,
 units       :: ResolutionType,
 quality     :: CULong,
 sampling_factor :: CString,
 server_name     :: CString,
 font            :: CString,
 texture         :: CString,
 density         :: CString,
 pointsize       :: CDouble,
 fuzz            :: CDouble,
 pen             :: PixelPacket,
 background_color :: PixelPacket,
 border_color     :: PixelPacket,
 matte_color      :: PixelPacket,
 dither          :: CUInt,
 monochrome      :: CUInt,
 progress        :: CUInt,
 colorspace      :: ColorspaceType,
 tYPE            :: ImageType,
 group           :: CLong,
 verbose         :: CUInt,
 view            :: CString,
 authenticate    :: CString,
 client_data     :: CString,
 stream          :: StreamHandler,
 file            :: Ptr CFile,
-- these two are actually represented as arrays
 magick          :: CharArray,
 filename        :: CharArray,
-- private from here on out
 cache           :: CString,
 definitions     :: CString,
 attributes      :: Ptr Image,
 ping            :: CUInt,
 preview_type    :: PreviewType,
 affirm          :: CUInt,
 blob            :: CString,
 lENGTH          :: CSize,
 unique          :: CString,
 zero            :: CString,
 signature       :: CULong   
}

-- Could we eliminate the duplicated fields and add code to copy them
-- back and forth between the Image and the ImageInfo to the Storable
-- instances?
data HImage_ = HImage_ {
  storage_class :: ClassType,
  colorspace_   :: ColorspaceType,
  compression_ :: CompressionType,
  dither_      :: CUInt,
  matte       :: CUInt,
  columns     :: CULong,
  rows        :: CULong,
  colors      :: CUInt,
  depth_       :: CUInt,
  colormap    :: CIntPtr,
  background_color_ :: PixelPacket,
  border_color_ :: PixelPacket,
  matte_color_  :: PixelPacket,
  gamma       :: CDouble,
  chromaticity :: ChromaticityInfo,
  orientation  :: OrientationType,
  rendering_intent :: RenderingIntent,
  units_       :: ResolutionType,
  montage      :: CString,
  directory    :: CString,
  geometry     :: CString,
  offset       :: CLong,
  x_resolution :: CDouble,
  y_resolution :: CDouble,
  page_        :: RectangleInfo,
  tile_info    :: RectangleInfo,
  blur         :: CDouble,
  fuzz_        :: CDouble,
  fILTER       :: FilterTypes,
  interlace_   :: InterlaceType,
  endian_      :: EndianType,
  gravity      :: GravityType,
  compose      :: CompositeOperator,
  dispose      :: DisposeType,
  scene        :: CULong,
  delay        :: CULong,
  iterations   :: CULong,
  total_colors :: CULong,
  start_loop   :: CLong,
  eRROR        :: ErrorInfo,
  timer        :: TimerInfo,
  client_data_ :: CIntPtr,
  filename_    :: CharArray,
  magick_filename :: CharArray,
  magick_      :: CharArray,
  magick_rows  :: CULong,
  exception    :: ExceptionInfo,
  previous     :: CIntPtr,
  next         :: CIntPtr,
  -- private from here on
  profiles     :: CIntPtr,
  is_monochrome :: CUInt,
  is_grayscale :: CUInt,
  taint        :: CUInt,
  clip_mask    :: CIntPtr,
  cache_       :: CacheInfoPtr,
  attributes_  :: ImageAttributePtr,
  ascii85      :: Ascii85InfoPtr,
  blob_        :: BlobInfoPtr,
  reference_count :: CLong,
  semaphore    :: SemaphoreInfoPtr,
  logging      :: CUInt,
  list         :: CIntPtr,
  signature_   :: CULong
}
