module Types where

import Foreign
import Foreign.C.Types
import Foreign.C.String

-- types used for representing data from GraphicsMagick

-- Types for external use. These are part of the Haskell
-- GraphicsMagick interface.

-- The idea here is that when we first create an image we
-- have an exception info and an image info.
-- Later after it's loaded, we have an image as well.
-- getFilename and setFilename are class methods that work on
-- either one, because if we have an image that's not loaded
-- yet, we want to be able to set the filename (for loading it later),
-- and if we have an image that *is* loaded, we want to be able to set
-- the filename in both the image *and* the info.

-- TODO: don't export the selectors for this.
data HImage = HImage {image::Ptr HImage_,
                      otherInfo::ImageNotLoaded}
data ImageNotLoaded = ImageNotLoaded { imageInfo::Ptr HImageInfo, 
                                       exceptionInfo::Ptr ExceptionInfo }

-- A rectangle is represented as a width, height, horizontal offset, and
-- vertical offset
data Rectangle = Rectangle { width  :: Word,
                             height :: Word,
                             x      :: Int,
                             y      :: Int }
  deriving Show

data AffineMatrix = AffineMatrix { sx::Double,
                                   rx::Double,
                                   ry::Double,
                                   sy::Double,
                                   tx::Double,
                                   ty::Double }

data PixelPacket a = PixelPacket { red::a,
                                   green::a,
                                   blue::a,
                                   opacity::a}
data Level = Level { black::Double,
                     mid::Double,
                     white::Double }

data Modulation = Modulation { brightness::Double,
                               saturation::Double,
                               hue::Double }

data Negation = AllPixels | GrayscalePixels

-- TODO: quantum depth (number of bits in a pixel)
-- is determined at GraphicsMagick compile time. need
-- to reflect that (I guess in a config file for this
-- library...)
type PixelPacketByte = PixelPacket Word8

data ChannelType = 
  UndefinedChannel|
  RedChannel|
  CyanChannel|
  GreenChannel|
  MagentaChannel|
  BlueChannel|
  YellowChannel|
  OpacityChannel|
  BlackChannel|
  MatteChannel
   deriving Enum

getImage :: HImage -> Ptr HImage_
getImageInfo :: HImage -> Ptr HImageInfo
getExceptionInfo :: HImage -> Ptr ExceptionInfo
setImage :: HImage -> Ptr HImage_ -> HImage

getImage = image
getImageInfo = imageInfo.otherInfo
getExceptionInfo = exceptionInfo.otherInfo

setImage hIm imPtr = hIm{ image = imPtr }

mkUnloadedImage :: Ptr HImageInfo -> Ptr ExceptionInfo -> ImageNotLoaded
mkUnloadedImage iInfo exInfo = 
   ImageNotLoaded{ imageInfo = iInfo, exceptionInfo = exInfo }

mkImage :: Ptr HImage_ -> ImageNotLoaded -> HImage
mkImage p info = HImage { image=p, otherInfo=info }

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
    deriving Enum

data CompositeOp = Undefined
   | Over
   | In
   | Out
   | Atop
   | Xor
   | Plus
   | Minus
   | Add
   | Subtract
   | Difference
   | Multiply
   | Bumpmap
   | Copy
   | CopyRed
   | CopyGreen
   | CopyBlue
   | CopyOpacity
   | Clear
   | Dissolve
   | Displace
   | Modulate
   | Threshold
   | No
   | Darken
   | Lighten
   | Hue
   | Saturate
   | Colorize
   | Luminize
   | Screen
   | Overlay
   | CopyCyan
   | CopyMagenta
   | CopyYellow
   | CopyBlack
  deriving Enum

data Contrast = IncreaseContrast | DecreaseContrast

data ImageCharacteristics = ImageC {
      cmyk::Bool,
      grayscale::Bool,
      mONOCHROME::Bool,
      opaque::Bool,
      palette::Bool
}

-- TODO:
-- the Right Thing to do would be
-- to use type classes rather than all these underscores
data ImageStatistics = ImageS {
      red_::ImageChannelStatistics,
      green_::ImageChannelStatistics,
      blue_::ImageChannelStatistics,
      opacity_::ImageChannelStatistics
}

data ImageChannelStatistics = ImageCS {
      maximum::Double,
      minimum::Double,
      mean::Double,
      standard_deviation::Double,
      variance::Double
}

data SegmentInfo = SegmentInfo {
      x1::Double, y1::Double, x2::Double, y2::Double
}

data ImageOrder = LeftToRight | TopToBottom
  deriving Enum
----------- Storage (used by constituteImage)
data StorageType = CharPixel | ShortPixel | IntegerPixel | LongPixel 
                 | FloatPixel | DoublePixel
   deriving Enum

-- OMG functional dependencies squee!!
class Storable b => StorablePixel a b | a -> b where
   storageType    :: a -> StorageType
   marshalPixel   :: a -> b
   unmarshalPixel :: b -> a

instance StorablePixel Word8 CUChar where
    storageType _ = CharPixel
    marshalPixel  = fromIntegral
    unmarshalPixel  = fromIntegral
instance StorablePixel Word16 CUShort where
    storageType _ = ShortPixel
    marshalPixel  = fromIntegral
    unmarshalPixel  = fromIntegral
instance StorablePixel Word32 CUInt where
    storageType _ = IntegerPixel
    marshalPixel  = fromIntegral
    unmarshalPixel  = fromIntegral
instance StorablePixel Word64 CULong where
    storageType _ = LongPixel
    marshalPixel  = fromIntegral
    unmarshalPixel  = fromIntegral
instance StorablePixel Float CFloat where
    storageType _ = FloatPixel
    marshalPixel  = realToFrac
    unmarshalPixel  = realToFrac
instance StorablePixel Double CDouble where
    storageType _ = DoublePixel
    marshalPixel  = realToFrac
    unmarshalPixel  = realToFrac

-- TODO:
-- should have better constraints. ex. no repeated
-- quantums, list can't be empty. I don't think all
-- combinations are legal.
newtype PixMap = PixMap [QuantumType]

instance Show PixMap where
  show (PixMap things) = concatMap show things

pixelSize :: PixMap -> Int
pixelSize (PixMap quantums) = length quantums

data QuantumType = R|G|B|A|O|T|C|Y|M|K|I|P
  deriving Show

-- TODO: better name
data QuantumType2 = UndefinedQuantum | IndexQuantum | GrayQuantum
  | IndexAlphaQuantum | GrayAlphaQuantum | RedQuantum | CyanQuantum | GreenQuantum
  | YellowQuantum | BlueQuantum | MagentaQuantum | AlphaQuantum | BlackQuantum
  | RGBQuantum | RGBAQuantum | CMYKQuantum | CMYKAQuantum | CIEYQuantum
  | CIEXYZQuantum
   deriving Enum

-- All types below should only be used internally to the library.

-- we append underscores to names for fields that appear 
-- in multiple different record types, but there's ugly.
-- must be a better way (type classes?)
data ExceptionInfo = ExceptionInfo {
      severity     :: ExceptionType,
      reason       :: CString,
      description  :: CString,
      error_number :: CInt,
      mODULE       :: CString,
      function     :: CString,
      line         :: CULong,
      signature__  :: CULong
}

------ TODO: stubs
type ExportPixelAreaOptions = Word32
type ExportPixelAreaInfo = Word32
type ImportPixelAreaOptions = Word32
type ImportPixelAreaInfo = Word32

-------------
type ImagePtr = Ptr Image
type Image = Word32
data CharArray = CharArray 
type CompressionType = Word32
type InterlaceType = Word32
type EndianType = Word32
type ResolutionType = Word32
type ColorspaceType = Word32
type ImageType = Word32
type StreamHandler = Word32
type PreviewType = Word32
type ClassType = Word32
type ChromaticityInfo = Word32
type OrientationType = Word32
type RenderingIntent = Word32
type GravityType = Word32
type DisposeType = Word32
type ErrorInfo = Word32
type TimerInfo = Word32
type CacheInfoPtr = Word32
type ImageAttributePtr = Word32
type Ascii85InfoPtr = Word32
type BlobInfoPtr = Word32
type SemaphoreInfoPtr = Word32
type ExceptionType = CUInt -- actually an enum type

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
 pen             :: PixelPacketByte,
 background_color :: PixelPacketByte,
 border_color     :: PixelPacketByte,
 matte_color      :: PixelPacketByte,
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
 magick          :: String,
 filename        :: String,
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
  background_color_ :: PixelPacketByte,
  border_color_ :: PixelPacketByte,
  matte_color_  :: PixelPacketByte,
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
  page_        :: Rectangle,
  tile_info    :: Rectangle,
  blur         :: CDouble,
  fuzz_        :: CDouble,
  fILTER       :: FilterTypes,
  interlace_   :: InterlaceType,
  endian_      :: EndianType,
  gravity      :: GravityType,
  compose      :: CompositeOp,
  dispose      :: DisposeType,
  scene        :: CULong,
  delay        :: CULong,
  iterations   :: CULong,
  total_colors :: CULong,
  start_loop   :: CLong,
  eRROR        :: ErrorInfo,
  timer        :: TimerInfo,
  client_data_ :: CIntPtr,
  filename_    :: String,
  magick_filename :: String,
  magick_      :: String,
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
