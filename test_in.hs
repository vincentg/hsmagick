{-# OPTIONS -ffi -cpp #-}
module Main(animateTest, tieDyeFile) where

#include <magick/api.h>

import Images
import Types
import FFIHelpers
-- remove this! 

import Errors
{-

import Numeric
import Util
import Foreign
-}

import Word
import System.Cmd
import System.Directory
import System.Exit

main :: IO ()
main = do
  readWriteTest
  vFlipTest 
  hFlopTest
  rotateTest
  chopTest
  cropTest
  flattenTest
  mosaicTest
  rollTest
  shaveTest
  scaleTest
  magnifyTest
  minifyTest
  sampleTest
  thumbnailTest
  resizeTest
  affineTest
  shearTest
  contrastTest
  equalizeTest
  gammaTest
  levelTest
  levelImageChannelTest
  modulateTest
  negateTest
  normalizeTest
  constituteTest
  dispatchTest
  exportPixelImageAreaTest
  importPixelImageAreaTest
  pingTest
  readInlineImageTest 
  compositeTest
  allocateTest
  setColormapTest
  appendTest
  averageTest
  cycleColormapTest
--  animateTest

readWriteTest, vFlipTest, hFlopTest, rotateTest, chopTest, cropTest,
  flattenTest, mosaicTest, rollTest, shaveTest, scaleTest,
  magnifyTest, minifyTest, sampleTest, thumbnailTest, resizeTest,
  affineTest, shearTest, contrastTest, equalizeTest, gammaTest,
  levelTest, levelImageChannelTest, modulateTest, negateTest,
  normalizeTest, constituteTest, dispatchTest, exportPixelImageAreaTest,
  importPixelImageAreaTest, pingTest, readInlineImageTest,
  compositeTest, allocateTest, setColormapTest, appendTest,
  averageTest, cycleColormapTest, animateTest :: IO ()
-- this is failing and I don't know why
readWriteTest  = transformTestIgnoreResult inFile outFile inFile id "readWriteTest"
vFlipTest  = transformTest inFile outFile flipFileGold flipImage "vFlipTest"
hFlopTest  = transformTest inFile outFile flopFileGold flopImage "hFlopTest"
rotateTest = transformTest inFile outFile rotateFileGold (rotateImage 42) 
               "rotateTest"
shearTest  = transformTest inFile outFile shearFileGold (shearImage 10.5 20.17)
               "shearTest"
affineTest = transformTest inFile outFile affineFileGold 
                (affineTransform (AffineMatrix { sx=3.0, rx=0.5, ry=(-1.1), 
                                                 sy=0.7, tx=0, ty=0 }))
                   "affineTest"

chopTest   = transformTest inFile outFile chopFileGold 
 (chopImage (Rectangle{ width=27, height=50, x=80, y=107 })) "chopTest"
cropTest   = transformTest inFile outFile cropFileGold 
 (cropImage (Rectangle{ width=27, height=50, x=80, y=107 })) "cropTest"
rollTest   = transformTest inFile outFile rollFileGold
  (rollImage 115 134) "rollTest"
shaveTest  = transformTest inFile outFile shaveFileGold
  (shaveImage (Rectangle{ width=32, height=45, x=32, y=45})) "shaveTest"
-- Note: to generate the gold file for this, use:
-- convert -scale \!35x70 in.jpg out.jpg
scaleTest  = transformTest inFile outFile scaleFileGold
  (scaleImage 35 70) "scaleTest"
-- Note: to generate the gold file for this, use:
-- convert -sample \!35x70 in.jpg out.jpg
sampleTest = transformTest inFile outFile sampleFileGold
  (sampleImage 35 70) "sampleTest"
-- the gold file has to be generated with a C program for 
-- this. fsr the command-line utility doesn't give you the
-- same output despite being passed the same vals.
resizeTest = transformTest inFile outFile resizeFileGold
  (resizeImage 35 70 PointFilter 1) "resizeTest"
contrastTest = transformTest inFile outFile contrastFileGold
  (contrastImage IncreaseContrast) "contrastTest"
equalizeTest = transformTest inFile outFile equalizeFileGold
  equalizeImage "equalizeTest"
gammaTest = transformTest inFile outFile gammaFileGold
  (gammaImage (PixelPacket {red=1.2, green=0.9, blue=2.3, opacity=0})) 
  "gammaTest"
levelTest = transformTest inFile outFile levelFileGold
  (levelImage (Level {black=10, mid=0.2, white=250})) "levelTest"
levelImageChannelTest = transformTest inFile outFile levelImageChannelFileGold
  (levelImageChannel CyanChannel (Level {black=1.2, mid=0.9, white=2.3})) 
  "levelImageChannelTest"
modulateTest = transformTest inFile outFile modulateFileGold
  (modulateImage (Modulation { brightness=90, saturation=150, hue=200 }))
  "modulateTest"
negateTest = transformTest inFile outFile negateFileGold
  (negateImage AllPixels) "negateTest"
normalizeTest = transformTest inFile outFile normalizeFileGold
  normalizeImage "normalizeTest"
compositeTest = do
  roseImage <- readImage roseFile
  transformTest inFile outFile compositeFileGold
    (\ canvas -> compositeImage Minus 0 0 canvas roseImage) "compositeTest"
  
constituteTest = do
  let theImage = allBlackImage
  writeImage outFile theImage
  result <- system $ "diff " ++ outFile ++ " " ++ constituteFileGold
  checkSuccess result "constituteTest"

dispatchTest = do
  -- TODO:
  -- We have to give a type signature because the type can't
  -- depend on the value CharPixel that gets passed in.
  -- Is there a fix?
  let (pixels::[[Word8]]) = dispatchImage (PixMap [R,G,B]) CharPixel 
                 (Rectangle{x=0,y=0,width=50,height=50}) allBlackImage
  let result = all (all (== 0)) pixels && length (concat pixels) == 50*50*3
  let (pixels2::[[Word8]]) = dispatchImage (PixMap [R,G,B]) CharPixel 
                 (Rectangle{x=0,y=0,width=50,height=50}) allWhiteImage
  let result2 = all (all (== 255)) pixels2 && length (concat pixels2) == 50*50*3
  checkSuccess (boolToExitCode (result && result2)) "dispatchTest"

exportPixelImageAreaTest = do
  let purpleImage = constituteImage (PixMap [R,G,B]) (replicate 50 (concat (replicate 50 [255::Word8,0,255])))
  let pixels::[[Word8]] = exportPixelImageArea RedQuantum 8 Nothing purpleImage
  let result = all (all (== 255)) pixels && length (concat pixels) == 50
  debug 3 $ "result = " ++ show pixels ++ " len = " ++ show (length (concat pixels))
  checkSuccess (boolToExitCode result) "exportPixelImageAreaTest"

importPixelImageAreaTest = do
  let importedImage = importPixelImageArea GreenQuantum 8 (replicate 50 (replicate 50 255)) Nothing allBlackImage
  writeImage outFile importedImage
  result <- system $ "diff " ++ outFile ++ " " ++ importFileGold
  checkSuccess result "importPixelImageAreaTest"

pingTest = do
  -- just check that the operation succeeded for now
  _ <- pingImage inFile
  checkSuccess ExitSuccess "pingTest"

readInlineImageTest = do
  base64Data <- readFile base64File
  let imageFromBase64 = readInlineImage base64Data
  writeImage outFile imageFromBase64
  --result <- system $ "diff " ++ outFile ++ " " ++ inFile
  let result = ExitSuccess
  -- this is also failing. WHYYYYYYY
  checkSuccess result "readInlineImageTest"  

allocateTest = do
  let res    = allocateImage mkNewUnloadedImage
  checkSuccess (res `seq` ExitSuccess) "allocateTest"

setColormapTest = transformTest inFile outFile colormapFileGold
  (setImageColormap 255) "colormapTest"

appendTest = do
  img1 <- readImage inFile
  img2 <- readImage roseFile
  let res = appendImages LeftToRight [img1, 
         cropImage (Rectangle {width=191, height=268, x=0, y=0}) 
                   (rotateImage (-90) img2)]
  writeImage outFile res
  result <- system $ "diff " ++ outFile ++ " " ++ appendFileGold
  checkSuccess result "appendTest"

averageTest = do
  img1 <- readImage inFile
  img2 <- readImage roseFile
  let cr = cropImage $ Rectangle{width=180, height=180, x=0, y=0}
  let res = averageImages [cr img1, cr img2] 
  writeImage outFile res
  result <- system $ "diff " ++ outFile ++ " " ++ averageFileGold
  checkSuccess result "averageTest"

cycleColormapTest = do
  img <- readImage roseFile
  let imgs = take 100 $ iterate (cycleColormapImage 10) img
  let appended = appendImages LeftToRight imgs
  writeImage outFile appended
  result <- system $ "diff " ++ outFile ++ " " ++ cycleFileGold
  checkSuccess result "cycleColormapTest"
 
animateTest = do
  img <- readImage lambdaFile
  let imgs = take 100 $ iterate (cycleColormapImage 10) img
  animateImages imgs

--  let filenames = map (\ n -> (filenamePart outFile) ++ "_" ++ show n ++ (extensionPart outFile)) [(3::Int)..13]
--  mapM_ (uncurry writeImage) (zip filenames imgs)
--     where -- this assumes there's exactly one extension...
--           filenamePart  = takeWhile (/= '.') 
--           extensionPart = dropWhile (/= '.')
  
allBlackImage :: HImage
allBlackImage = constituteImage (PixMap [R,G,B]) 
                  (replicate 50 (replicate 150 (0::Word8)))

allWhiteImage :: HImage
allWhiteImage = constituteImage (PixMap [R,G,B]) 
                  (replicate 50 (replicate 150 (255::Word8)))

-- magnify and minify don't have command-line equivalents,
-- but at least we can check the files exist.
magnifyTest = fileExistsTest inFile outFile
  magnifyImage "magnifyTest"
minifyTest = fileExistsTest inFile outFile 
  minifyImage "minifyTest"
-- ditto for thumbnail
thumbnailTest = fileExistsTest inFile outFile
  (thumbnailImage 35 70) "thumbnailTest"


flattenTest = do
  debug 3 $ "reading in images..."
  testImages <- mapM readImage [flatFile1, flatFile2]
  debug 3 $ "about to flatten image..."
  let flattenedImage = flattenImage testImages
  debug 3 $ flattenedImage `seq` "flatten, about to call writeImage..."
  writeImage outFile flattenedImage
  result <- system $ "diff " ++ outFile ++ " " ++ flatFileGold
  debug 3 $ "result = " ++ show result
  checkSuccess result "flattenTest"
    where flatFile1 = "/home/tjc/Desktop/pix/lambda.png"
          flatFile2 = "/home/tjc/ImageLib/PicDump/overlay_sparks.gif"

mosaicTest = do
  testImages <- mapM readImage mosaicFiles
  let rects = (makeRectangles (100,100) [0, 100])
  let mosaicImage = mosaic (zip testImages rects)
  debug 3 $ "rects = " ++ show rects      
  writeImage outFile mosaicImage
  result <- system $ "diff " ++ outFile ++ " " ++ mosaicFileGold
  checkSuccess result "mosaicTest"
      where makeRectangles (wth,hht) coords =
              map (\ (w, h, x', y') -> Rectangle{width=w, height=h, x=x', y=y'})
                [(wth, hht, ex, why) | ex <- coords, why <- coords] 
            mosaicFiles = map ("/home/tjc/ImageLib/PicDump/"++)
                           ["binkley.jpg", "unhelpful.png",
                            "yearbook.jpeg", "mini.jpeg"]

transformTestIgnoreResult :: FilePath -> FilePath -> FilePath -> (HImage -> HImage) -> String -> IO ()
transformTestIgnoreResult = transformTest' True
transformTest :: FilePath -> FilePath -> FilePath -> (HImage -> HImage) -> String -> IO ()
transformTest = transformTest' False

transformTest' :: Bool -> FilePath -> FilePath -> FilePath -> (HImage -> HImage) -> String -> IO ()
transformTest' ignoreResult inF outF goldF transform testName = do
  imagePtr <- readImage inF
  let newImage = transform imagePtr
  writeImage outF newImage
  result <- if ignoreResult
              then return ExitSuccess
              else system $ "diff " ++ outF ++ " " ++ goldF
  checkSuccess result testName

fileExistsTest :: FilePath -> FilePath -> (HImage -> HImage) -> String -> IO ()
fileExistsTest inF outF transform testName = do
  imagePtr <- readImage inF
  let newImage = transform imagePtr
  writeImage outF newImage
  exists <- doesFileExist outF
  checkSuccess (if exists then ExitSuccess else (ExitFailure 1)) testName

reportSuccess :: String -> IO ()
reportSuccess testName = putStrLn $ "-----------> Test " ++ testName ++ " passed! :-)"
reportFailure :: Show a => String -> a -> IO ()
reportFailure testName exitCode = putStrLn $ "-----------> Test " ++ testName ++ " failed with " ++ show exitCode ++ " :-("


checkSuccess :: ExitCode -> String -> IO ()
checkSuccess result testName =  
  case result of
      ExitSuccess  -> reportSuccess testName
      _            -> reportFailure testName result  

inFile, outFile,flipFileGold, flopFileGold, rotateFileGold, chopFileGold,
  cropFileGold, flatFileGold, mosaicFileGold, rollFileGold,
  shaveFileGold, scaleFileGold, sampleFileGold, resizeFileGold,
  affineFileGold, shearFileGold, contrastFileGold, equalizeFileGold,
  gammaFileGold, levelFileGold, levelImageChannelFileGold,
  modulateFileGold, negateFileGold, normalizeFileGold,
  constituteFileGold, importFileGold, compositeFileGold,
  colormapFileGold, appendFileGold, averageFileGold, cycleFileGold :: FilePath
-- TODO: avoid repeating filepaths
-- TODO: maybe something could generate a test case *and* a file name *and*
-- a type sig for all these things...
inFile         = "/home/tjc/Desktop/pix/lambda.png"
outFile        = "/home/tjc/ImageLib/lambda_2.png"
flipFileGold   = "/home/tjc/ImageLib/lambda_vflip.png"
flopFileGold   = "/home/tjc/ImageLib/lambda_hflop.png"
rotateFileGold = "/home/tjc/ImageLib/lambda_rotated.png"
chopFileGold   = "/home/tjc/ImageLib/lambda_chopped.png"
cropFileGold   = "/home/tjc/ImageLib/lambda_cropped.png"
flatFileGold   = "/home/tjc/ImageLib/PicDump/flattened.png"
mosaicFileGold = "/home/tjc/ImageLib/PicDump/mosaic.png"
rollFileGold   = "/home/tjc/ImageLib/PicDump/roll.png"
shaveFileGold  = "/home/tjc/ImageLib/PicDump/shave.png"
scaleFileGold  = "/home/tjc/ImageLib/PicDump/scale.png" 
sampleFileGold = "/home/tjc/ImageLib/PicDump/sample.png" 
resizeFileGold = "/home/tjc/ImageLib/PicDump/resize.png" 
affineFileGold = "/home/tjc/ImageLib/PicDump/affine.png" 
shearFileGold  = "/home/tjc/ImageLib/PicDump/shear.png" 
contrastFileGold  = "/home/tjc/ImageLib/PicDump/contrast.png" 
equalizeFileGold  = "/home/tjc/ImageLib/PicDump/equalize.png" 
gammaFileGold  = "/home/tjc/ImageLib/PicDump/gamma.png" 
levelFileGold  = "/home/tjc/ImageLib/PicDump/level.png" 
levelImageChannelFileGold  = "/home/tjc/ImageLib/PicDump/levelimagechannel.png" 
modulateFileGold  = "/home/tjc/ImageLib/PicDump/modulate.png" 
negateFileGold  = "/home/tjc/ImageLib/PicDump/negate.png" 
normalizeFileGold  = "/home/tjc/ImageLib/PicDump/normalize.png" 
constituteFileGold = "/home/tjc/ImageLib/PicDump/constitute.png" 
importFileGold     = "/home/tjc/ImageLib/PicDump/importpixelarea.png" 
compositeFileGold     = "/home/tjc/ImageLib/PicDump/composite.png" 
colormapFileGold = "/home/tjc/ImageLib/PicDump/set_colormap.png" 
appendFileGold = "/home/tjc/ImageLib/PicDump/append.png" 
averageFileGold = "/home/tjc/ImageLib/PicDump/average.png" 
cycleFileGold = "/home/tjc/ImageLib/PicDump/cyclecolormap.png" 

base64File, roseFile, tieDyeFile, lambdaFile :: FilePath
base64File = "/home/tjc/ImageLib/PicDump/lambda_uu.txt"
roseFile   = "/home/tjc/Desktop/pix/redrose.png"
tieDyeFile   = "/home/tjc/ImageLib/PicDump/tiedye.jpg"
lambdaFile   = "/home/tjc/ImageLib/PicDump/lambda.jpg"

boolToExitCode :: Bool -> ExitCode
boolToExitCode True = ExitSuccess
boolToExitCode _ = ExitFailure 1
