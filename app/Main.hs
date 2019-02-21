{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import           Control.Arrow
import           Control.Concurrent
import           Control.Monad.Random
import           Control.Monad.Reader
import           Data.Colour.RGBSpace
import           Data.Colour.RGBSpace.HSV
import           Data.Foldable            (for_)
import           Data.List                (nub)
import           Data.Semigroup           ((<>))
import           Data.Time.Clock.POSIX
import           Graphics.Rendering.Cairo
import           Linear.V2
import           Linear.Vector
import qualified Numeric.Noise.Perlin     as P
import           Text.Printf

data World = World
  { worldWidth  :: Int
  , worldHeight :: Int
  , worldSeed   :: Int
  , worldScale  :: Double
  }

type Generate a = RandT StdGen (ReaderT World Render) a

-- | Lift a Cairo action into a Generate action
cairo :: Render a -> Generate a
cairo = lift . lift

getSize :: Num a => Generate (a, a)
getSize = do
  (w, h) <- asks (worldWidth &&& worldHeight)
  pure (fromIntegral w, fromIntegral h)

data Poly = Poly [V2 Double] deriving (Eq, Ord)

fromIntegralVector :: V2 Int -> V2 Double
fromIntegralVector (V2 x y) = V2 (fromIntegral x) (fromIntegral y)

quadAt :: V2 Double -> Poly
quadAt v =
    Poly
      [ v
      , (v ^+^ V2 0 1.5)
      , (v ^+^ V2 1.5 1.5)
      , (v ^+^ V2 1.5 0)
      ]

triAt :: V2 Double -> Poly
triAt v =
    Poly
      [ v
      , (v ^+^ V2 0 1.5)
      , (v ^+^ V2 1.5 1.5)
      ]

removeIndex :: Int -> [a] -> [a]
removeIndex _ []        = []
removeIndex i (y:ys)
| i == 0                = removeItem (i-1) ys
| otherwise             = y : removeItem (i-1) ys

rotatedTriAt :: V2 Double -> Poly
rotatedTriAt v =
    Poly . removeIndex <$> skipIndex <*> pure points
    where
      skipIndex = getRandomR (0, 3)
      points =
        [ v
        , (v ^+^ V2 0 1.5)
        , (v ^+^ V2 1.5 1.5)
        , (v ^+^ V2 1.5 0)
        ]

--- my version
genShapeGrid :: (V2 Double -> Poly) -> Generate [Poly]
genShapeGrid shapeAt= do
  (w, h) <- getSize @Int
  vectors <- replicateM 800 $ do
    v <- V2 <$> getRandomR (3, w `div` 2 - 3) <*> getRandomR (3, h `div` 2 - 3)
    pure $ v ^* 2
  pure . nub . flip map vectors $ \v ->
    shapeAt (fromIntegralVector v)

renderClosedPath :: [V2 Double] -> Render ()
renderClosedPath (V2 x y:vs) = do
  newPath
  moveTo x y
  for_ vs $ \v -> let V2 x' y' = v in lineTo x' y'
  closePath
renderClosedPath [] = pure ()

renderPoly :: Poly -> Render ()
renderPoly (Poly points) = renderClosedPath points

polyAddNoise :: Poly -> Generate Poly
polyAddNoise (Poly points)= do
  perlinSeed <- fromIntegral <$> asks worldSeed

  let
    perlinOctaves = 5
    perlinScale = 0.1
    perlinPersistance = 0.5
    perlinNoise
      = P.perlin (round perlinSeed) perlinOctaves perlinScale perlinPersistance
    perlin2d (V2 x y)
      = P.noiseValue perlinNoise (x + perlinSeed, y + perlinSeed, perlinSeed) - 0.5
    addNoise v = let noise = perlin2d v in v ^+^ V2 (noise / 5) (noise / 8)

  pure $ Poly (map addNoise points)

fillScreen :: (Double -> Render a) -> Double -> Generate ()
fillScreen color opacity = do
  (w, h) <- getSize @Double
  cairo $ do
    rectangle 0 0 w h
    color opacity *> fill

hsva :: Double -> Double -> Double -> Double -> Render ()
hsva h s v = setSourceRGBA channelRed channelGreen channelBlue
 where RGB{..} = hsv h s v

eggshell :: Double -> Render ()
eggshell = hsva 71 0.13 0.96

darkGunmetal :: Double -> Render ()
darkGunmetal = hsva 170 0.30 0.16

teaGreen :: Double -> Render ()
teaGreen = hsva 81 0.25 0.94

vividTangerine :: Double -> Render ()
vividTangerine = hsva 11 0.40 0.92

englishVermillion :: Double -> Render ()
englishVermillion = hsva 355 0.68 0.84

tutorialColors =
    [ teaGreen
    , vividTangerine
    , englishVermillion
    , darkGunmetal
    ]

kuCrimson :: Double -> Render ()
kuCrimson = hsva 4 0.96 0.89

seaFoamGreen :: Double -> Render ()
seaFoamGreen = hsva 138 0.26 0.90

bananaMania :: Double -> Render ()
bananaMania = hsva 55 0.30 0.99

saffron :: Double -> Render ()
saffron = hsva 40 0.83 0.97

darkPurple :: Double -> Render ()
darkPurple = hsva 293 0.36 0.18

myColors =
  [ kuCrimson
  , seaFoamGreen
  , saffron
  , darkPurple
  ]

bgColor = bananaMania
blockColors = myColors

renderSketch :: Generate ()
renderSketch = do
  fillScreen bgColor 1

  cairo $ setLineWidth 0.15

  shapes <- genShapeGrid triAt
  noisyShapes <- traverse polyAddNoise shapes

  for_ noisyShapes $ \shape -> do
    strokeOrFill <- weighted [(fill, 0.4), (stroke, 0.6)]
    color <- uniform blockColors
    cairo $ do
      renderPoly shape
      color 1 *> strokeOrFill

main :: IO ()
main = do
  seed <- round . (*1000) <$> getPOSIXTime
  let
    stdGen = mkStdGen seed
    width = 60
    height = 60
    scaleAmount = 20

    scaledWidth = round $ fromIntegral width * scaleAmount
    scaledHeight = round $ fromIntegral height * scaleAmount

  surface <- createImageSurface FormatARGB32 scaledWidth scaledHeight
  -- The "world" thinks the width and height are the initial values, not scaled.
  let world = World width height seed scaleAmount

  void
    . renderWith surface
    . flip runReaderT world
    . flip runRandT stdGen
    $ do
      cairo $ scale scaleAmount scaleAmount
      renderSketch

  putStrLn "Generating art..."
  surfaceWriteToPNG surface
    $ "images/example_sketch/"
    <> show seed <> "-" <> show (round scaleAmount :: Int) <> ".png"
  surfaceWriteToPNG surface "images/example_sketch/latest.png"

