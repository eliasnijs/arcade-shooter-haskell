import Data.List
import Data.Time.Clock.POSIX
import Data.Tuple
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random

type X = Int

type Y = Int

type Coord = (X, Y)

type Line = [X]

type Texture = Picture

data Game
  = Playing
      { player :: Coord
      , obstacles :: [Coord]
      , bullets :: [Coord]
      , lines :: [Line]
      , textures :: [Texture]
      , time :: (Int, Int)
      , buttonsPressed :: Int
      }
  | GameOver
  | Won
  | Endscreen
      { score :: Int
      }

--- <-----------------------> dblock
---     <---------------> dwidth
---         <-------> dinner
---     +---------------+
---     |               |
---     |   MMMMMMMMM   |
---     |   MMMMMMMMM   |
---     |   MMMMMMMMM   |
---     |               |
---     +---------------+
width = 10

height = 20

dblock = 12

dwidth = 10

dinner = 5

fscale = 3

backgroundColor, playerColor, enemyColor, inactiveColor, bulletColor :: Color
backgroundColor = makeColor 0 0 0 1

inactiveColor = makeColor 0 0.1 0.1 1

playerColor = makeColor 0 1 1 1

enemyColor = makeColor 1 0.5 0 1

bulletColor = makeColor 1 1 1 1

bottom, top :: Y
left, right :: X
bottom = div (-height) 2

top = div height 2

left = div (-width) 2

right = div width 2

--- RENDERER -----------------------------------------------------------
gridToviewCoords :: Coord -> (Float, Float)
gridToviewCoords c =
  let x = fromIntegral $ fst c :: Float
      y = fromIntegral $ snd c :: Float
   in (x * dblock * fscale, y * dblock * fscale)

drawXAt :: Picture -> Coord -> Picture
drawXAt p c = uncurry translate (gridToviewCoords c) p

scaleImage :: Picture -> Picture
scaleImage = scale fscale fscale

pixel :: Color -> Picture
pixel c =
  let outer = rectangleWire (fscale * dwidth) (fscale * dwidth)
      inner = rectangleSolid (fscale * dinner) (fscale * dinner)
   in color c $ pictures [outer, inner]

emptyBoard :: Color -> Picture
emptyBoard c =
  pictures
    [drawXAt (pixel c) (x, y) | x <- [left .. right], y <- [bottom .. top]]

scoreBoard :: Int -> Picture
scoreBoard c =
  let text =
        translate
          (-fscale * 2)
          (-fscale * 2.5)
          (scale (fscale * 0.05) (fscale * 0.05) (Color black (Text $ show c)))
      board =
        Color white (rectangleSolid (3 * (dblock + 1) * fscale) (10 * fscale))
   in pictures [board, text]

renderPic :: Game -> Picture
renderPic (Playing p o b _ t (st, fp) _) =
  pictures
    [ scaleImage $ translate 0 (fromIntegral bottom * dblock) (t !! 3)
    , emptyBoard inactiveColor
    , pictures $ fmap (drawXAt $ scaleImage $ t !! 1) o
    , pictures $ fmap (drawXAt $ scaleImage $ head t) b
    , drawXAt (scaleImage $ t !! 2) p
    , translate 0 (fromIntegral (bottom - 1) * dblock * fscale) (scoreBoard fp)
    ]
renderPic Won = emptyBoard playerColor
renderPic GameOver = emptyBoard enemyColor
renderPic (Endscreen score) =
  pictures
    [ Color white (rectangleWire (fscale * dblock * 6) (fscale * dblock * 3))
    , scoreBoard score
    ]

-- ENGINE --------------------------------------------------------------
onBoard :: Coord -> Bool
onBoard c =
  let x = fst c
      y = snd c
   in x >= left && x <= right && y >= bottom && y <= top

atBottom :: Coord -> Bool
atBottom (x, y) = y == bottom

collide :: [Coord] -> [Coord] -> ([Coord], [Coord])
collide c1 c2 = (c1 \\ c2, c2 \\ c1)

moveAndCollide :: (Coord -> Coord) -> ([Coord], [Coord]) -> ([Coord], [Coord])
moveAndCollide move (moving, static) =
  collide (filter onBoard (fmap move moving)) (nub static)

generateLine :: Int -> [Int]
generateLine seed =
  let t :: RandomGen g => Int -> g -> [Int]
      t n = take n . unfoldr (Just . uniformR (0, 1))
      booleanLine = t (width + 1) (mkStdGen seed) :: [Int]
      empty = sum $ t 2 (mkStdGen $ seed + 271) :: Int
   in [c | c <- [0 .. width], (booleanLine !! c) == 1 && empty == 0]

nextFrame :: Float -> Game -> Game
nextFrame t (Playing p o b ln tex (t1, fp) bp)
  | any atBottom o = Endscreen fp
  -- | all null o && all null ln = Won
  | otherwise =
    let t = moveAndCollide (\c -> (fst c, snd c - 1)) (o, b)
        q = moveAndCollide (\c -> (fst c, snd c + 1)) (snd t, fst t)
        l = ln ++ [generateLine (t1 + (fp + bp) * 10)]
     in Playing
          p
          (snd q ++ [(e - right, top) | e <- head l])
          (fst q)
          (tail l)
          tex
          (t1, fp + 1)
          bp
nextFrame t g = g

decBound, incBound :: (Ord a, Num a) => a -> a -> a
decBound x b = max b (x - 1)

incBound x b = min b (x + 1)

move :: Event -> Game -> Game
move (EventKey (SpecialKey KeyLeft) Down _ _) (Playing p o b l t tm bp) =
  Playing (decBound (fst p) left, snd p) o b l t tm (bp + 1)
move (EventKey (SpecialKey KeyRight) Down _ _) (Playing p o b l t tm bp) =
  Playing (incBound (fst p) right, snd p) o b l t tm (bp + 1)
move (EventKey (SpecialKey KeySpace) Down _ _) (Playing p o b l t tm bp) =
  Playing p o (b ++ [p]) l t tm (bp + 1)
move _ game = game

--- MAIN ---------------------------------------------------------------
level1 :: [Line]
level1 = [[0, 1, 3, 4, 5, 9, 10], [], [], [], [2, 3, 4, 5, 6, 7, 9, 10]]

main :: IO ()
main = do
  bulletTexture <- loadBMP "resources/bullet.bmp"
  enemyTexture <- loadBMP "resources/enemy.bmp"
  playerTexture <- loadBMP "resources/player.bmp"
  backgroundTexture <- loadBMP "resources/background.bmp"
  starttime <- round `fmap` getPOSIXTime
  let startGame =
        Playing
          (0, bottom)
          []
          []
          level1
          [bulletTexture, enemyTexture, playerTexture, backgroundTexture]
          (starttime, 0)
          0
  bulletImage <- loadBMP "resources/bullet.bmp"
  play
    (InWindow "Brick Game (c) Elias Nijs" (500, 900) (10, 10))
    backgroundColor
    2 -- aantal stappen per seconde
    startGame
    renderPic
    move
    nextFrame
