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

dinner = 3

fscale = 3

-- TODO (elias): remove 
backgroundColor, playerColor, enemyColor, emptyColor, bulletColor :: Color
backgroundColor = makeColor 0 0 0 1

emptyColor = makeColor 0 0.5 0.5 0.5

-- TODO (elias): remove 
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
gridToviewCoords (x, y) =
  (fromIntegral x * dblock * fscale, fromIntegral y * dblock * fscale)

drawXAt :: Picture -> Coord -> Picture
drawXAt p c =
  let (vc1, vc2) = gridToviewCoords c
   in translate vc1 vc2 (scale (fscale * dwidth / 10) (fscale * dwidth / 10) p)

pixel :: Color -> Picture
pixel c =
  color c $
  pictures
    [ rectangleWire 10 10
    , rectangleSolid (dinner / dwidth * 10) (dinner / dwidth * 10)
    ]

emptyBoard :: Color -> Picture
emptyBoard c =
  pictures
    [drawXAt (pixel c) (x, y) | x <- [left .. right], y <- [bottom .. top]]

scoreBoard :: Int -> Picture
scoreBoard c =
  let text = translate (-3.5) (-2.5) (scale 0.05 0.05 (Text $ show c))
      board = Color white (rectangleSolid 36 10)
   in pictures [board, text]

renderPic :: Game -> Picture
renderPic (Playing p o b _ t (st, fp) _) =
  pictures
    [ drawXAt (t !! 3) (0, bottom)
    , emptyBoard emptyColor
    , pictures $ fmap (drawXAt $ t !! 1) o
    , pictures $ fmap (drawXAt $ head t) b
    , drawXAt (t !! 2) p
    , drawXAt (scoreBoard fp) (0, bottom - 1)
    ]
-- TODO (elias): remove 
-- renderPic Won = emptyBoard playerColor
-- renderPic GameOver = emptyBoard enemyColor
renderPic (Endscreen score) =
  pictures
    [ emptyBoard enemyColor
    , drawXAt (Color white (rectangleWire (dblock * 6) (dblock * 3))) (0, 0)
    , drawXAt (scoreBoard score) (0, 0)
    ]

-- ENGINE --------------------------------------------------------------
onBoard :: Coord -> Bool
onBoard (x, y) = x >= left && x <= right && y >= bottom && y <= top

atBottom :: Coord -> Bool
atBottom (x, y) = y == bottom

collide :: [Coord] -> [Coord] -> ([Coord], [Coord])
collide c1 c2 = (c1 \\ c2, c2 \\ c1)

moveAndCollide :: (Coord -> Coord) -> ([Coord], [Coord]) -> ([Coord], [Coord])
moveAndCollide move (m, s) = collide (filter onBoard (fmap move m)) (nub s)

generateEnemyLine :: Int -> [Int]
generateEnemyLine seed =
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
    let (pq1, pq2) = moveAndCollide (\(c1, c2) -> (c1, c2 - 1)) (o, b)
        (q1, q2) = moveAndCollide (\(c1, c2) -> (c1, c2 + 1)) (pq2, pq1)
        (l1:l) = ln ++ [generateEnemyLine (t1 + (fp + bp) * 10)]
     in Playing p (q2 ++ [(e - right, top) | e <- l1]) q1 l tex (t1, fp + 1) bp
-- TODO (elias): remove 
nextFrame t g = g

decBound, incBound :: (Ord a, Num a) => a -> a -> a
decBound x b = max b (x - 1)

incBound x b = min b (x + 1)

move :: Event -> Game -> Game
move (EventKey (SpecialKey KeyLeft) Down _ _) (Playing (px, py) o b l t tm bp) =
  Playing (decBound px left, py) o b l t tm (bp + 1)
move (EventKey (SpecialKey KeyRight) Down _ _) (Playing (px, py) o b l t tm bp) =
  Playing (incBound px right, py) o b l t tm (bp + 1)
move (EventKey (SpecialKey KeySpace) Down _ _) (Playing p o b l t tm bp) =
  Playing p o (b ++ [p]) l t tm (bp + 1)
move _ game = game

--- MAIN ---------------------------------------------------------------
main :: IO ()
main = do
  bulletTexture <- loadBMP "resources/bullet.bmp"
  enemyTexture <- loadBMP "resources/enemy.bmp"
  playerTexture <- loadBMP "resources/player.bmp"
  backgroundTexture <- loadBMP "resources/background.bmp"
  starttime <- round `fmap` getPOSIXTime
-- TODO (elias): remove 
  let level1 = [[0, 1, 3, 4, 5, 9, 10], [], [], [], [2, 3, 4, 5, 6, 7, 9, 10]]
  let startGame =
        Playing
          (0, bottom)
          []
          []
          level1
          [bulletTexture, enemyTexture, playerTexture, backgroundTexture]
          (starttime, 0)
          0
  play
    (InWindow "Brick Game (c) Elias Nijs" (500, 900) (10, 10))
    backgroundColor
    2 -- aantal stappen per seconde
    startGame
    renderPic
    move
    nextFrame
