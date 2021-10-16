import Data.List
import Data.Tuple
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

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
      }
  | GameOver
  | Won

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

emptyBoard :: Picture
emptyBoard =
  pictures
    [drawXAt (pixel inactiveColor) (x, y) | x <- [left .. right], y <- [bottom .. top]]

renderPic :: Game -> Picture
renderPic (Playing p o b _ t) =
  pictures
    [ emptyBoard
    , pictures $ fmap (drawXAt $ scaleImage $ t !! 1) o 
    , pictures $ fmap (drawXAt $ scaleImage $ head t) b
    , drawXAt (scaleImage $ t !! 2) p
    ]
renderPic Won =
  pictures
    [drawXAt (pixel playerColor) (x, y) | x <- [left .. right], y <- [bottom .. top]]
renderPic GameOver =
  pictures
    [drawXAt (pixel enemyColor) (x, y) | x <- [left .. right], y <- [bottom .. top]]

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

nextFrame :: Float -> Game -> Game
nextFrame t (Playing p o b ln tex)
  | any atBottom o = GameOver
  | all null o && all null ln = Won
  | otherwise =
    let t = moveAndCollide (\c -> (fst c, snd c - 1)) (o, b)
        q = moveAndCollide (\c -> (fst c, snd c + 1)) (snd t, fst t)
        l = ln ++ [[]]
     in Playing
          p
          (snd q ++ [(e - right, top) | e <- head l])
          (fst q)
          (tail l)
          tex
nextFrame t g = g

decBound, incBound :: (Ord a, Num a) => a -> a -> a
decBound x b = max b (x - 1)

incBound x b = min b (x + 1)

move :: Event -> Game -> Game
move (EventKey (SpecialKey KeyLeft) Down _ _) (Playing p o b l t) =
  Playing (decBound (fst p) left, snd p) o b l t
move (EventKey (SpecialKey KeyRight) Down _ _) (Playing p o b l t) =
  Playing (incBound (fst p) right, snd p) o b l t
move (EventKey (SpecialKey KeySpace) Down _ _) (Playing p o b l t) =
  Playing p o (b ++ [p]) l t
move _ game = game

--- MAIN ---------------------------------------------------------------
level1 :: [Line]
level1 = [[0, 1, 3, 4, 5, 9, 10], [], [], [], [2, 3, 4, 5, 6, 7, 9, 10]]

main :: IO ()
main = do
  bulletTexture <- loadBMP "resources/bullet.bmp"
  enemyTexture <- loadBMP "resources/enemy.bmp"
  playerTexture <- loadBMP "resources/player.bmp"
  let startGame =
        Playing
          (0, bottom)
          []
          []
          level1
          [bulletTexture, enemyTexture, playerTexture]
  bulletImage <- loadBMP "resources/bullet.bmp"
  play
    (InWindow "Brick Game (c) Elias Nijs" (500, 800) (10, 10))
    backgroundColor
    1 -- aantal stappen per seconde
    startGame
    renderPic
    move
    nextFrame
