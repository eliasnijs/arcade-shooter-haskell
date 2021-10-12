import Data.List
import Data.Tuple
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

type X = Int

type Y = Int

type Coord = (X, Y)

type Line = [X]

data Game
  = Playing
      { player :: Coord
      , obstacles :: [Coord]
      , bullets :: [Coord]
      , lines :: [Line]
      }
  | GameOver
  | Won

backgroundColor, activeColor, inactiveColor :: Color
backgroundColor = makeColor 0 0 0 1

activeColor = makeColor 1 1 1 1

inactiveColor = makeColor 0.2 0.2 0.2 1

-- Enkele constanten om te gebruiken in de rest van de code. Bij het
-- quoteren kunnen wij deze veranderen om te kijken welke invloed ze
-- hebben.
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
width = 10 -- de breedte van het bord (original: 10)

height = 20 -- de hoogte van het bord (original: 20)

dblock = 12 -- de zijde van 1 blokje (original: 12)

dwidth = 10 -- de zijde van 1 blokje (original: 10)

dinner = 7 -- de zijde van 1 blokje (original: 7)

fscale = 3 -- algemene schaal van de hele tekening (original: 3)

-- De randen van het bord
bottom, top :: Y
left, right :: X
bottom = div (-height) 2

top = div height 2

left = div (-width) 2

right = div width 2

-- GRAFISCHE ELEMENTEN ------------------------------------------------
pixel :: Color -> Picture
pixel c =
  let outer = color c $ rectangleWire (fscale * dwidth) (fscale * dwidth)
      inner = color c $ rectangleSolid (fscale * dinner) (fscale * dinner)
   in pictures [outer, inner]

filledPixel :: Picture
filledPixel = pixel activeColor

emptyPixel :: Picture
emptyPixel = pixel inactiveColor

gridToviewCoords :: Coord -> (Float, Float)
gridToviewCoords c =
  let x = fromIntegral $ fst c :: Float
      y = fromIntegral $ snd c :: Float
   in (x * dblock * fscale, y * dblock * fscale)

drawXAt :: Picture -> Coord -> Picture
drawXAt p c = uncurry translate (gridToviewCoords c) p

emptyBoard :: Picture
emptyBoard =
  pictures [drawXAt emptyPixel (x, y) | x <- [left .. right], y <- [bottom .. top]]

drawCoord :: Coord -> Picture
drawCoord = drawXAt filledPixel

renderPic :: Game -> Picture
renderPic (Playing p o b _) =
  pictures
    [ emptyBoard
    , drawCoord p
    , pictures [drawCoord c | c <- o]
    , pictures [drawCoord c | c <- b]
    ]
renderPic Won = emptyBoard
renderPic GameOver = emptyBoard

-- SPELLOGICA ----------------------------------------------------------
onBoard :: Coord -> Bool
onBoard c =
  let x = fst c
      y = snd c
   in x >= left && x <= right && y >= bottom && y <= top

atBottom :: Coord -> Bool
atBottom c = snd c == bottom

-- Gegeven twee lijsten van coördinaten, geef deze twee lijsten terug
-- zonder de coördinaten die ze gemeenschappelijk hebben.
collide :: [Coord] -> [Coord] -> ([Coord], [Coord])
collide c1 c2 =
  let its = c1 `intersect` c2
   in (c1 \\ its, c2 \\ its)

-- Gebruik de `move` functie om de coördinaten in `moving` te verzetten.
-- Bij botsingen met de coördinaten in `static` moeten beide coördinaten
-- verwijderd worden uit de teruggegeven lijsten.
--TODO (elias):
moveAndCollide :: (Coord -> Coord) -> ([Coord], [Coord]) -> ([Coord], [Coord])
moveAndCollide move (moving, static) = undefined

--TODO (elias):
nextFrame :: Float -> Game -> Game
nextFrame t = undefined

-- Hulpfuncties om een getal te decrementeren of incrementeren zonder
-- een grens te overschrijden.
decBound, incBound :: (Ord a, Num a) => a -> a -> a
decBound x b = max b (x - 1)

incBound x b = min b (x + 1)

--TODO (elias):
move :: Event -> Game -> Game
move (EventKey (SpecialKey KeyLeft) Down _ _) = undefined
move (EventKey (SpecialKey KeyRight) Down _ _) = undefined
move (EventKey (SpecialKey KeySpace) Down _ _) = undefined
move _ = undefined

-- MAIN -----------------------------------------------------------------
level1 :: [Line]
level1 = [[0, 1, 3, 4, 5, 9, 10], [], [], [], [2, 3, 4, 5, 6, 7, 9, 10]]

startGame = Playing (0, -10) [] [] level1

-- main =
--   play
--     (InWindow "Brick Game (c) Elias Nijs" (500, 800) (10, 10))
--     backgroundColor
--     2 -- aantal stappen per seconde
--     startGame
--     renderPic
--     move
--     nextFrame
main =
  display
    (InWindow "Brick Game (c) Elias Nijs" (500, 800) (10, 10))
    backgroundColor
    (renderPic startGame)
