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

backgroundColor, activeColor :: Color
backgroundColor = makeColor 0 0 0 1

activeColor = makeColor 1 1 1 1

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
width = 10 -- de breedte van het bord

height = 20 -- de hoogte van het bord

dblock = 12 -- de zijde van 1 blokje (inclusief marge rondom)

dwidth = 10 -- de zijde van 1 blokje (exclusief marge, inclusief randje)

dinner = 7 -- de zijde van 1 blokje (enkel het zwarte stuk middenin)

fscale = 3 -- algemene schaal van de hele tekening

-- De randen van het bord
bottom, top :: Y
left, right :: X
bottom = div (-height) 2

top = div height 2

left = div (-width) 2

right = div width 2

-- GRAPGISCHE ELEMENTEN ------------------------------------------------
gridToviewCoords :: Coord -> (Float, Float)
gridToviewCoords c =
  let x = fromIntegral $ fst c :: Float
      y = fromIntegral $ snd c :: Float
   in (x, y)

pixel :: Picture
pixel =
  let length = fscale * dblock
   in rectangleSolid length length

filledPixel :: Picture
filledPixel = color activeColor pixel

emptyPixel :: Picture
emptyPixel = color backgroundColor pixel

emptyBoard :: Picture
emptyBoard = pictures []

drawCoord :: Coord -> Picture
drawCoord c = uncurry translate (gridToviewCoords c) filledPixel

renderPic :: Game -> Picture
--(TODO: elias)
renderPic (Playing p o b _) = emptyBoard
renderPic Won = emptyBoard
renderPic GameOver = emptyBoard

-- SPELLOGICA ----------------------------------------------------------
onBoard :: Coord -> Bool
onBoard c =
  let x = fst c
      y = snd c
   in x >= left && x <= right && y >= bottom && y <= top

atBottom :: Coord -> Bool
atBottom c =
  let y = snd c
   in y == bottom

-- Gegeven twee lijsten van coördinaten, geef deze twee lijsten terug
-- zonder de coördinaten die ze gemeenschappelijk hebben.
collide :: [Coord] -> [Coord] -> ([Coord], [Coord])
collide c1 c2 =
  let its = c1 `intersect` c2
   in (c1 \\ its, c2 \\ its)

-- Gebruik de `move` functie om de coördinaten in `moving` te verzetten.
-- Bij botsingen met de coördinaten in `static` moeten beide coördinaten
-- verwijderd worden uit de teruggegeven lijsten.
--(TODO: elias)
moveAndCollide :: (Coord -> Coord) -> ([Coord], [Coord]) -> ([Coord], [Coord])
moveAndCollide move (moving, static) = undefined

--(TODO: elias)
nextFrame :: Float -> Game -> Game
nextFrame t = undefined

-- Hulpfuncties om een getal te decrementeren of incrementeren zonder
-- een grens te overschrijden.
--(TODO: elias)
decBound, incBound :: (Ord a, Num a) => a -> a -> a
decBound x b = max b (x - 1)

incBound x b = min b (x + 1)

--(TODO: elias)
moveInput :: Event -> Game -> Game
moveInput (EventKey (SpecialKey KeyLeft) Down _ _) = undefined
moveInput (EventKey (SpecialKey KeyRight) Down _ _) = undefined
moveInput (EventKey (SpecialKey KeySpace) Down _ _) = undefined
moveInput _ = undefined

-- MAIN -----------------------------------------------------------------
level1 :: [Line]
level1 = [[0, 1, 3, 4, 5, 9, 10], [], [], [], [2, 3, 4, 5, 6, 7, 9, 10]]

startGame = Playing (0, -10) [] [] level1

main =
  play
    (InWindow "Brick Game (c) Elias Nijs" (500, 800) (10, 10))
    backgroundColor
    2 -- aantal stappen per seconde
    startGame
    renderPic
    moveInput
    nextFrame
