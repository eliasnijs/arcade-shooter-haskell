-- Een aantal imports die wel handig kunnen blijken.
import Data.List
import Data.Tuple
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

--------------------------------------------------------------------------------
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

backgroundColor, screenGray :: Color
backgroundColor = makeColor 0 0.05 0.05 1

screenGray = makeColorI 0x64 0x6F 0x5D 0XFF

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

-- De randen van het bord, om te gebruiken in andere functies.
bottom, top :: Y
left, right :: X
bottom = div (-height) 2

top = div height 2

left = div (-width) 2

right = div width 2

-- SPELLOGICA ----------------------------------------------------------
filledPixel :: Picture
filledPixel = undefined

emptyPixel :: Picture
emptyPixel = undefined

emptyBoard :: Picture
emptyBoard = undefined

-- Een gevulde/actieve pixel op de locatie aangeduid door de coördinaat.
drawCoord :: Coord -> Picture
drawCoord = undefined

renderPic :: Game -> Picture
renderPic (Playing p o b _) = undefined
renderPic Won = undefined
renderPic GameOver = undefined

-- SPELLOGICA ----------------------------------------------------------
onBoard :: Coord -> Bool
onBoard = undefined

atBottom :: Coord -> Bool
atBottom = undefined

-- Gegeven twee lijsten van coördinaten, geef deze twee lijsten terug
-- zonder de coördinaten die ze gemeenschappelijk hebben.
collide :: [Coord] -> [Coord] -> ([Coord], [Coord])
collide = undefined

-- Gebruik de `move` functie om de coördinaten in `moving` te verzetten.
-- Bij botsingen met de coördinaten in `static` moeten beide coördinaten
-- verwijderd worden uit de teruggegeven lijsten.
moveAndCollide :: (Coord -> Coord) -> ([Coord], [Coord]) -> ([Coord], [Coord])
moveAndCollide move (moving, static) = undefined

nextFrame :: Float -> Game -> Game
nextFrame t = undefined

-- Hulpfuncties om een getal te decrementeren of incrementeren zonder
-- een grens te overschrijden.
decBound, incBound :: (Ord a, Num a) => a -> a -> a
decBound x b = max b (x - 1)

incBound x b = min b (x + 1)

-- Verwerk gebruiksinput.
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
