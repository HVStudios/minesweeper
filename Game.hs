

import Data.Map as Map
import Data.Set as Set
import System.Random
import System.Random.Shuffle (shuffle')
import Graphics.Gloss
window :: Display
window = InWindow "Nice Window" (200, 200) (10, 10)

background :: Color
background = green

drawing :: Picture
drawing = circle 80

main :: IO ()
main = display window background drawing

-- fieldSize@(fieldWidth, fieldHeight) = (20,14) :: (Int,Int)
-- mineCount = 50::Int



data CellState = Mine | Flag | Open Int
type Field = Map Cell CellState
type Cell = (Int,Int)
type Mine = Set Cell

data Mode = Mode 
    {
        name                :: [Char],
        fieldSize           :: (Int, Int),
        mineCount           :: Int
    } deriving Show


data GameState = GameState
    {
        field::Field,
        mines::Either StdGen Mine,
        gameOver::Bool
    }

createField::Field
createField = Map.empty 

shuffleCells :: RandomGen g=> g -> Mode -> [Cell] -> [Cell]
shuffleCells g mode list = shuffle' list (width*height-1) g
    where
        width = fst $ fieldSize mode
        height = snd $ fieldSize mode 

createMines::RandomGen g => g -> Cell -> Mode -> Mine
createMines g firstCell mode  = Set.fromList $ Prelude.take mineNumber $ shuffleCells g mode $ [(x,y)|x<-[0..width-1],y<-[0..height-1],(x,y)/= firstCell]
    where
        width = fst $ fieldSize mode
        height = snd $ fieldSize mode
        mineNumber = mineCount mode

isMine :: Cell -> Field -> Bool
isMine cell field = case Map.lookup cell field of
    (Just Mine) -> True
    _           -> False

