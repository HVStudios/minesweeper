import Data.Map as Map
import Data.Set as Set
import System.Random
import System.Random.Shuffle (shuffle')
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
-- window :: Display
-- window = InWindow "Nice Window" (200, 200) (10, 10)

background :: Color
background = green

drawing :: Picture
drawing = rectangleWire cellSize cellSize

-- display window background drawing 
main :: IO ()
main = do 
    putStr $ "Choose a difficulty :  \n"
        ++ "Type '1' for Beginner \n"
        ++ "Type '2' for Intermediate \n"
        ++ "Type '3' for Expert \n"
    m <- readLn :: IO Int
    let mode = createMode m
    gen <- getStdGen
    putStr $ "You choose " ++ show m ++ "\n"
    play (InWindow "Minesweeper" (600, 600) (500, 500)) (greyN 0.25) 50 (initialState gen mode) graphicsRenderer handler updater 
        where 
            updater _ = id
            handler _ = id
            


createMode :: Int -> Mode
createMode 2 = Mode "Intermediate" (15,15) 40
createMode 3 = Mode "Expert" (30,15) 99
createMode _ = Mode "Beginner" (10,10) 10


mapCell :: (Int, Int) -> (Float, Float)
mapCell (x,y) = (cellSize * fromIntegral x,cellSize * fromIntegral y) 

graphicsRenderer :: GameState -> Picture 
graphicsRenderer gs = pictures [createGrid gs ,createCells gs ,createPics gs]


createGrid :: GameState -> Picture
-- createGrid _ = translate 10.0 10.0 $ rectangleWire 10.0 10.0
-- createGrid _ = pictures [uncurry translate (cellToScreen (x, y)) $ color white $ rectangleWire cellSize cellSize | x <-[0..20-1], y <-[0..14-1]]
createGrid _ = Pictures [(translate (fst $ mapCell (x, y)) (snd $ mapCell (x,y)) $ color black $ rectangleWire cellSize cellSize) | x<-[0..20-1], y<-[0..14-1]]

createCells :: GameState -> Picture
createCells (GameState fld _ mode _) = Pictures [(translate (fst $ mapCell (x, y)) (snd $ mapCell (x,y)) $ (color $ cellColor $ Map.lookup (x,y) fld) $ rectangleSolid cellSize cellSize) | x<-[0..20-1], y<-[0..14-1]]

createPics :: GameState -> Picture
createPics (GameState fld _ mode _) = Pictures [(translate (fst $ mapCell (x, y)) (snd $ mapCell (x,y)) $ color black $ rescale $ text (cellPic $ Map.lookup (x,y) fld)) | x<-[0..20-1], y<-[0..14-1]]
    where
        rescale = translate (-5) (-5) . scale 0.1 0.1 . color black


cellColor :: Maybe CellState -> Color
cellColor Nothing = blue
cellColor (Just (Open x)) = green
cellColor (Just Flag) = orange
cellColor _ = red

cellPic :: Maybe CellState -> String
cellPic Nothing = "lol"
cellPic (Just (Open x)) = show x
cellPic (Just Flag) = "F"
cellPic _ = "X"




--    display window background drawing 


-- fieldSize@(fieldWidth, fieldHeight) = (20,14) :: (Int,Int)
-- mineCount = 50::Int
cellSize = 24 :: Float


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
        mode :: Mode,
        gameOver::Bool
    }

createField::Field
createField = Map.empty 

shuffleCells :: RandomGen g=> g -> Mode -> [Cell] -> [Cell]
shuffleCells gen mode list = shuffle' list (width*height-1) gen
    where
        width = fst $ fieldSize mode
        height = snd $ fieldSize mode 

createMines::RandomGen g => g -> Cell -> Mode -> Mine
createMines gen firstCell mode  = Set.fromList $ Prelude.take mineNumber $ shuffleCells gen mode $ [(x,y)|x<-[0..width-1],y<-[0..height-1],(x,y)/= firstCell]
    where
        width = fst $ fieldSize mode
        height = snd $ fieldSize mode
        mineNumber = mineCount mode

isMine :: Cell -> Field -> Bool
isMine cell field = case Map.lookup cell field of
    (Just Mine) -> True
    _           -> False

initialState :: StdGen -> Mode -> GameState
initialState gen mode = GameState (Map.empty) (Left gen) mode False
