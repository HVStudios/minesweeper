import Data.Map as Map
import Data.Set as Set
import System.Random
import System.Random.Shuffle (shuffle')
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
import System.IO.Unsafe

import Test.HUnit 

------------------------------------------------------------------------------------------------------------------------------------------------
-- DATA STRUCTURES
------------------------------------------------------------------------------------------------------------------------------------------------

{- CellStatus
  describes the state of a cell. a cell can have different states, these are:
	  Mine - contains a mine
	  Flag - flagged by player
      Open - opened by player, also displays the amount of mines around the cell
-} 
data CellState = Mine | Flag | Open Int  deriving(Eq,Show)

{- Field
    Represents the playing field. 
    The cell coordinates are stored as the key and the cell state as the value.
-}
type Field = Map Cell CellState

{-- Cell
  Represents a cell. 
  The cell’s coordinate position is represented by two ints in a tuple. 
 --}
type Cell = (Int,Int)

{- Mine
  represents mines, all the mines are stored in a set as Cells
-}
type Mine = Set Cell

{- Mode
  Represents the difficulty setting. 
    name is the name of the difficulity setting
    fieldSize is the size of the playing field, two ints in a tuple represent the amount of nodes on the x and y axises
    mineCount is the amount of mines that will be placed
-}
data Mode = Mode 
    {
        name                :: [Char],
        fieldSize           :: (Int, Int),
        mineCount           :: Int
    } deriving(Eq,Show)

{- GameState
  Describes the state of the game.
    field is the playing field
    mines is either a StdGen (this is only the case if a click hasnt been performed) or Mine
    mode is the game mode (difficulty)
    gameOver represents if the game is over or not, True means its over, False means its not.
-}
data GameState = GameState
    {
        field :: Field,
        mines :: Either StdGen Mine,
        mode :: Mode,
        gameOver :: Bool
    } deriving(Show)

------------------------------------------------------------------------------------------------------------------------------------------------
-- MAIN
------------------------------------------------------------------------------------------------------------------------------------------------
{- main
Runs the game.
SIDE EFFECTS: Reads input from keyboard, prints output to the screen.
-}

main :: IO ()
main = do 
    putStr $ "Choose a difficulty :  \n"
        ++ "Type '1' for Beginner \n"
        ++ "Type '2' for Intermediate \n"
        ++ "Type '3' for Expert \n"
    m <- readLn :: IO Int
    let mode = createMode m
    gen <- getStdGen
    putStr $ "You chose " ++ show m ++ "\n"
    play (InWindow "ITSweeper" (windowSize $ fieldSize mode) (300, 300)) (greyN 0.25) 50 (initialState gen mode) graphicsRenderer keyHandler updater 
        where 
            updater _ = id
            windowSize (x,y) = (round cellSize * x, round cellSize * y)

------------------------------------------------------------------------------------------------------------------------------------------------
-- CONSTANTS
------------------------------------------------------------------------------------------------------------------------------------------------

-- choose size of cells
cellSize = 30 :: Float

------------------------------------------------------------------------------------------------------------------------------------------------
-- Auxiliary functions
------------------------------------------------------------------------------------------------------------------------------------------------

{- createMode int
     Chooses the size of the playing field and amount of mines.
     RETURNS: a Mode depending on the given int. If 'int' isn't 2 or 3, the "Beginner" Mode will be chosen.
     EXAMPLES: createMode 1 == Mode {name = "Beginner", fieldSize = (10,10), mineCount = 10}
  -}
createMode :: Int -> Mode
createMode 2 = Mode "Intermediate" (15,15) 40
createMode 3 = Mode "Expert" (30,15) 99
createMode _ = Mode "Beginner" (10,10) 10

{- cellColor mbCS
     Assigns different colors to different cell states.
     RETURNS: a color corresponding to the mbCS given
     EXAMPLES: 
  -}
cellColor :: Maybe CellState -> Color
cellColor Nothing = blue
cellColor (Just (Open x)) = green
cellColor (Just Flag) = orange
cellColor _ = red

{- cellPic mbCS
     Assigns labels to cells depending on what the cell's state is.
     RETURNS: a String. 'mbCS' decides what String is returned. 
     EXAMPLES: 
  -}
cellPic :: Maybe CellState -> String
cellPic Nothing = "DV"
cellPic (Just (Open x)) = show x
cellPic (Just Flag) = "F"
cellPic _ = "IT"

{-   initialState gen mode
     Creates a GameState of the initial state of the game
     RETURNS: a default GameState which uses the gen and mode from the arguments. 
     EXAMPLES: initialState (mkStdGen 5) (createMode 1) == GameState {field = fromList [], mines = Left (StdGen {unStdGen = SMGen 15450398106449630581 7134611160154358619}), mode = Mode {name = "Beginner", fieldSize = (10,10), mineCount = 10}, gameOver = False}
-}

initialState :: StdGen -> Mode -> GameState
initialState gen mode = GameState Map.empty (Left gen) mode False

-- gets the screen coordinates of a cell
cellToScreen :: (Int, Int) -> (Float, Float)
cellToScreen (x,y) = (cellSize * fromIntegral x,cellSize * fromIntegral y)

-- Adds a cell to a map.
addCell :: Cell -> CellState -> Field -> Field
addCell = Map.insert

-- Applies a function to both of the elements in a tuple.
mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (x,y) = (f x, f y)

{- shuffleCells gen mode list
Shuffles the elements in a list using a random generator.
PRE: The list cannot be empty.
RETURNS: "list" but with its elements shuffled around randomly using "gen"
EXAMPLES: shuffleCells (mkStdGen 5) (createMode 1) [(1,1),(2,2)] == [(2,2),(1,1)]
-}
shuffleCells :: RandomGen g=> g -> Mode -> [Cell] -> [Cell]
shuffleCells gen mode list = shuffle' list (length list) gen

{- createMines gen cell mode
Spawns mines on the playing field by creating a Set with cells, every cell in the Set is a mine.
It creates a list of every possible coordinate of the playing field, shuffles it, then takes x ammount of elements from it(x is how many mines we need)
and then creates a set with all of the coordinates we took.
RETURNS: Mine (a set of cells)

-}
createMines::RandomGen g => g -> Cell -> Mode -> Mine
createMines gen firstCell mode  = Set.fromList $ Prelude.take mineNumber $ shuffleCells gen mode $ [(x,y)|x<-[0..width-1],y<-[0..height-1],(x,y)/= firstCell]
    where
        width = fst $ fieldSize mode
        height = snd $ fieldSize mode
        mineNumber = mineCount mode

{- isMine cell field
     Checks if a cell contains a mine
     RETURNS: True if '???' contains a mine, False if it does not.
     EXAMPLES: 
  -}
isMine :: Cell -> Field -> Bool
isMine cell field = case Map.lookup cell field of
    (Just Mine) -> True
    _           -> False
------------------------------------------------------------------------------------------------------------------------------------------------
-- GUI FUNCTIONS
------------------------------------------------------------------------------------------------------------------------------------------------

-- translates the grid into a viewport, to make it fit the screen we have to shift it a bit
-- because gloss starts drawing from the middle
viewPort :: (Int, Int) -> ViewPort
viewPort (x, y) = ViewPort (mapTuple (negate . (/ 2) . (subtract cellSize)) $ cellToScreen (x, y)) 0 1

{- graphicsRenderer gamestate 
Renders the GUI with information from the gamestate (the size of the field).
Combines several Pictures into one: the picture of the cells, the piture of the grid, and the picture of the labels on the cells.
RETURNS: A picture made with information from "gamestate"
gs@ makes it so you can access the entire gamestate as an argument gs but also use the different arguments in it.

-}
graphicsRenderer :: GameState -> Picture 
graphicsRenderer gs@(GameState fld _ mode _) = applyViewPortToPicture (viewPort playAreaSize) $ pictures [createCells gs ,createGrid gs, createPics gs]
    where
        playAreaSize = fieldSize mode

{- createGrid gamestate
Creates the GUI for the grid with information from the gamestate (the size of the field).
Creates a list with all possible coordinates for the window and makes the outline of a rectangle on the coordinates to make a grid.
RETURNS: A picture(that looks like a grid) made with information from "gamestate"
-}

createGrid :: GameState -> Picture
createGrid (GameState fld _ mode _) = Pictures [(translate (fst $ cellToScreen (x, y)) (snd $ cellToScreen (x,y)) $ color black $ rectangleWire cellSize cellSize) | x<-[0..(fst size)-1], y<-[0..(snd size)-1]]
    where
        size = fieldSize mode

{-createCells gamestate
Creates the picture for the cells with information from the gamestate (the size of the field)
Creates a list with all possible coordinates for the window and makes a solid rectangle on the coordinates.
RETURNS: A picture made with information from "gamestate"
-}
createCells :: GameState -> Picture
createCells (GameState fld _ mode _) = Pictures [(translate (fst $ cellToScreen (x, y)) (snd $ cellToScreen (x,y)) $ (color $ cellColor $ Map.lookup (x,y) fld) $ rectangleSolid cellSize cellSize) | x<-[0..(fst size)-1], y<-[0..(snd size)-1]]
    where
        size = fieldSize mode

{- createPics gamestate
Creates the pictures for what to label the cells with(flagged, nearby cells etc) with information from the gamestate(the size of the field)
Creates a list with all possible coordinates for the window and draws a "label" on the coordinates.
RETURNS: A picture made with information from "gamestate"

-}
createPics :: GameState -> Picture
createPics (GameState fld _ mode _) = Pictures [(translate (fst $ cellToScreen (x, y)) (snd $ cellToScreen (x,y)) $ color black $ rescale $ text (cellPic $ Map.lookup (x,y) fld)) | x<-[0..(fst size)-1], y<-[0..(snd size)-1]]
    where
        -- rescale rescales the text because its huge by default in gloss and adjusts its position so its not in top right corner of a cell.
        rescale = translate (-8) (-8) . scale 0.1 0.1 . color black
        size = fieldSize mode

-- gets the cell coordinates of a screen, both coordinates are divided by the size of the cell and rounded, getting its index.
screenToCell :: (Float, Float) -> (Int, Int) -> (Int, Int)
screenToCell screen grid = mapTuple (round . (/ cellSize)) (invertViewPort (viewPort grid) screen)


------------------------------------------------------------------------------------------------------------------------------------------------
-- MOUSE FUNCTIONALITY
------------------------------------------------------------------------------------------------------------------------------------------------

 {- keyHandler key gamestate
     Handles keypresses and mouse clicks and changing the board depending on the state of the
     game, and the key pressed/mousebutton clicked
     PRE:  DUBBELKOLLA DETTA MEN TROR INTE DET
     RETURNS: A new gamestate with actions performed
     SIDE EFFECTS: If the user action triggered a change in the game (e.g. opening or flagging a cell), the GUI is
     updated with a new field
     EXAMPLES: Not possible to test keyHandler function via terminal as far as we know, but helper functions can be tested (see below)
  -}
keyHandler :: Event -> GameState -> GameState
--left click 
keyHandler (EventKey (MouseButton LeftButton) Down _ mouse) GameState
    {
        field = field,
        mines = mines,
        mode = mode,
        gameOver = gameOver
    } =
    case mines of
        (Left mine) -> GameState  --First cell clicked, play area needs to be generated
            {
                field = action (screenToCell mouse playAreaSize) field,
                mines = Right getMines,
                mode = mode,
                gameOver = False
            }
        (Right mine) -> if gameOver == False --Second click and onwards, checks if fields are mines
            then GameState
                {
                    field = action (screenToCell mouse playAreaSize) field,
                    mines = Right mine,
                    mode = mode,
                    gameOver = isMine (screenToCell mouse playAreaSize) redoField
                }
            else GameState
                {
                    field = field,
                    mines = mines,
                    mode = mode,
                    gameOver = gameOver
                }
        where
            playAreaSize = fieldSize mode 
            redoField = action (screenToCell mouse playAreaSize) field -- Redraws the playing field with the new information from a mouse click
            {- getMines mines (from gamestate)
                If the field is empty, getMines passes on all arguments required to create the mines, otherwise it
                just processes the action (like opening a cell, or flagging one)
                RETURNS: If field is already created, it simply returns the mines from gamestate. If field has not been created it
                creates the mines, which then results in it "converting" Left mines to Right mines
                EXAMPLES: IMPOSSIBLE: "Perhaps you meant ‘getLine’"
            -}
            getMines :: Mine
            getMines = case mines of
                (Right ms) -> ms
                (Left ms) -> createMines ms (screenToCell mouse playAreaSize) mode
            {- action (c1, c2) field
                takes a cell and current field and recreates the field with the user interaction added
                RETURNS: A new field, where the user interaction is combined with the previous field
                EXAMPLES: DO LATER IF POSSIBLE
            -}
            action :: Cell -> Field -> Field
            action (coord1, coord2) fld
                | Map.member (coord1, coord2) fld = fld
                | Set.member (coord1, coord2) getMines = openMinesAll
                | otherwise = if nearbyMines 
                    then openSafe
                    else openSafeAll
                where
                    {- nearby (c1, c2)
                        Creates a list of cells near the cell currently clicked on. This happens by mapping all the possible moves
                        from the current cell and then filtering out the moves which are deemed out of bounds
                        RETURNS: List of cells neighbouring the current cell
                        EXAMPLES: No can do sir
                    -}
                    nearby :: [Cell]
                    nearby = Prelude.filter checkBounds
                        $ Prelude.map (\(a,b) -> (a + coord1, b + coord2)) moves
                            where
                                checkBounds = \(a, b) -> (a < x && 0 <= a) && (b < y && 0 <= b)
                                moves = [(1, 0), (0, 1), (-1, 0), (0, -1), (1, 1), (-1, -1), (1, -1), (-1, 1)]
                                x = fst playAreaSize
                                y = snd playAreaSize
                                

                    {- countNearbyMines (c1, c2)
                        Counts the number of mines nearby the current cell. If it is zero then it returns zero, otherwise it explores
                        the neighbouring cells
                        RETURNS: An int representing number of mines nearby the current cell
                        EXAMPLES: No can do sir
                    -}
                    countNearbyMines :: Int
                    countNearbyMines = length $ Prelude.filter (`Set.member` getMines) nearby

                    {-  nearbyMines (c1, c2)
                        Checks if there are mines nearby the current cell
                        RETURNS: True if the number of nearby mines are not zero, otherwise returns false
                        EXAMPLES: No can do sir
                    -}
                    nearbyMines :: Bool
                    nearbyMines = 0 /= countNearbyMines

                    {- openSafe (c1, c2)
                        Opens a cell with no nearby mines
                        RETURNS: A field with the cell in question set to "Open"
                        EXAMPLES: Nein
                    -}
                    openSafe :: Field
                    openSafe = addCell (coord1, coord2) (Open countNearbyMines) fld

                    {- openSafeAll (c1, c2)
                        Opens all nearby cells which are safe
                        RETURNS: A field with all adjacent safe cells set to "Open"
                        EXAMPLES: Nein
                    -}
                    openSafeAll :: Field
                    openSafeAll = Prelude.foldr action openSafe nearby

                    {- openMinesAll (c1, c2)
                        Opens all mines once a mine is clicked
                        RETURNS: A field with all the mines opened
                        EXAMPLES: Nein
                    -}
                    openMinesAll :: Field
                    openMinesAll = Prelude.foldr action openMines $ Set.elems getMines
                        where openMines = addCell (coord1, coord2) Mine fld
     

-- right click
keyHandler (EventKey (MouseButton RightButton) Down _ mouse) GameState
    {
        field = field,
        mines = (Right mines),
        mode = mode,
        gameOver = False
    } = 
        case Map.lookup (screenToCell mouse playAreaSize) field of
        Nothing -> GameState -- If field does not contain flag, add a flag
            {
                field = Map.insert (screenToCell mouse playAreaSize) Flag field,
                mines = Right mines,
                mode = mode,
                gameOver = False
            }
        (Just Flag) -> GameState -- If field contains a flag, remove the flag
            {
                field = Map.delete (screenToCell mouse playAreaSize) field,
                mines = Right mines,
                mode = mode,
                gameOver = False
            }
        (Just _) -> GameState -- anything else, do nothing
            {
                field = field,
                mines = Right mines,
                mode = mode,
                gameOver = False
            }
        where playAreaSize = fieldSize mode

--restarts the game if "r" is pressed
keyHandler (EventKey(Char 'r') Down _ _) GameState{gameOver = True, mode = mode} = initialState gen mode 
    where gen = unsafePerformIO newStdGen

keyHandler _ gamestate = gamestate -- ignore all other inputs

{- RESTART BUTTON CODE PLACEHOLDER-}

------------------------------------------------------------------------------------------------------------------------------------------------
-- TEST CASES ska ligga här
------------------------------------------------------------------------------------------------------------------------------------------------
-- cellToScreen
test1 = TestCase $ assertEqual "cellToScreen (0,0)" (0.0,0.0) (cellToScreen (0,0))
test2 = TestCase $ assertEqual "cellToScreen (3,5)" (90.0,150.0) (cellToScreen (3,5))

-- createMines
-- test3 = TestCase $ assertEqual "createMines gen (2,3) mode" (Set.fromList [(0,0),(0,2),(0,3),(0,6),(0,8),(0,9),(0,13),(1,1),(1,2),(1,7),(2,0),(2,1),(2,2),(4,11),(6,0),(6,1),(6,10),(6,12),(7,2),(7,5),(8,3),(8,4),(9,0),(9,2),(9,8),(9,9),(9,12),(9,13),(9,14),(10,3),(10,4),(10,13),(11,2),(11,9),(11,14),(12,0),(13,9),(13,11),(13,14),(14,9)]) (createMines gen (2,3) mode)
--    where
--        gen = mkStdGen 5
--        mode = createMode 2

-- initialState
-- test4 = TestCase $ assertEqual "initialState gen (createMode 2)" GameState {field = Map.fromList [], mines = Left (gen), mode = Mode {name = "Intermediate", fieldSize = (15,15), mineCount = 40}, gameOver = False} (initialState gen (createMode 2))
--     where 
--         gen = mkStdGen 5

-- addCell
test5 = TestCase $ assertEqual "addCell (5,5) Mine field" (Map.fromList [((5,5),Mine)]) (addCell (5,5) Mine field)
    where
        field = Map.empty

test6 = TestCase $ assertEqual "addCell (5,5) (Open 3) field" (Map.fromList [((5,5),Open 3)])(addCell (5,5) (Open 3) field)
    where
        field = Map.fromList [((5,5),Mine)]

test7 = TestCase $ assertEqual "addCell (3,2) Flag field" (Map.fromList [((3,2),Flag),((5,5),Mine)]) (addCell (3,2) Flag field)
    where
        field = Map.fromList [((5,5),Mine)]

-- mapTuple
test8 = TestCase $ assertEqual "mapTuple ((+) 5) (1,2)" (6,7) (mapTuple ((+) 5) (1,2))

-- shuffleCells
test9 = TestCase $ assertEqual "Testing if the list gets shuffled" False (shuffleCells gen mode list == list)
    where
        gen = mkStdGen 5
        mode = createMode 2
        list = [(1,1),(2,2)]

-- isMine
test10 = TestCase $ assertEqual "basecase for if the field is empty" False (isMine (3,3) Map.empty)

test11 = TestCase $ assertEqual "making sure it returns True if its a mine"  True (isMine (3,3) field)
    where
        field = Map.fromList [((3,3),Mine),((5,5),Flag)]

-- cellColor
test12 = TestCase $ assertEqual "basecase cellcolor" blue (cellColor Nothing)

-- cellPic
test13 = TestCase $ assertEqual "basecase cellpic" "DV" (cellPic Nothing)

runtests = runTestTT $ TestList [test1,test2,test5,test6,test7,test8,test9,test10,test11,test12,test13]