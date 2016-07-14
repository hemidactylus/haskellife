import System.IO
import Data.Char
import IOUtils

-- TYPES

type Pos = (Int,Int)

type Board = [Pos]

type Bstate = ([Board],Board, Int, Int)
    -- previous_list, current, num_of_live_cells, generation

-- SETUP AND INITIALISATIONS

width :: Int
width = 40

height :: Int
height = 20

sleeptime :: Int
sleeptime = 80000

emptyboard :: Board
emptyboard = []

fullboard :: Board
fullboard = [(x,y) | x <- [1..width], y <- [1..height] ]

-- this dictates the maximum loop length that can be detected
historyLength :: Int
historyLength = 4

glider :: Board
glider = [(4,2),(2,3),(4,3),(3,4),(4,4)]

oscillator :: Board
oscillator = [(4,5),(4,6),(4,7)]

-- period-three oscillator
caterer :: Board
caterer = [(3,1),(1,2),(5,2),(6,2),(7,2),(8,2),(1,3),(5,3),(1,4),(4,5),(2,6),(3,6)]

stationary :: Board
stationary = [(4,4),(4,5),(5,4),(5,5),(10,10),(12,12),(12,10)]

livingcell :: String
livingcell = colorizeText 2 3 "*"

emptycell :: String
emptycell = colorizeText 0 5 " "

bordersign :: String
bordersign = colorizeText 4 1 "+"

-- UTILITY OPERATIONS ON CELLS

neighbs :: Pos -> [Pos]
neighbs (x,y) = map wrap [(x+d,y+e) | (d,e) <- neigoffsets]

neigoffsets :: [(Int,Int)]
neigoffsets = [(-1,-1),(0,-1),(1,-1),(1,0),(1,1),(0,1),(-1,1),(-1,0)]
                            
wrap :: Pos -> Pos
wrap (x,y) =    (
                    ((x-1) `mod` width) +1,
                    ((y-1) `mod` height) +1
                )

pickx :: Pos -> Int
pickx (x,y) = x

picky :: Pos -> Int
picky (x,y) = y

-- this does the modulo-size ops to fit the pattern into the board
cropboard :: Board -> Board
cropboard b = map fitintofield b

-- shift a configuration with no checks on boundaries
shift :: Pos -> Pos -> Pos
shift (i,j) (x,y) = (x+i,y+j)

bshift :: Pos -> Board -> Board
bshift p = map (shift p)

fitintofield :: Pos -> Pos
fitintofield (x,y) = ( (((x-1) `mod` width) +1), (((y-1) `mod` height) +1) )

isAlive :: Board -> Pos -> Bool
isAlive b p = elem p b

isEmpty :: Board -> Pos -> Bool
isEmpty b p = not (isAlive b p)

liveneighbs :: Board -> Pos -> Int
liveneighbs b = length . filter (isAlive b) . neighbs

survivors :: Board -> [Pos]
survivors b = [p | p <- b, elem (liveneighbs b p) [2,3] ]

births :: Board -> [Pos]
births b = [p | p <- rmdups (concat (map neighbs b)),
                isEmpty b p,
                liveneighbs b p == 3 ]

nextgen :: Board -> Board
nextgen b = survivors b ++ births b

-- MAIN LOOPING OPERATIONS

life :: Board -> IO (Bstate)
life b = do cls
            drawborders
            showcells ([fullboard],(cropboard . rmdups) b,0,0)
            showcells ([emptyboard],(cropboard . rmdups) b,0,0)
            (bs,q,n,g) <- lifeloop ([emptyboard], (cropboard . rmdups) b, (length b), 0 )
            putStrLn ("\n\n\tFinished in " ++ (show g) ++ " generations.")
            putStrLn ("\tLoop of length " ++ (show n) ++ " detected.\n\n")
            return ([q],q,n,g)

lifeloop :: Bstate -> IO (Bstate)
lifeloop (bs,q,n,g) = if elem q bs then do
                             showcells (bs,q,n,g)
                             return (bs,q,reverseindex q bs,g)
                      else do
                             showcells (bs,q,n,g)
                             wait sleeptime
                             lifeloop (stickintolist historyLength q bs,p,m,h) where
                                 p = nextgen q
                                 m = length p
                                 h = g + 1

handlelife :: IO Bstate
handlelife = do c <- editboard
                if c == emptyboard then
                    return ([],[],0,0)
                else do
                    d <- life c
                    handlelife

editboard :: IO Board
editboard = do  putStr (editmenu beditors)
                a <- getChar
                menuchoice a beditors editboard

-- EDITOR
menuchoice :: Char -> [(Char, String, IO Board)] -> IO Board -> IO Board
menuchoice d bs f = head ([i | (c,s,i) <- bs, c==d] ++ [f])

editmenu :: [(Char, String, IO Board)] -> String
editmenu [] = ""
editmenu ((c,s,i):xs) = "  '" ++ [c] ++ "' ==> " ++ s ++ "\n" ++ editmenu xs

beditors :: [(Char, String, IO Board)]
beditors =  [ ('c', "caterer", return (bshift (5,2) caterer)),
              ('g', "glider" , return glider),
              ('e', "editor",  ieditor),
              ('q', "<quit>",  do cls
                                  return emptyboard)]

-- interactive editor is a looping construct starting from the empty board
ieditor :: IO Board
ieditor = do drawborders
             b <- lieditor fullboard emptyboard (1,1)
             return b

editmessage :: String
editmessage = "Editor (h=help)"
             
-- this IS TO FIX
lieditor :: Board -> Board -> Pos -> IO Board
lieditor p b (x,y) = do showcells ([p], b, 0, -1 )
                        writeat (x+1,y+1) (colorizeText 6 3 "*")
                        c <- getChar
                        case c of
                           'x' -> return b
                           'z' -> lieditor b (togglepos (wrap (x+1,y)) b) (wrap (x+1,y))
                           ' ' -> lieditor b (togglepos (x,y) b) (x,y)
                           otherwise -> return b

togglepos :: Pos -> Board -> Board
togglepos o (p:ps) = if o == p then
                        togglepos o ps
                     else
                        [p] ++ togglepos o ps
togglepos o []     = [o]
                    
-- END EDITOR
                     
showcells :: Bstate -> IO ()
showcells (bs,q,n,g)  = do  seqn [writeat ((pickx p)+1,(picky p)+1) livingcell | p <- q, not (elem p b)]
                            seqn [writeat ((pickx p)+1,(picky p)+1) emptycell | p <- b, not (elem p q)]
                            if g >= 0 then
                                writeat (4,height+2) (colorizeText 6 3 infomsg)
                            else
                                writeat (4,height+2) (colorizeText 4 5 editmessage) where
                                    
                                    infomsg = (" Gen " ++ (resizemsg 5 (show g)) ++ " : " ++ (resizemsg 5 (show n)) ++ " ")
                                    b = head bs

drawborders :: IO ()
drawborders = do seqn [writeat (1,y) bordersign | y <- [1..height+1] ]
                 seqn [writeat (width+2,y) bordersign | y <- [1..height+1] ]
                 seqn [writeat (x,1) bordersign | x <- [1..width+1] ]
                 seqn [writeat (x,height+2) bordersign | x <- [1..width+2] ]

-- ADDITIONAL STUFF

strtopair :: [Char] -> [Pos]
strtopair [] = []
strtopair [x] = []
strtopair (x:y:zs) = (pairtopos x y):(strtopair zs)

pairtopos :: Char -> Char -> Pos
pairtopos c g = fitintofield (1000 + ((ord c) *5) + ord g, ((ord g)*2) + (ord c))

liven = life . strtopair

-- 280:
{--
liven "ascsdfaqawre2349icq[-34v346b#%^B$^B#sndhdfn54n6dNYTDNRYDDFGSERTSVTRDDYT$%^BW$B%^ENW$^%$^#$%&B#^&#^&&V#%@V#$%@V#$%@#V$%@V^%*&M*&(<&)*)&_>&)<*<%%*M%N$^B&$@VC!@X%$DERGSTVDNTUFNGUJNTFURTSETBGFHBDTDYRTBGDFYUYDRTFBXHFDY^BTRDXNFJDFKI<MBCV$"
--}

