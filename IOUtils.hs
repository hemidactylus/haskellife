module IOUtils where

import System.IO
import Data.Char

-- put cursor position on screen
goto :: (Int,Int) -> IO ()
goto (x,y) = sputStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

-- clear screen
cls :: IO ()
cls = sputStr "\ESC[2J"

-- (ugly way to) delay execution some time
wait :: Int -> IO ()
wait n = seqn [return () | _ <- [1..n] ]

-- pad string with spaces so that it is exactly n characters wide
resizemsg :: Int -> String -> String
resizemsg n xs = reverse (take n ((reverse xs) ++ repeat ' '))

-- print string to specified position
writeat :: (Int,Int) -> String -> IO ()
writeat p xs = do goto p
                  sputStr xs

-- write string
sputStr :: String -> IO ()
sputStr [] = return ()
sputStr (x:xs) = do putChar x
                    sputStr xs


rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : rmdups (filter (/= x) xs)

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (a:as) = do a
                 seqn as

repitem :: Int -> a -> [a]
repitem n x = [x | _ <- [1..n]]

-- handling colors
type Color = Int

openColor :: Color -> String
openColor n = "\ESC[1;" ++ show n ++ "m"

closeColor :: String
closeColor = "\ESC[0m"

-- lower-levels: adds a color start/end signature to a string
colorText :: Color -> String -> String
colorText n xs = openColor n ++ xs ++ closeColor

-- this handles fore- and background colors (with whifts) and formats the whole string
colorizeText :: Color -> Color -> String -> String
colorizeText f b xs = colorText (30+f) (colorText (40+b) xs)

takelast :: [a] -> a
takelast xs = head (reverse xs)

stickintolist :: Int -> a -> [a] -> [a]
stickintolist n x xs = take n (x:xs)

reverseindex :: Eq a => a -> [a] -> Int
reverseindex x xs = head [j | (i,j) <- zip (xs) [1.. length xs] , i==x]

-- some workaround to get characters with no echo back - does not work properly with Hugs
getCh :: IO Char
getCh = do  oldBufferMode <- hGetBuffering stdin
            hSetBuffering stdin NoBuffering
            hSetEcho stdin False
            c <- getChar
            hSetEcho stdin True
            hSetBuffering stdin oldBufferMode
            return c

-- capturing control characters such as arrows:
-- two testing functions to see the control sequences
sgetchars :: IO [Char]
sgetchars = getchars []

getchars :: [Char] -> IO [Char]
getchars cs = do    c <- getCh
                    putStrLn ("#" ++ (foldr  (\ a1 a2 -> a1 ++ " " ++ a2) "" (map (show . ord) (cs ++ [c]))) ++ "#")
                    getchars (cs ++ [c])

-- a type carrying control-char info
type GenChar = (Int, Char)
    -- i.e. 0/1 (normal/control) + a char
-- a type carrying control sequences
type CharSeq = ([Int],Char)
-- an array of ascii codes + a desired representation char

getGenChar :: IO GenChar
getGenChar = getcharseq [] seqMap

charsToInts :: [Char] -> [Int]
charsToInts = map ord

seqMap :: [CharSeq]
seqMap = [([27,91,65],'u'), ([27,91,66],'d'), ([27,91,67],'r'), ([27,91,68],'l')]
-- i.e. up/down/right/left arrows

maptospecialchar :: [Char] -> [CharSeq] -> GenChar
maptospecialchar = \ ls ss -> head [ (1,c) | (is,c) <- ss, is==( charsToInts ls ) ]

canstillbesequence :: [Char] -> [CharSeq] -> Bool
canstillbesequence = \ ls ss -> length [(1, c) | (is,c) <- ss, (take (length ls) is) == (charsToInts ls) ] > 0

-- this holds the focus until it either has a sequence not fitting for sure into seqMap or one exactly from it
getcharseq :: [Char] -> [CharSeq] -> IO GenChar
getcharseq cs ss = do   c <- getCh
                        if elem (charsToInts (cs++[c])) [is | (is,c) <- ss] then
                            return  ( maptospecialchar (cs ++ [c]) ss )
                        else
                            -- check if partial seqs are still possible
                            if canstillbesequence (cs ++ [c]) ss then
                                getcharseq (cs++[c]) ss
                            else
                                return (0,c)
