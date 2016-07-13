module IOUtils where

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
