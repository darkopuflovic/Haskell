import System.IO
import Data.Bits
import Data.Char
import System.CPUTime
import Data.Time

main :: IO()
main = do
    putStr ("\x1b[32m" ++ "Unesite broj diskova: " ++ "\x1b[31m")
    hFlush stdout
    discCount <- getLine
    let n = toInteger (parseInt discCount)
    putStr ("\x1b[32m" ++ "Unesite \x1b[94m\"T\"\x1b[32m ili \x1b[94m\"t\"\x1b[32m ukoliko zelite da rezultati budu prikazani: \x1b[94m")
    hFlush stdout
    showResult <- getLine
    let showRes = parseBool showResult
    time hanoi n "A" "B" "C" "slow" showRes
    time hanoiFast n "A" "B" "C" "fast" showRes

hanoi :: Integer -> String -> String -> String -> [(String, String)]
hanoi 0 _ _ _ = []
hanoi n f i l = hanoi (n - 1) f l i ++ [(f, l)] ++ hanoi (n - 1) i f l

firstPeg :: Integer -> Integer
firstPeg x = (x .&. (x - 1)) `mod` 3

lastPeg :: Integer -> Integer
lastPeg x = ((x .|. (x - 1)) + 1) `mod` 3

mapChar :: String -> String -> String -> Integer -> String
mapChar f _ _ 0 = f
mapChar _ i _ 1 = i
mapChar _ _ l 2 = l

hanoiFast :: Integer -> String -> String -> String -> [(String, String)]
hanoiFast 0 _ _ _ = []
hanoiFast n f i l = zip (map (mapChar f i l . firstPeg) [1..2^n - 1]) (map (mapChar f i l . lastPeg) [1..2^n - 1])

joinArray :: [String] -> String
joinArray [x] = x
joinArray (head:tail) = head ++ "\n" ++ joinArray tail

arrowArray :: [(String, String)] -> [String]
arrowArray = map formatArrow

formatArrow :: (String, String) -> String
formatArrow tuple = fst tuple ++ " -> " ++ snd tuple

parseCharInt :: Char -> Int
parseCharInt a | ord a < 48 = 0
               | ord a > 57 = 0
               | otherwise  = ord a - 48

parseInt :: String -> Int
parseInt []    = 0
parseInt [a]   = parseCharInt a
parseInt (a:b) = (parseCharInt a * 10) + parseInt b

parseCharBool :: Char -> Bool
parseCharBool a | ord a == 84 = True
                | ord a == 116 = True
                | otherwise = False

parseBool :: String -> Bool
parseBool []    = False
parseBool [a]   = parseCharBool a
parseBool (a:b) = parseCharBool a

printLines :: [String] -> IO ()
printLines [val] = putStrLn ("\x1b[92m" ++ val ++ "\x1b[0m")
printLines (head:tail) = do
    putStrLn ("\x1b[92m" ++ head ++ "\x1b[0m")
    printLines tail

time :: (Integer -> String -> String -> String -> [(String, String)]) -> Integer -> String -> String -> String -> String -> Bool -> IO ()
time func n f i l sf showResult = do
    putStrLn ("\x1b[36mHanoi \x1b[96m" ++ sf ++ "\x1b[36m:")
    startCPU <- getCPUTime
    start <- getCurrentTime
    let hanoiResult = func n f i l
    let result = arrowArray hanoiResult
    if showResult then printLines result else putStrLn "\x1b[92mRezultati nisu prikazani."
    putStrLn ("\x1b[36mNumber of moves (\x1b[96mhanoi " ++ sf ++ "\x1b[36m): \x1b[31m" ++ show (length hanoiResult))
    end <- getCurrentTime
    endCPU <- getCPUTime
    putStrLn ("\x1b[36mTime (\x1b[91mCPU\x1b[36m): \x1b[31m" ++ show (fromIntegral (endCPU - startCPU) / (10^12)))
    putStrLn ("\x1b[36mTime (\x1b[91mTime\x1b[36m): \x1b[31m" ++ show (diffUTCTime end start) ++ "\x1b[0m")