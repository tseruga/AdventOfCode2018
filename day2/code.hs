import Data.List
import Data.Ord

main = do
  fileContent <- readFile "input"
  let splitFileContent = lines fileContent

  -- Part 1
  let twos = [s | s <- splitFileContent, hasExactlyNOfAnyLetter s 2]
  let threes = [s | s <- splitFileContent, hasExactlyNOfAnyLetter s 3]
  print ((length twos) * (length threes))

  -- Part 2
  let allPairs = [(x,y) | x <- splitFileContent, y <- splitFileContent, x /= y]
  let common = [commonLetters (fst x) (snd x) | x <- allPairs]
  print (maximumBy (comparing length) common)


hasExactlyNOfAnyLetter :: String -> Int -> Bool
hasExactlyNOfAnyLetter str n = [ s | s <- str, length (findIndices (==s) str) == n] /= []


commonLetters :: String -> String -> String
commonLetters a b
  | a == "" = ""
  | otherwise = case (head a) == (head b) of
      True -> [(head a)] ++ commonLetters (tail a) (tail b)
      False -> commonLetters (tail a) (tail b)
