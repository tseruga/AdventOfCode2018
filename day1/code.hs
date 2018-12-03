import Data.List
import qualified Data.Set as Set

main = do
  fileContent <- readFile "input"
  let splitFileContent = lines fileContent

  let intL = strLtoIntL (splitFileContent)
  print (sum(intL))

  let intList = cycle intL
  print (firstDoubleFrequency intList 0 0 (Set.fromList []))

strLtoIntL :: [String] -> [Int]
strLtoIntL strs = [read (stripPlus s) :: Int | s <- strs]

stripPlus :: String -> String
stripPlus str = if head str == '+'
                then tail str
                else str

firstDoubleFrequency :: [Int] -> Int -> Int -> Set.Set Int -> Int
firstDoubleFrequency xs i freq fSet
  | Set.member freq fSet = freq
  | otherwise = do
      let newFreq = freq + (xs !! i)
      firstDoubleFrequency xs (i + 1) newFreq (Set.insert freq fSet)
