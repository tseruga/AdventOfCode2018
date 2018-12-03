import Data.Map hiding (split)
import Data.Maybe
import Data.List
import Data.Set hiding (split)

main = do
  fileContent <- readFile "input"
  let splitFileContent = lines fileContent
  let areas = [ claimToArea s | s <- splitFileContent ]

  -- Part 1
  let ma = mapAreas areas Data.Map.empty
  print (length(Data.Map.filter filterOverlaps ma))

  -- Part 2
  let overlaps = Data.Map.filter filterOverlaps ma
  let nonOverlapsL = Data.Map.toList (ma Data.Map.\\ overlaps)
  let overlapsL = Data.Map.toList(overlaps)

  let overlappedCids = [snd o | o <- overlapsL]
  let uniqueOverlappedCids = Data.Set.fromList (concat overlappedCids)

  let allUniqueCids = Data.Set.fromList (concat [snd o | o <- Data.Map.toList(ma)])
  print (allUniqueCids Data.Set.\\ uniqueOverlappedCids)


filterOverlaps :: [String] -> Bool
filterOverlaps ss = (length ss) > 1


claimToArea :: String -> (String, [(Int, Int)])
claimToArea str = do
  let ws = words str
  let claimId = tail (head ws)

  let offset = split (init (ws !! 2)) ','
  let offsetT = (read (head offset) :: Int, read (last offset) :: Int)

  let dim = split (last ws) 'x'
  let dimT = (read (head dim) :: Int, read (last dim) :: Int)

  (claimId, [ (x + (fst offsetT), y + (snd offsetT)) | x <- [1..(fst dimT)], y <- [1..(snd dimT)] ])


replaceDelimWithSpace :: String -> Char -> String
replaceDelimWithSpace str d
  | str == "" = ""
  | head str == d = " " ++ replaceDelimWithSpace (tail str) d
  | otherwise = [head str] ++ replaceDelimWithSpace (tail str) d


split :: String -> Char -> [String]
split str c = words (replaceDelimWithSpace str c)


mapPoints :: [(Int, Int)] -> String -> Map (Int,Int) [String] -> Map (Int,Int) [String]
mapPoints ps cid m
  | ps == [] = m
  | otherwise = do
      let currVal = Data.Map.lookup (head ps) m
      case currVal of
        Just x  -> do
          let newVal = [cid] ++ x
          mapPoints (tail ps) cid (Data.Map.insert (head ps) newVal m)
        Nothing -> mapPoints (tail ps) cid (Data.Map.insert (head ps) [cid] m)


mapAreas :: [(String, [(Int, Int)])] -> Map (Int,Int) [String] -> Map (Int,Int) [String]
mapAreas as m
  | as == [] = m
  | otherwise = mapAreas (tail as) (mapPoints (snd (head as)) (fst (head as)) m)
