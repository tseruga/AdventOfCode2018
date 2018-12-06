import Data.Map
import Data.List
import Data.Ord

main = do
  fileContent <- readFile "input"
  let fc = lines fileContent
  let splitFileContent = sortBy (comparing (Data.List.take 18)) fc

  let logActions = ([logEntryToAction s | s <- splitFileContent])
  let sleepChanges = Data.List.filter removeBegins (groupActionsByGuard logActions "n/a")

  let pairs = everyOther (zip (sleepChanges) (tail sleepChanges))
  let flatPairs = flattenMinutes pairs Data.Map.empty

  let unzippedFlatPairs = (unzip (Data.Map.toList flatPairs))
  let sleepiestGid = fst (maximumBy (comparing snd) (zip (fst unzippedFlatPairs) [length s | s <- snd unzippedFlatPairs]))

  case Data.Map.lookup sleepiestGid flatPairs of
    Just x  -> print ((mode x) * (read sleepiestGid :: Int))
    Nothing -> print "No answer"

  let idAndMode = (zip (fst unzippedFlatPairs) [ length (findIndices (==(mode x)) x) | x <- snd unzippedFlatPairs])
  let idModeVal = zip idAndMode [mode x | x <- snd unzippedFlatPairs]

  let ans = (maximumBy (comparing (fstsnd)) idModeVal)
  print ((read (fst (fst ans)) :: Int) * (snd ans))


removeBegins :: (String, Int, String) -> Bool
removeBegins x = (third x) /= "Begin"


mostAsleep :: (String, [Int]) -> (String, [Int]) -> Bool
mostAsleep a b = length (snd a) > length (snd b)


logEntryToAction :: String -> (String, Int, String)
logEntryToAction str = do
  let ws = words str
  let minutes = read (Data.List.take 2 (Data.List.drop 3 (ws !! 1))) :: Int
  let entry = Data.List.drop 2 ws

  if "Guard" `elem` entry
    then (tail (entry !! 1), minutes, "Begin")
    else if "falls" `elem` entry
      then ("noid", minutes, "Sleep")
      else ("noid", minutes, "Wake")


groupActionsByGuard :: [(String, Int, String)] -> String -> [(String, Int, String)]
groupActionsByGuard l lastId
  | l == [] = []
  | otherwise = if first (head l) == "noid"
                  then (lastId, second (head l), third (head l)) : groupActionsByGuard (tail l) lastId
                  else (first (head l), second (head l), third (head l)) : groupActionsByGuard (tail l) (first (head l))


flattenMinutes :: [((String, Int, String), (String, Int, String))] -> Map String [Int] -> Map String [Int]
flattenMinutes pairs m
  | pairs == [] = m
  | otherwise = do
      let sleep = fst (head pairs)
      let wake = snd (head pairs)
      let minutes = init[(second sleep)..(second wake)]

      let currVal = Data.Map.lookup (first sleep) m
      case currVal of
        Just x  -> flattenMinutes (tail pairs) (Data.Map.insert (first sleep) (x ++ minutes) m)
        Nothing -> flattenMinutes (tail pairs) (Data.Map.insert (first sleep) minutes m)


mode :: [Int] -> Int
mode xs = head (maximumBy (comparing length) (group (sort xs)) )


everyOther l
  | l == [] = []
  | otherwise = (head l):everyOther' (tail l)


everyOther' l
  | l == [] = []
  | otherwise = everyOther (tail l)


first (x,_,_) = x
second (_,x,_) = x
third (_,_,x) = x

fstsnd ((_,x),_) = x
