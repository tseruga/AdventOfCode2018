import Data.List
import Data.Char

main = do
  inputWithNewLine <- readFile "input"
  let input = init(inputWithNewLine)

  -- Part 1
  print (length (react (getPairs input) (getPairs input)))

  -- Part 2 :: This take a while...
  let units = zip ['a'..'z'] ['A'..'Z']
  let inputs = [ filter (/=(snd unit)) (filter (/=(fst unit)) input) | unit <- units ]
  print (head (sort [ length (react (getPairs i) (getPairs i) ) | i <- inputs]))

getPairs :: String -> [(Char,Char)]
getPairs s = zip s (tail s)

unzipPairs :: [(Char,Char)] -> String
unzipPairs ps = do
  let uz = unzip ps
  (fst uz) ++ [(last (snd uz))]

react :: [(Char,Char)] -> [(Char,Char)] -> String
react poly polyImmute
  | poly == [] = unzipPairs polyImmute
  | abs( (ord(fst (head poly))) - (ord(snd (head poly))) ) == 32 = do
      let removeIdx = findIndex (==(head poly)) polyImmute
      case removeIdx of
        Just idx  -> do
          -- If we are the first or last element in the list
          if (length poly == length polyImmute) || (length (tail poly) == 0)
            then do
              let newPolyImmute = (take idx polyImmute) ++ (drop (idx + 1) polyImmute)
              react newPolyImmute newPolyImmute
          -- We are in the middle of the list
            else do
              let newPair = (fst (polyImmute !! (idx - 1)), snd (polyImmute !! (idx + 1)))
              let newPolyImmute = (take (idx - 1) polyImmute) ++ [newPair] ++ (drop (idx + 2) polyImmute)
              react newPolyImmute newPolyImmute
        Nothing   -> do
          -- This case should never be hit
          let newPolyImmute = (take 0 polyImmute) ++ (drop (1) polyImmute)
          react newPolyImmute newPolyImmute
  | otherwise = react (tail poly) polyImmute
