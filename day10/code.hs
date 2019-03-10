import Data.List
import Data.Function
import Data.Ord

main = do
  input <- readFile "inputSmall"
  let ls = lines input
  let stars = [parseLine l | l <- ls]
  -- print stars
  -- plotStars stars
  print(entropy stars)

-- Oh god, I hate parsing strings in Haskell so muchhhhh
parseLine :: String -> ((Int,Int),(Int,Int),(Int,Int))
parseLine l = do
  let ws = words l
  let pss = ws !! 0 ++ ws !! 1 ++ ws !! 2
  let vss = ws !! 3 ++ ws !! 4 ++ ws !! 5
  let ps = init (replaceDelimWithSpace (drop 10 pss) ',')
  let vs = init(replaceDelimWithSpace (drop 10 vss) ',')
  let ips = [read p :: Int | p <- words ps]
  let ivs = [read v :: Int | v <- words vs]
  ((ips !! 0, ips !! 1), (ivs !! 0, ivs !! 1), (0,0))


-- Plotting! This question does not favor this language choice
plotStars :: [((Int,Int),(Int,Int),(Int,Int))] -> IO ()
plotStars stars = do
  let minX = px (minimumBy (compare `on` px) stars)
  let minY = py (minimumBy (compare `on` py) stars)
  let maxX = px (maximumBy (compare `on` px) stars)
  let maxY = py (maximumBy (compare `on` py) stars)

  let rawGrid = [((x,y),'-') | x <- [minX..maxX], y <- [minY..maxY]]
  let starsToPlot = [ ((px s,py s), '#') | s <- stars]

  let a = (groupBy ((==) `on` fst . fst) [head x | x <- groupBy ((==) `on` fst) (sort (rawGrid ++ starsToPlot))])
  putStr (unlines (map (map snd) a))


replaceDelimWithSpace :: String -> Char -> String
replaceDelimWithSpace str d
  | str == "" = ""
  | head str == d = " " ++ replaceDelimWithSpace (tail str) d
  | otherwise = [head str] ++ replaceDelimWithSpace (tail str) d


incrementStar :: ((Int,Int),(Int,Int),(Int,Int)) -> ((Int,Int),(Int,Int),(Int,Int))
incrementStar ((px,py),(vx,vy),(dx,dy)) = ((px,py),(vx,vy),(dx+vx,dy+vy))

-- In this case, high entropy is good. Im just bad at names
entropy :: [((Int,Int),(Int,Int),(Int,Int))] -> Int
entropy stars = do
    length(maximumBy (compare `on` length) (groupBy ((==) `on` px) xstars)) + length(maximumBy (compare `on` length) (groupBy ((==) `on` py) ystars))
    where xstars = sortBy (compare `on` px) stars
          ystars = sortBy (compare `on` py) stars

-- These had to be flipped to orient output properly, I guess
py ((x,_),(_,_),(dx,_)) = x + dx
px ((_,y),(_,_),(_,dy)) = y + dy
