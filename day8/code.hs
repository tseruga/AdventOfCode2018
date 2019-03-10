import Data.List
-- main = do
--   input <- readFile "only2node"
--   let units = [read i :: Int | i <- (words (input)) ]
--   -- print (length units)
--   print (node units)
--
-- node :: [Int] -> (Int, [Int])
-- node xs
--   | n == 0 = (2 + m, take m (drop 2 xs))
--   | otherwise = do
--       let childrenXs = drop 2 xs
--       let childre = [ node childrenXs | _ <- [1..n]]
--       let children = head childre
--       let ys = drop (fst children) childrenXs
--
--       ((fst (children)) + 2 + m, (take m ys) ++ (snd (children)))
--
--
--   where n = head(xs)
--         m = xs !! 1


reduct f = xs where xs = map f $ inits xs
