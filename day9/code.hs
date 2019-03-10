import Data.Sequence
import Data.List

-- This solution sucks
main = do
  let ss = Data.List.replicate 455 0
  print (maximum (marble(Data.Sequence.fromList [0]) 1 0 0 ss))

marble :: Seq Int -> Int -> Int -> Int -> [Int] -> [Int]
marble ms num cIdx cP ss
  | num == (7122300 + 1) = ss
  | num `mod` 23 == 0 = do
      let ncIdx = (cIdx - 7) `mod` Data.Sequence.length ms
      case Data.Sequence.lookup ncIdx ms of
        Just deletedVal -> do
          let nScr = (ss !! cP) + num + deletedVal
          let nss = (Data.List.take cP ss) ++ [nScr] ++ (Data.List.drop (cP + 1) ss)
          let nms = Data.Sequence.deleteAt ncIdx ms
          marble nms (num + 1) ncIdx ((cP + 1) `mod` 455) nss
        Nothing -> error "Index too big"
  | otherwise = do
      let pos = ((cIdx + 2) `mod` Data.Sequence.length ms)
      let nms = Data.Sequence.insertAt pos num ms
      marble nms (num + 1) pos ((cP + 1) `mod` 455) ss
