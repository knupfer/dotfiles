{-# LANGUAGE OverloadedStrings #-}
import           Data.List (intersperse)
import qualified Data.Text    as T
import qualified Data.Text.IO as T (interact)

main :: IO ()
main = T.interact parseCSV

parseCSV :: T.Text -> T.Text
parseCSV = T.unlines .
           putPipes .
           align .
           stripNum .
           stripEmpty .
           splitDelimiter .
           filter (not . T.null) .
           T.lines

putPipes :: [[T.Text]] -> [T.Text]
putPipes = map (\t -> T.strip $ T.unwords $ intersperse "|" $ "":t ++ [""])

stripEmpty :: [[T.Text]] -> [[T.Text]]
stripEmpty = doStrip 0
  where
    doStrip n m
      | isShort n m = m
      | all (\x -> T.null (x !! n)) m = doStrip n $ newList n m (flip const)
      | otherwise = doStrip (n + 1) m

newList :: Int -> [[T.Text]] -> (T.Text -> [T.Text] -> [T.Text]) -> [[T.Text]]
newList n m fun = map ((\(a,y:ys) -> a ++ fun y ys) . splitAt n) m

align :: [[T.Text]] -> [[T.Text]]
align = doAlign 0
  where
    doAlign n m
      | isShort n m = m
      | otherwise = doAlign (n+1) $ newList n m (\x xs -> pad x :xs)
      where pad = T.justifyRight (maximum $ map (T.length . flip (!!) n) m) ' '

stripNum :: [[T.Text]] -> [[T.Text]]
stripNum = doStrip 0
  where
    doStrip n m
      | isShort n m = m
      | all (isStrNum (reads :: String -> [(Int,String)])) m = doStrip (n + 1) m
      | all (isStrNum (reads :: String -> [(Double,String)])) m = doStrip (n + 1)
               $ newList n m (\y ys -> (let s = T.pack $ show (read $ T.unpack y :: Double)
                                        in if T.last s == '0'
                                              && T.last (T.init s) == '.'
                                              then T.init $ T.init s
                                              else T.replace ".0e" "e" s):ys)
      | otherwise = doStrip (n + 1) m
      where isStrNum fun x = not (null . fun . T.unpack $ x !! n)
             && ""== snd (head . fun . T.unpack $ x !! n)

isShort :: Int -> [[a]] -> Bool
isShort n = not . all (\x -> n < length x)

splitDelimiter :: [T.Text] -> [[T.Text]]
splitDelimiter str
  | all (T.any ('\t'==)) str = cells "\t"
  | all (T.any (',' ==)) str = cells ","
  | all (T.any (';' ==)) str = cells ";"
  | all (T.any ('|' ==)) str = cells "|"
  | otherwise                = cells " "
  where cells c = map (map T.strip . T.splitOn c) str
