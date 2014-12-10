{-# LANGUAGE OverloadedStrings #-}
import           Data.List (intersperse, transpose, foldl')
import qualified Data.Text    as T
import qualified Data.Text.IO as T (interact)
import           Numeric (readFloat, readDec, readSigned, showGFloat)

main :: IO ()
main = T.interact parseCSV

parseCSV :: T.Text -> T.Text
parseCSV = T.unlines .
           init .
           foldl' (\acc x -> if x/= head acc then x:acc else acc) [""] .
           putPipes .
           transpose .
           align .
           stripNum .
           stripEmpty .
           transpose .
           splitDelimiter .
           filter (not . T.null) .
           foldl' (\acc x -> if x/= head acc then x:acc else acc) [""] .
           T.lines

putPipes :: [[T.Text]] -> [T.Text]
putPipes = map (\t -> T.strip $ T.unwords $ intersperse "|" $ "":t ++ [""])

stripEmpty :: [[T.Text]] -> [[T.Text]]
stripEmpty = filter (not . all T.null)

align :: [[T.Text]] -> [[T.Text]]
align = map (\n -> pad n (maximum $ map T.length n))
  where pad n lim = map (T.justifyRight lim ' ') n

stripNum :: [[T.Text]] -> [[T.Text]]
stripNum = map doStrip
  where
    doStrip m
      | all (isStrNum (readSigned readDec :: String -> [(Int,String)])) m
             = m
      | all (isStrNum (readSigned readFloat :: String -> [(Float,String)])) m
             = map (remNull . (\n -> showGFloat (Just 3) n "")
                 . fst . head . (readSigned readFloat :: String -> [(Float,String)]) . T.unpack) m
      | otherwise = m
      where isStrNum fun x = not (null . fun . T.unpack $ x)
             && ""== snd (head . fun . T.unpack $ x)
            remNull str = let s = T.pack $ show (read str :: Double)
                          in if T.isSuffixOf ".0" s
                                then T.init $ T.init s
                                else T.replace ".0e" "e" s

splitDelimiter :: [T.Text] -> [[T.Text]]
splitDelimiter str
  | all (T.any ('\t'==)) str = cells "\t"
  | all (T.any (',' ==)) str = cells ","
  | all (T.any (';' ==)) str = cells ";"
  | all (T.any ('|' ==)) str = cells "|"
  | otherwise                = cells " "
  where cells c = map (map T.strip . T.splitOn c) str
