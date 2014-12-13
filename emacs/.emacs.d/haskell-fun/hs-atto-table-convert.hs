{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text.IO as T (interact)
import qualified Data.List as L
import Data.Text (Text)
import qualified Data.Text as T
import Data.Attoparsec.Text as AT
import Control.Applicative
import Control.Monad

delim :: Parser ()
delim = void $ many (char ' ')
        >> AT.satisfy (inClass " \t;|,")
        >> many (char ' ')

parseRow :: Parser [Text]
parseRow = do
  x  <- cell
  xs <- many $ delim *> cell
  return $ x:xs

cell :: Parser Text
cell = (T.pack . show <$> scientific) <|>
  T.strip <$> AT.takeWhile (notInClass "\t;|,\n")

parseCsv :: Parser [[Text]]
parseCsv = many $ parseRow <* endOfLine

getCells :: Text -> [[Text]]
getCells str = either error id $ parseOnly parseCsv str


prettify :: [[Text]] -> Text
prettify = T.unlines
    . map (T.unwords . (\x -> "|" : x ++ ["|"]) . L.intersperse "|")
    . L.transpose . cleanUp . align . L.transpose
    . map (map (\x -> if T.isSuffixOf ".0" x
                         then T.init $ T.init x
                         else x))

cleanUp :: [[Text]] -> [[Text]]
cleanUp = filter (not . all T.null)

align :: [[Text]] -> [[Text]]
align = map (\n -> pad n (maximum $ map T.length n))
  where pad n lim = map (T.justifyRight lim ' ') n

main :: IO ()
main = T.interact $ prettify . getCells
