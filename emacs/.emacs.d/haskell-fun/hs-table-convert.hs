{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative
import           Data.Attoparsec.Text
import           Data.List
import qualified Data.Text            as T
import qualified Data.Text.IO         as T (interact)

main :: IO ()
main = T.interact parseCsv

parseCsv :: T.Text -> T.Text
parseCsv = T.unlines   . putPipes          -- print result
         . map head    . group . transpose -- remove duplicates
         . align       . stripNum          -- prettify numbers
         . filter (not . all T.null)       -- remove empty cols
         . transpose   . splitDelimiter    -- transpose matrix
         . filter (not . T.null) . T.lines -- remove empty rows

putPipes :: [[T.Text]] -> [T.Text]
putPipes = map (\t -> T.unwords $ "|":intersperse "|" t ++ ["|"])

align :: [[T.Text]] -> [[T.Text]]
align = map (\n -> pad (maximum $ map T.length n) n)
  where pad lim = map (T.justifyRight lim ' ')

parseNum :: Parser T.Text
parseNum = ((\x -> if T.isSuffixOf ".0" x || T.isInfixOf ".0e" x
                    then T.replace ".0" "" x
                    else x) . T.pack . show)
           <$> scientific <* endOfInput

stripNum :: [[T.Text]] -> [[T.Text]]
stripNum = filter (any ("0"/=))
         . map (map (\x -> either (const x) id $ parseOnly parseNum x))

splitDelimiter :: [T.Text] -> [[T.Text]]
splitDelimiter s = maybe (error "Can't find a delimiter.") cells delim
  where cells c  = map (map T.strip . T.split (c==)) s
        delim    = find (\c -> all (T.any (c==)) s) "\t,;| "
