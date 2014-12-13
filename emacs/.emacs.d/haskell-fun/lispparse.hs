{-# LANGUAGE OverloadedStrings #-}
import qualified Data.AttoLisp as AL
import qualified Data.Attoparsec.ByteString as AP
--import qualified Data.ByteString.UTF8 as UN
import Data.ByteString.Char8 (pack)
import Data.Attoparsec.Number
import Control.Applicative
import Data.Char

main :: IO ()
main = loop ""
  where loop xs = do
         x <- getLine
         if x == "STOP LINE"
            then putStrLn (dispatcher xs) >> loop ""
            else loop $ xs++x

-- i could create a list of fun which patternmatches against the parsed result
fun (AL.List [AL.Symbol "baa"]) = replicate 500 'a'

dispatcher :: String -> String
dispatcher = either (error "DOESN'T PARSE") fun
             . AP.parseOnly AL.lisp . pack -- . UN.fromString
