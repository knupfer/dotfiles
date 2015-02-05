{-# LANGUAGE NoImplicitPrelude #-}
module Text ( append
            , breakOn
            , breakOnAll
            , breakOnEnd
            , center
            , chunksOf
            , commonPrefixes
            , concat
            , cons
            , copy
            , count
            , drop
            , dropEnd
            , group
            , head
            , index
            , init
            , inits
            , intercalate
            , intersperse
            , isInfixOf
            , isPrefixOf
            , isSuffixOf
            , justifyLeft
            , justifyRight
            , last
            , length
            , lines
            , maximum
            , minimum
            , null
            , pack
            , replace
            , replicate
            , reverse
            , singleton
            , snoc
            , splitAt
            , splitOn
            , strip
            , stripEnd
            , stripPrefix
            , stripStart
            , stripSuffix
            , tail
            , tails
            , take
            , takeEnd
            , toCaseFold
            , toLower
            , toTitle
            , toUpper
            , transpose
            , uncons
            , unlines
            , unpack
            , unwords
            , words
            , zip) where

import           Data.Text (Text)
import qualified Data.Text as T
import           Prelude   (Bool, Char, Int, Maybe, String, uncurry)

append :: (Text, Text) -> Text
append = uncurry T.append

breakOn :: (Text, Text) -> (Text, Text)
breakOn = uncurry T.breakOn

breakOnAll :: (Text, Text) -> [(Text, Text)]
breakOnAll = uncurry T.breakOnAll

breakOnEnd :: (Text, Text) -> (Text, Text)
breakOnEnd = uncurry T.breakOnEnd

center :: (Int, Char, Text) -> Text
center (i,c,t) = T.center i c t

chunksOf :: (Int, Text) -> [Text]
chunksOf = uncurry T.chunksOf

commonPrefixes :: (Text, Text) -> Maybe (Text, Text, Text)
commonPrefixes = uncurry T.commonPrefixes

concat :: [Text] -> Text
concat = T.concat

cons :: (Char, Text) -> Text
cons = uncurry T.cons

copy :: Text -> Text
copy = T.copy

count :: (Text, Text) -> Int
count = uncurry T.count

drop :: (Int, Text) -> Text
drop = uncurry T.drop

dropEnd :: (Int, Text) -> Text
dropEnd = uncurry T.dropEnd

group :: Text -> [Text]
group = T.group

head :: Text -> Char
head = T.head

index :: (Text, Int) -> Char
index = uncurry T.index

init :: Text -> Text
init = T.init

inits :: Text -> [Text]
inits = T.inits

intercalate :: (Text, [Text]) -> Text
intercalate = uncurry T.intercalate

intersperse :: (Char, Text) -> Text
intersperse = uncurry T.intersperse

isInfixOf :: (Text, Text) -> Bool
isInfixOf = uncurry T.isInfixOf

isPrefixOf :: (Text, Text) -> Bool
isPrefixOf = uncurry T.isPrefixOf

isSuffixOf :: (Text, Text) -> Bool
isSuffixOf = uncurry T.isSuffixOf

justifyLeft :: (Int, Char, Text) -> Text
justifyLeft (i,c,t) = T.justifyLeft i c t

justifyRight :: (Int, Char, Text) -> Text
justifyRight (i,c,t) = T.justifyRight i c t

last :: Text -> Char
last = T.last

length :: Text -> Int
length = T.length

lines :: Text -> [Text]
lines = T.lines

maximum :: Text -> Char
maximum = T.maximum

minimum :: Text -> Char
minimum = T.minimum

null :: Text -> Bool
null = T.null

pack :: String -> Text
pack = T.pack

replace :: (Text, Text, Text) -> Text
replace (t1,t2,t3) = T.replace t1 t2 t3

replicate :: (Int, Text) -> Text
replicate = uncurry T.replicate

reverse :: Text -> Text
reverse = T.reverse

singleton :: Char -> Text
singleton = T.singleton

snoc :: (Text, Char) -> Text
snoc = uncurry T.snoc

splitAt :: (Int, Text) -> (Text, Text)
splitAt = uncurry T.splitAt

splitOn :: (Text, Text) -> [Text]
splitOn = uncurry T.splitOn

strip :: Text -> Text
strip = T.strip

stripEnd :: Text -> Text
stripEnd = T.stripEnd

stripPrefix :: (Text, Text) -> Maybe Text
stripPrefix = uncurry T.stripPrefix

stripStart :: Text -> Text
stripStart = T.stripStart

stripSuffix :: (Text, Text) -> Maybe Text
stripSuffix = uncurry T.stripSuffix

tail :: Text -> Text
tail = T.tail

tails :: Text -> [Text]
tails = T.tails

take :: (Int, Text) -> Text
take = uncurry T.take

takeEnd :: (Int, Text) -> Text
takeEnd = uncurry T.takeEnd

toCaseFold :: Text -> Text
toCaseFold = T.toCaseFold

toLower :: Text -> Text
toLower = T.toLower

toTitle :: Text -> Text
toTitle = T.toTitle

toUpper :: Text -> Text
toUpper = T.toUpper

transpose :: [Text] -> [Text]
transpose = T.transpose

uncons :: Text -> Maybe (Char, Text)
uncons = T.uncons

unlines :: [Text] -> Text
unlines = T.unlines

unpack :: Text -> String
unpack = T.unpack

unwords :: [Text] -> Text
unwords = T.unwords

words :: Text -> [Text]
words = T.words

zip :: (Text, Text) -> [(Char, Char)]
zip = uncurry T.zip
