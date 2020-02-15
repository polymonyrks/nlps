module Main where

import Prelude as P
import Lib
import qualified Data.Text as Text
import           Data.List
import Turtle
import qualified Control.Foldl as Fold
import Text.Show.Unicode
import           System.IO
import           Data.Time

tes = do
  lis <- listFromFileJP "./data/sample.txt"
  let
    isNoun :: String -> Bool
    isNoun = isInfixOf "名詞"
    filterNoun :: [String] -> [String]
    filterNoun = filter isNoun
  putStrLn ""

main :: IO ()
main = putStrLn ""

testWord :: String
testWord = "これはテストです"

getMecabed :: String -> IO [String]
getMecabed sens = do
  let
    cmd = fromString $ "echo " P.++ "\""  P.++ sens P.++ "\"" P.++ " | mecab"
    --cmd = fromString $ "echo " P.++ sens P.++ " | mecab"
  getShellRes cmd

getMecabedShow :: String -> IO [()]
getMecabedShow sens = do
  mecabed <- getMecabed sens
  cshowUI mecabed

getShellRes :: String -> IO [String]
getShellRes cmd = do
  let
    get2 :: (MonadIO m) => Text -> m [Line]
    get2 cmd =
      fold (inshell cmd Turtle.empty) Fold.list
    texized = Text.pack cmd
  fmap (Text.unpack . lineToText) <$> get2 texized

takeFst :: [(a, b)] -> [a]
takeFst = map fst

takeSnd :: [(a, b)] -> [b]
takeSnd = map snd

takeFstT :: [(a, b, c)] -> [a]
takeFstT = map f
    where
      f (a, b, c) = a

takeSndT :: [(a, b, c)] -> [b]
takeSndT = map f
    where
      f (a, b, c) = b

takeThdT :: [(a, b, c)] -> [c]
takeThdT = map f
    where
      f (a, b, c) = c

fstT :: (a, b, c) -> a
fstT (x, y, z) = x

sndT :: (a, b, c) -> b
sndT (x, y, z) = y

thdT :: (a, b, c) -> c
thdT (x, y, z) = z

listFromFile :: String -> IO [String]
listFromFile listPath = do
  handle <- openFile listPath ReadMode
  fText <- hGetContents handle
  let forout = lines fText
  return forout

listFromFileJP :: String -> IO [String]
listFromFileJP listPath = do
  handle <- openFile listPath ReadMode
  hSetEncoding handle utf8
  fText <- hGetContents handle
  let forout = lines fText
  return forout

listToFileJP :: Show a => [a] -> String -> IO ()
listToFileJP vec fpath = do
  handle <- openFile fpath WriteMode
  hSetEncoding handle utf8
  mapM_ (hPutStrLn handle . ushow) vec
  hClose handle

strToFileJP :: String -> String -> IO ()
strToFileJP str fpath = do
  handle <- openFile fpath WriteMode
  hSetEncoding handle utf8
  hPutStr handle str
  hClose handle

indexing :: [a] -> [(Int, a)]
indexing as = zip [0 .. (length as - 1)] as

cshow :: Show a => [a] -> IO [()]
cshow = mapM print

cshowI :: Show a => [a] -> IO [()]
cshowI = cshow . indexing

cshowR :: Show a => [a] -> IO [()]
cshowR vs = cshow $ reverse $ indexing vs

cshowS :: Show a => [a] -> Int -> Int-> IO [()]
cshowS vs n m = cshow forShow
  where
    indexed = indexing vs
    forShow = map (\n -> indexed !! n) [n .. m]

cshowU :: Show a => [a] -> IO [()]
cshowU = mapM (putStrLn . ushow)

cshowUI :: Show a => [a] -> IO [()]
cshowUI vs = cshowU $ indexing vs

showMecabRes :: String -> IO ()
showMecabRes str = do
  res <- getMecabed str
  uprint res

take2 = take 2
drop3 = drop 3

divideAtTab :: String -> (String, String)
divideAtTab s = (take2 s, drop3 s)

isNotTab :: Char -> Bool
isNotTab c = c /= '\t'

isNotSpecChar :: Char -> Char -> Bool
isNotSpecChar c c1 = c /= c1
divideAt :: Char -> String -> (String, String)
divideAt c s = (takeWhile (isNotSpecChar c) s, tail (dropWhile (isNotSpecChar c) s))

ss1 = ['今', '日', '\t', '名', '詞', ',', '副', '詞', '可', '能']

ss1left' = takeWhile isNotTab2 ss1
  where
      isNotTab2 c = c /= '\t'

getTokenTag :: String -> (String, String)
getTokenTag str = (token, tag)
  where
    token = takeWhile (\c -> c /= '\t') str
    tagPrim
     | dropped == "" = ""
     | otherwise = tail dropped
      where
         dropped = (dropWhile (\c -> c /= '\t') str)
    tag = takeWhile (\c -> c /= ',') tagPrim
