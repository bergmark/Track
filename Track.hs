module Main (
    module Control.Applicative
  , module Control.Arrow
  , module Control.Monad
  , module Text.ParserCombinators.Parsec
  , module Directory
  , module System.Environment
  , module Data.Maybe
  , module Main
  ) where

import Control.Applicative hiding (optional)
import Control.Monad (liftM)
import Text.ParserCombinators.Parsec hiding (many, (<|>))
import Directory (getDirectoryContents, getCurrentDirectory, renameFile)
import System.Environment (getArgs)
import Control.Arrow
import Data.Maybe

data Track = Track { _cd :: Maybe Int, _no :: Int, _artist :: Maybe String, _title :: String, _ext :: String }
             deriving Show

hasCd :: Track -> Bool
hasCd = isJust . _cd
hasArtist :: Track -> Bool
hasArtist = isJust . _artist

condStr :: (Track -> Bool) -> (Track -> String) -> Track -> String
condStr p f t = if p t then f t else ""

trackToFp :: Track -> FilePath
trackToFp t = condStr hasCd ((++ "-") . show . fromJust . _cd) t
              ++ (pad . show . _no $ t)
              ++ " - "
              ++ condStr hasArtist ((++ " - ") . fromJust . _artist) t
              ++ _title t
              ++ _ext t
                 where
                   pad d = if length d == 1 then "0" ++ d else d

-- Returns Nothing if p fails no matter if it consumes input or not.
myTry :: CharParser st a -> CharParser st (Maybe a)
myTry p = try (optionMaybe p) <|> pure Nothing

p_cd :: CharParser st (Maybe Int)
p_cd = myTry $ read <$> many1 digit <* char '-' <* lookAhead digit

p_no :: CharParser st Int
p_no = read <$> many digit

p_cd_no :: CharParser st (Maybe Int, Int)
p_cd_no = do
  cd <- p_cd
  no <- p_no
  return (cd, no)

anystring :: CharParser st String
anystring = many1 anyToken

p_artist :: CharParser st (Maybe String)
p_artist = myTry $ anystring <* string " - " <* lookAhead anyChar

p_title :: CharParser st String
p_title = anyChar `manyTill` lookAhead p_ext

p_ext :: CharParser st String
p_ext = string ".mp3"

p_track :: CharParser st Track
p_track = do
  (cd, no) <- p_cd_no
  many1 (oneOf (" -.")) >> return ()
  artist <- p_artist
  title <- p_title
  ext <- p_ext
  eof
  return $ Track { _cd = cd, _no = no, _artist = artist, _title = title, _ext = ext }

parse'' :: CharParser () a -> String -> Either ParseError a
parse'' p s = parse p s s

parse' :: String -> Either ParseError Track
parse' fp = parse'' p_track fp

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = either (const Nothing) Just

partit :: (a -> Bool) -> [a] -> ([a],[a])
partit f = foldr (\a -> (if f a then first else second) (a:)) ([],[])

main :: IO ()
main = do
  args <- getArgs
  wd <- getCurrentDirectory
  let dirs = if null args then [wd] else args
  files <- concat <$> mapM getDirectoryContents dirs
  let parses = zip files . map (eitherToMaybe . parse') $ files
  let (nontracks,tracks) = (map fst *** map (fromJust . snd)) . partit (isNothing . snd) $ parses
  mapM_ putStrLn . filter (\x -> x /= "." && x /= "..") $ nontracks
  putStrLn ""
  mapM_ print tracks
  mapM_ (putStrLn . trackToFp) tracks
