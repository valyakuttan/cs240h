--
--
{-# LANGUAGE OverloadedStrings #-}

module Parse where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8

import Data.Attoparsec as P
import Data.Attoparsec.Char8 (char8, endOfLine)
import qualified Data.Attoparsec.Char8 as P8

import Control.Applicative hiding (many)
import Data.Word (Word8)
import Data.Char

data Request = Request {
      requestMethod  :: ByteString
    , requestUri     :: ByteString
    , requestVersion :: ByteString
    } deriving (Eq, Ord, Show)

requestLine :: Parser Request
requestLine = do
  method <- P.takeWhile1 isToken <* char8 ' '
  cheesyUri <- P8.takeWhile1 (/=' ') <* char8 ' '
  ver <- version <* endOfLine
  return (Request method cheesyUri ver)

isToken = undefined

version :: Parser ByteString
version = string "HTTP/" >>
          P8.takeWhile (\c -> isDigit c || c == '.')

betterVersion :: Parser (Int, Int)
betterVersion = string "HTTP/" *>
                ((,) <$> (P8.decimal <* char8 '.') <*> P8.decimal)

responseLine :: Parser ((Int,Int,Int), ByteString)
responseLine =
    (,) <$> (betterVersion *> sp *> xxx <* sp) 
        <*> P.takeTill P8.isEndOfLine <* P8.endOfLine
  where xxx = (,,) <$> x <*> x <*> x
        x = digitToInt <$> P8.digit
        sp = char8 ' '
