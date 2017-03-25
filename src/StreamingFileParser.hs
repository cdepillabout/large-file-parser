module StreamingFileParser where

import MyPrelude

import Data.Attoparsec.ByteString.Char8
       (Parser, decimal, endOfLine, isAlpha_ascii, skipSpace, string,
        takeWhile1)
import Data.Conduit.Attoparsec (conduitParser)

import Types
       (DataLine(..), FinalLine(..), HeaderLine(..), Line(..),
        exampleFile)

parserLine :: Parser Line
parserLine =
  (fmap HeaderLine parserHeaderLine <|>
  fmap DataLine parserDataLine <|>
  fmap FinalLine parserFinalLine) <* endOfLine

parserHeaderLine :: Parser HeaderLine
parserHeaderLine = string "00 header" $> HL

parserDataLine :: Parser DataLine
parserDataLine = do
  void $ string "01 "
  quote <- takeWhile1 isAlpha_ascii
  skipSpace
  price <- decimal
  pure $ DL {..}

parserFinalLine :: Parser FinalLine
parserFinalLine = string "99 final " *> fmap FL decimal

sink
  :: MonadIO m
  => Sink a m ()
sink = pure ()

streamingFileParser
  :: (MonadBaseControl IO m, MonadIO m, MonadThrow m)
  => m ()
streamingFileParser =
  runResourceT $ sourceFile exampleFile =$= conduitParser parserLine $$ sink
