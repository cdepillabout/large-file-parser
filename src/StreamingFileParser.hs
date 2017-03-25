module StreamingFileParser where

import MyPrelude hiding (foldl, foldM)

import Data.Attoparsec.ByteString.Char8
       (Parser, decimal, endOfLine, isAlpha_ascii, skipSpace, string,
        takeWhile1)
import Data.Conduit.Attoparsec (conduitParser)
import Data.Conduit.Combinators (foldl, foldM)

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

-- XXX: This takes up a lot of memory but is fast.

-- sink
--   :: MonadIO m
--   => Int -> Sink (a, Line) m ()
-- sink lineNum = do
--   res <- await
--   case res of
--     Just (_, HeaderLine HL) -> sink lineNum
--     Just (_, FinalLine (FL finalLen)) ->
--       if lineNum /= finalLen
--         then fail "length of the data lines do not equal final line length"
--         else putStrLn ("Successfully parsed " <> tshow lineNum <> " lines.")
--     Just (_, resLine@DataLine{}) -> do
--       -- putStrLn (tshow resLine)
--       sink $ lineNum + 1
--     Nothing -> fail "Never got the final line..."

data StreamResult
  = Beginning
  | Cont Int
  | Succeed
  | ErrLengthNotEqual
  | ErrFileIsBad
  deriving (Data, Eq, Read, Show, Typeable)

-- XXX: This doesn't take up any memory but is slow.

-- sink
--   :: forall m x.
--      MonadIO m
--   => Sink (x, Line) m StreamResult
-- sink = foldM f Beginning
--   where
--     f :: StreamResult -> (x, Line) -> m StreamResult
--     f Beginning      (_, HeaderLine HL)           = pure $ Cont 0
--     f (Cont lineNum) (_, DataLine{})              = pure $ Cont (lineNum + 1)
--     f (Cont lineNum) (_, FinalLine (FL finalLen)) =
--       if lineNum /= finalLen
--         then pure ErrLengthNotEqual
--         else pure Succeed
--     f _              _                            = pure ErrFileIsBad

sink
  :: forall m x.
     MonadIO m
  => Sink (x, Line) m StreamResult
sink = foldl f Beginning
  where
    f :: StreamResult -> (x, Line) -> StreamResult
    f Beginning      (_, HeaderLine HL)           = Cont 0
    f (Cont lineNum) (_, DataLine{})              = Cont (lineNum + 1)
    f (Cont lineNum) (_, FinalLine (FL finalLen)) =
      if lineNum /= finalLen
        then ErrLengthNotEqual
        else Succeed
    f _              _                            = ErrFileIsBad

streamingFileParser
  :: (MonadBaseControl IO m, MonadIO m, MonadThrow m)
  => m ()
streamingFileParser = do
  streamRes <-
    runResourceT $ sourceFile exampleFile =$= conduitParser parserLine $$ sink
  print streamRes
