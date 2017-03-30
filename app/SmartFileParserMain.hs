{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import MyPrelude hiding (readFile)

import System.IO (readFile)
import Text.Parsec
       (Consumed(..), ParseError, ParsecT, Reply(..), State(..), char,
        digit, letter, many1, mkPT, newline, runParsecT, runParserT,
        string)

import Types
       (DataLine(..), FinalLine(..), HeaderLine(..), exampleFile)

-- manyLength :: forall a . Parser a -> Parser (Int, [a])
-- manyLength p = go 0 []
--   where
--     go :: Int -> [a] -> Parser (Int, [a])
--     go i as = (p >>= \a -> go (i + 1) (a:as)) <|> pure (i, as)

-- manyLength :: forall a . Parser a -> Parser Int
-- manyLength p = go 0
--   where
--     go :: Int -> Parser Int
--     go !i = (p *> go (i + 1)) <|> pure i

manyLength :: forall a . Parser a -> Parser Int
manyLength p = go 0
  where
    go :: Int -> Parser Int
    -- go !i = (p *> go (i + 1)) <|> pure i
    go !i =
      (p *> pure True) <|> pure False >>=
        \success -> if success then go (i+1) else pure i

manyLengthInline :: forall s u m a. Monad m => ParsecT s u m a -> ParsecT s u m Int
manyLengthInline p = go 0
  where
    go :: Int -> ParsecT s u m Int
    -- go !i =
    --   (p *> pure True) <|> pure False >>=
    --     \success -> if success then go (i+1) else pure i
    -- go !i =
    --   (>>=)
    --     ((p *> pure True) <|> pure False)
    --     (\success -> if success then go (i+1) else pure i)
    go !i =
      ParsecT $ \s cok cerr eok eerr ->
        let mcok :: Bool -> State s u -> ParserError -> m b
            mcok success s' err =
                let peok :: Int -> State s u -> ParserError -> m b
                    peok int s'' err' = cok int s'' (mergeError err err')
                    peerr :: ParserError -> m b
                    peerr err' = cerr (mergeError err err')
                in unParser
                    (if success then go (i + 1) else pure i)
                    s'
                    cok
                    cerr
                    peok
                    peerr
            meok :: Bool -> State s u -> ParserError -> m b
            meok success s' err =
                let peok :: Int -> State s u -> ParserError -> m b
                    peok int s'' err' = eok int s'' (mergeError err err')
                    peerr :: ParserError -> m b
                    peerr err' = eerr (mergeError err err')
                in unParser
                    (if success then go (i + 1) else pure i)
                    s'
                    cok
                    pcerr
                    peok
                    peerr
        in unParser ((p *> pure True) <|> pure False) s mcok cerr meok eerr

manyLength :: forall s u m a. Monad m => ParsecT s u m a -> ParsecT s u m Int
manyLength p = go 0
  where
    go :: Int -> ParsecT s u m Int
    -- go !i = (p *> go (i + 1)) <|> pure i
    go !i =
      ParsecT $ \s cok cerr eok eerr ->
        let meerr :: ParserError -> m b
            meerr err =
              let neok :: Int -> State s u -> ParserError -> m b
                  neok y s' err' = eok y s' (mergeError err err')
                  neerr :: ParserError -> m b
                  neerr err' = eerr $ mergeError err err'
              in unParser (pure i) s cok cerr neok neerr
        in unParser (p *> go (i + 1)) s cok cerr eok meerr

type Parser = ParsecT String () IO

-- manyNoAccumLength :: forall a m. ParsecT m a -> ParsecT m Int
-- manyNoAccumLength p = ParsecT f
--   where
--     f
--       :: forall b.
--          String
--       -> (Int -> String -> ParseError -> m b) -- consumed ok
--       -> (ParseError -> m b)                  -- consumed err
--       -> (Int -> String -> ParseError -> m b) -- empty ok
--       -> (ParseError -> m b)                  -- empty err
--       -> m b
--     f s cok cerr eok _ =
--       let walk :: Int -> a -> String -> ParseError -> m b
--           walk !i _ s' _ =
--             unParser p s'
--               (walk $ i + 1)              -- consumed-ok
--               cerr                        -- consumed-err
--               manyErr                     -- empty-ok
--               (\e -> cok (i + 1) s' e)    -- empty-err
--       in unParser p s (walk 0) cerr manyErr (\e -> eok 0 s e)
--     {-# INLINE f #-}

-- manyLength :: forall a. Parser a -> Parser Int
-- manyLength p = {-# SCC "manyLength" #-} mkPT f
--   where
--     f :: State String () -> IO (Consumed (IO (Reply String () Int)))
--     f parseState = do
--       consumed <- runParsecT p parseState
--       case consumed of
--         Empty ioReply -> do
--           reply <- ioReply
--           case reply of
--             Ok _ _ _ -> manyLengthErr
--             Error parseErr -> pure . Empty . pure $ Ok 0 parseState parseErr
--         Consumed ioReply -> do
--           reply <- ioReply
--           case reply of
--             Ok a newState parseErr -> walk 0 a newState parseErr
--             Error parseErr -> pure . Consumed . pure $ Error parseErr
--       where
--         walk
--           :: Int
--           -> a
--           -> State String ()
--           -> ParseError
--           -> IO (Consumed (IO (Reply String () Int)))
--         walk !i _ parseState' _ = do
--           consumed <- runParsecT p parseState'
--           case consumed of
--             Empty ioReply -> do
--               reply <- ioReply
--               case reply of
--                 Ok _ _ _ -> manyLengthErr
--                 Error parseErr -> pure . Consumed . pure $ Ok (i + 1) parseState' parseErr
--             Consumed ioReply -> do
--               reply <- ioReply
--               case reply of
--                 Ok a newState parseErr -> walk (i + 1) a newState parseErr
--                 Error parseErr -> pure . Consumed . pure $ Error parseErr

manyLengthErr :: Monad m => m a
manyLengthErr = fail "manyLength can't be used on a parser that accepts empty input"

parserHeaderLine :: Parser HeaderLine
parserHeaderLine = string "00 header" $> HL

parserDataLine :: Parser DataLine
parserDataLine = do
  void $ string "01 "
  quote <- pack . encodeUtf8 <$> many1 letter
  void $ char ' '
  price <- parserInt
  pure $ DL {..}

parserFinalLine :: Parser FinalLine
parserFinalLine = string "99 final " *> fmap FL parserInt

parserInt :: Parser Int
parserInt = do
  maybeInt <- readMay <$> many1 digit
  maybe (fail "Cannot read integer") pure maybeInt

parserFile :: Parser ()
parserFile = do
  void $ parserHeaderLine <* newline
  len <- manyLength (parserDataLine <* newline)
  (FL finalLen) <- parserFinalLine
  when (len /= finalLen) $
    fail "length of the data lines do not equal final line length"

smartFileParser1 :: IO ()
smartFileParser1 = do
  file <- readFile exampleFile
  eitherRes <- runParserT parserFile () exampleFile file
  case eitherRes of
    Left parserErr -> putStrLn $ "Got parser error: " <> tshow parserErr
    Right _ -> putStrLn "Successfully parsed."

main :: IO ()
main = smartFileParser1
