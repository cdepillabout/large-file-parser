module MonadTransFileParser where

import MyPrelude hiding (many, readFile)

import System.IO (readFile)

import Types
       (DataLine(..), FinalLine(..), HeaderLine(..),
        exampleFile)

manyLength :: forall a . Parser a -> Parser (Int, [a])
manyLength p = go 0 []
  where
    go :: Int -> [a] -> Parser (Int, [a])
    go i as = (p >>= \a -> go (i + 1) (a:as)) `orParse` pure (i, as)

manyLength_ :: forall a . Int -> Parser a -> Parser Int
-- manyLength_ n p = (p >> manyLength (n + 1)) `orParse` pure n
manyLength_ n (Parser exceptT) = do
  s <- get
  let stateTEitherA = runExceptT exceptT
  (eitherA, newS) <- liftIO $ runStateT stateTEitherA s
  put newS
  case eitherA of
    Left parseErr -> pure n
    Right _ -> manyLength_ (n + 1) (Parser exceptT)


many :: Parser a -> Parser [a]
many parser = do
  maybeA <- catchError (fmap Just parser) (const $ pure Nothing)
  case maybeA of
    Just a -> (a :) <$> many parser
    Nothing -> pure []

many1 :: Parser a -> Parser [a]
many1 parser = (:) <$> parser <*> many parser

newline :: Parser ()
newline = void $ char '\n'

char :: Char -> Parser Char
char c = do
  eitherChar <- state f
  case eitherChar of
    Right _ -> pure c
    Left parseError -> throwError parseError
  where
    f :: String -> (Either ParseError Char, String)
    f str@(firstChar:otherChars)
      | firstChar == c = (Right c, otherChars)
      | otherwise = (Left $ ParseError "char failed", str)
    f str = (Left $ ParseError "char failed on empty string", str)

string :: String -> Parser String
string = traverse char

orParse :: Parser a -> Parser a -> Parser a
orParse parserA parserB = catchError parserA (const parserB)

letter :: Parser Char
letter = char 'a' `orParse` char 'd' `orParse` char 't'

digit :: Parser Char
digit =
  char '0' `orParse`
  char '1' `orParse`
  char '2' `orParse`
  char '3' `orParse`
  char '4' `orParse`
  char '5' `orParse`
  char '6' `orParse`
  char '7' `orParse`
  char '8' `orParse`
  char '9'

int :: Parser Int
int = do
  maybeInt <- readMay <$> many1 digit
  case maybeInt of
    Just i -> pure i
    Nothing -> throwError $ ParseError "int failed"

parserHeaderLine :: Parser HeaderLine
parserHeaderLine = string "00 header" $> HL

parserDataLine :: Parser DataLine
parserDataLine = do
  void $ string "01 "
  quote <- pack . encodeUtf8 <$> many1 letter
  void $ char ' '
  price <- int
  pure $ DL {..}

parserFinalLine :: Parser FinalLine
parserFinalLine = string "99 final " *> fmap FL int

parserFile :: Parser ()
parserFile = do
  void $ parserHeaderLine <* newline
  len <- manyLength_ 0 (parserDataLine <* newline)
  (FL finalLen) <- parserFinalLine <* newline
  when (len /= finalLen)
    (throwError $ ParseError "input lines are not equal to final line length")

data ParseError = ParseError String

newtype Parser a = Parser
  { unParser :: ExceptT ParseError (StateT String IO) a
  } deriving ( Applicative
             , Functor
             , Monad
             , MonadError ParseError
             , MonadState String
             , MonadIO
             )

runParser :: Parser () -> String -> IO ()
runParser (Parser exceptT) inputString = do
  (eitherA, resString) <- runStateT (runExceptT exceptT) inputString
  case eitherA of
    Left (ParseError parseErr) ->
      putStrLn $ "Got parser error: " <> pack parseErr
    Right _ -> putStrLn $ "Successfully parsed."

monadTransFileParser :: IO ()
monadTransFileParser = do
  file <- readFile exampleFile
  runParser parserFile file
