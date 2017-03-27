module ScratchFileParser where

import MyPrelude hiding (many, readFile, try)

import Data.Char (isAlpha, isDigit)
import System.IO (readFile)

import Types
       (DataLine(..), FinalLine(..), HeaderLine(..),
        exampleFile)

unexpected :: String -> ParsecT m a
unexpected msg =
  ParsecT $ \_ _ _ _ eerr ->
    eerr . ParseError $ "unexpected error: " <> msg

newtype ParseError = ParseError { unParseError :: String }
  deriving (Data, Eq, IsString, Read, Show, Typeable)

newtype ParsecT m a
    = ParsecT {unParser :: forall b .
                 String
              -> (a -> String -> ParseError -> m b) -- consumed ok
              -> (ParseError -> m b)                -- consumed err
              -> (a -> String -> ParseError -> m b) -- empty ok
              -> (ParseError -> m b)                -- empty err
              -> m b
             }
     deriving (Functor, Typeable)

-- | Low-level unpacking of the ParsecT type. To run your parser, please look to
-- runPT, runP, runParserT, runParser and other such functions.
runParsecT
  :: forall m a.
     Monad m
  => ParsecT m a -> String -> m (Consumed (m (Reply a)))
runParsecT p s = unParser p s cok cerr eok eerr
  where
    cok :: a -> String -> ParseError -> m (Consumed (m (Reply a)))
    cok a s' err = return . Consumed . return $ Ok a s' err

    cerr :: ParseError -> m (Consumed (m (Reply a)))
    cerr err = return . Consumed . return $ Error err

    eok :: a -> String -> ParseError -> m (Consumed (m (Reply a)))
    eok a s' err = return . Empty . return $ Ok a s' err

    eerr :: ParseError -> m (Consumed (m (Reply a)))
    eerr err = return . Empty . return $ Error err

parserTest :: Show a => ParsecT IO a -> String -> IO (Either ParseError a)
parserTest p input = do
  consumed <- runParsecT p input
  reply <-
    case consumed of
      Consumed mReply -> do
        putStrLn "Consumed..."
        mReply
      Empty mReply -> do
        putStrLn "Empty..."
        mReply
  case reply of
    res@(Ok a _ _) -> do
      print res
      pure $ Right a
    Error parseErr -> pure $ Left parseErr

-- | Low-level creation of the ParsecT type. You really shouldn't have to do this.
mkPT
  :: Monad m
  => (String -> m (Consumed (m (Reply a))))
  -> ParsecT m a
mkPT k =
  ParsecT $ \s cok cerr eok eerr -> do
    m <- k s
    case m of
      Consumed mrep -> do
        rep <- mrep
        case rep of
          Ok x s' err -> cok x s' err
          Error err -> cerr err
      Empty mrep -> do
        rep <- mrep
        case rep of
          Ok x s' err -> eok x s' err
          Error err -> eerr err

data Consumed a
  = Consumed a
  | Empty !a
  deriving (Eq, Functor, Read, Show, Typeable)

data Reply a
  = Ok a String ParseError
  | Error ParseError
  deriving (Eq, Functor, Read, Show, Typeable)

-- instance Functor (ParsecT s u m) where
--     fmap f p =
--       ParsecT $ \s cok cerr eok eerr ->
--         unParser p s (cok . f) cerr (eok . f) eerr

instance Applicative (ParsecT m) where
  pure :: a -> ParsecT m a
  pure x =
    ParsecT $ \s _ _ eok _ ->
      eok x s $ ParseError "unknown erorr pure"

  (<*>) = ap -- TODO: Can this be optimized?

instance Alternative (ParsecT m) where
  empty = mzero

  (<|>) = mplus

instance Monad (ParsecT m) where
  (>>=) :: ParsecT m a -> (a -> ParsecT m b) -> ParsecT m b
  m >>= k =
    ParsecT $ \s cok cerr eok eerr ->
      let -- consumed-okay case for m
          mcok x s' err =
              let
                  -- if (k x) doesn't consume input, but is okay,
                  -- we still return in the consumed continuation
                  peok y s'' err' = cok y s'' (mergeError err err')

                  -- if (k x) doesn't consume input, but errors,
                  -- we return the error in the 'consumed-error'
                  -- continuation
                  peerr err' = cerr (mergeError err err')
              in  unParser (k x) s' cok cerr peok peerr
          -- empty-ok case for m
          meok x s' err =
              let
                  -- in these cases, (k x) can return as empty
                  pcok = cok
                  peok y s'' err' = eok y s'' (mergeError err err')
                  pcerr = cerr
                  peerr err' = eerr (mergeError err err')
              in  unParser (k x) s' pcok pcerr peok peerr
      in unParser m s
          -- consumed-okay case for m
          mcok
          -- empty-ok case for m
          cerr
          -- consumed-error case for m
          meok
          -- empty-error case for m
          eerr
  {-# INLINE (>>=) #-}


  fail :: String -> ParsecT m a
  fail str =
    ParsecT $ \_ _ _ _ eerr -> eerr $ ParseError str

mergeError :: ParseError -> ParseError -> ParseError
mergeError _ err2 = err2

instance (MonadIO m) => MonadIO (ParsecT m) where
  liftIO = lift . liftIO


instance MonadTrans ParsecT where
  lift :: Monad m => m a -> ParsecT m a
  lift amb =
    ParsecT $ \s _ _ eok _ -> do
      a <- amb
      eok a s $ ParseError "unknown error lift"

instance MonadPlus (ParsecT m) where
  mzero :: ParsecT m a
  mzero =
    ParsecT $ \_ _ _ _ eerr -> eerr $ ParseError "unknown error mzero"

  mplus :: ParsecT m a -> ParsecT m a -> ParsecT m a
  mplus m n =
    ParsecT $ \s cok cerr eok eerr ->
      let
          meerr err =
              let
                  neok y s' err' = eok y s' (mergeError err err')
                  neerr err' = eerr $ mergeError err err'
              in unParser n s cok cerr neok neerr
      in unParser m s cok cerr eok meerr
  {-# INLINE mplus #-}

try :: ParsecT m a -> ParsecT m a
try p =
  ParsecT $ \s cok _ eok eerr ->
    unParser p s cok eerr eok eerr

anyToken :: Applicative m => ParsecT m Char
anyToken = tokenPrim Just

eof :: Applicative m => ParsecT m ()
eof = notFollowedBy anyToken

notFollowedBy
  :: forall m a.
     Applicative m
  => ParsecT m a -> ParsecT m ()
notFollowedBy p = try (f <|> pure ())
  where
    f :: ParsecT m ()
    f = do
      void $ try p
      unexpected "got unexpected successful parse in notFollowedBy"
    {-# INLINE f #-}

unconsStr :: Applicative m => String -> m (Maybe (Char, String))
unconsStr [] = pure Nothing
unconsStr (t:ts) = pure $ Just (t, ts)
{-# INLINE unconsStr #-}

tokenPrimEx
  :: forall m a.
     Applicative m
  => (Char -> Maybe a) -> ParsecT m a
tokenPrimEx test = ParsecT f
  where
    f
      :: forall b.
         String
      -> (a -> String -> ParseError -> m b) -- consumed ok
      -> (ParseError -> m b)                -- consumed err
      -> (a -> String -> ParseError -> m b) -- empty ok
      -> (ParseError -> m b)                -- empty err
      -> m b
    f [] _ _ _ eerr = eerr $ ParseError "unexpected err empty string in tokenPrimEx"
    f (c:cs) cok _ _ eerr =
      case test c of
        Just x ->
          cok x cs $ ParseError "new unknown error in tokenPrimEx"
        Nothing -> eerr $ ParseError "unexpected err weird char in tokenPrimEx"
{-# INLINE tokenPrimEx #-}

token
  :: (Char -> Maybe a)
  -> ParsecT Identity a
token = tokenPrim

tokenPrim
  :: Applicative m
  => (Char -> Maybe a)
  -> ParsecT m a
tokenPrim = tokenPrimEx
{-# INLINE tokenPrim #-}


tokens
  :: forall m.
     Applicative m
  => String -> ParsecT m String
tokens [] =
  ParsecT $ \s _ _ eok _ ->
    eok [] s $ ParseError "tokens unknown error"
tokens tts@(tok:toks) = ParsecT f
  where
    f
      :: forall b.
         String
      -> (String -> String -> ParseError -> m b) -- consumed ok
      -> (ParseError -> m b)                  -- consumed err
      -> (String -> String -> ParseError -> m b) -- empty ok
      -> (ParseError -> m b)                  -- empty err
      -> m b
    f [] _ _ _ eerr = eerr $ ParseError "tokens eof"
    f (x:xs) cok cerr _ eerr
      | tok == x =
        let walk :: String -> String -> m b
            walk []     rs = cok tts rs $ ParseError "unknown in tokens"
            walk (_:_) [] = cerr $ ParseError "tokens eof"
            walk (t:ts) (x':xs')
              | t == x' = walk ts xs'
              | otherwise = cerr . ParseError $ "tokens expecting " <> show x
        in walk toks xs
      | otherwise = eerr . ParseError $ "tokens expecting " <> show x
    {-# INLINE f #-}
{-# INLINE tokens #-}

many :: ParsecT m a -> ParsecT m [a]
many p = do
  xs <- manyAccum (:) p
  return (reverse xs)

skipMany :: ParsecT m a -> ParsecT m ()
skipMany p = do
  void $ manyAccum (\_ _ -> []) p
  return ()

manyAccum :: forall a m. (a -> [a] -> [a]) -> ParsecT m a -> ParsecT m [a]
manyAccum acc p = ParsecT f
  where
    f
      :: forall b.
         String
      -> ([a] -> String -> ParseError -> m b) -- consumed ok
      -> (ParseError -> m b)                  -- consumed err
      -> ([a] -> String -> ParseError -> m b) -- empty ok
      -> (ParseError -> m b)                  -- empty err
      -> m b
    f s cok cerr eok _ =
      let walk :: [a] -> a -> String -> ParseError -> m b
          walk xs x s' _ =
            unParser p s'
              (seq xs $ walk $ acc x xs)  -- consumed-ok
              cerr                        -- consumed-err
              manyErr                     -- empty-ok
              (\e -> cok (acc x xs) s' e) -- empty-err
      in unParser p s (walk []) cerr manyErr (\e -> eok [] s e)
    {-# INLINE f #-}

many1 :: Applicative m => ParsecT m a -> ParsecT m [a]
many1 p = (:) <$> p <*> many p

char :: Applicative m => Char -> ParsecT m Char
char c = satisfy (== c)

satisfy :: Applicative m => (Char -> Bool) -> ParsecT m Char
satisfy f =
  tokenPrim (\c -> if f c then Just c else Nothing)

newline :: Applicative m => ParsecT m ()
newline = void $ char '\n'

letter :: Applicative m => ParsecT m Char
letter = satisfy isAlpha

digit :: Applicative m => ParsecT m Char
digit = satisfy isDigit

int :: forall m. Applicative m => ParsecT m Int
-- int = do
--   maybeInt <- readMay <$> many1 digit
--   case maybeInt of
--     Just i -> pure i
--     Nothing -> throwError $ ParseError "int failed"
int = ParsecT $ f
  where
    f
      :: forall b.
         String
      -> (Int -> String -> ParseError -> m b) -- consumed ok
      -> (ParseError -> m b)                  -- consumed err
      -> (Int -> String -> ParseError -> m b) -- empty ok
      -> (ParseError -> m b)                  -- empty err
      -> m b
    f s cok cerr eok eerr =
      -- TODO: dunno what this undefined should be
      unParser (many1 digit) s lala cerr undefined eerr
      where
        lala :: String -> String -> ParseError -> m b
        lala digitString input parseErr =
          case readMay digitString of
            Just i -> cok i input parseErr
            Nothing -> cerr $ ParseError "unknown error in lala"


string :: Applicative m => String -> ParsecT m String
string = tokens

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

parserHeaderLine :: ParsecT IO HeaderLine
parserHeaderLine = string "00 header" $> HL

parserDataLine :: ParsecT IO DataLine
parserDataLine = do
  void $ string "01 "
  quote <- pack . encodeUtf8 <$> many1 letter
  void $ char ' '
  -- price <- int
  let price = undefined
  pure $ DL {..}

parseFile :: ParsecT IO ()
parseFile = do
  void $ parserHeaderLine <* newline
  void $ parserDataLine <* newline


manyErr :: forall a. a
manyErr = error "Text.ParserCombinators.Parsec.Prim.many: combinator 'many' is applied to a parser that accepts an empty string."

scratchFileParser :: IO ()
scratchFileParser = do
  file <- readFile exampleFile
  consumed <- runParsecT parseFile file
  reply <-
    case consumed of
      Consumed mReply -> mReply
      Empty mReply -> mReply
  case reply of
    Ok () resultString _ -> putStrLn "Successful parse."
    Error parseErr -> print parseErr
