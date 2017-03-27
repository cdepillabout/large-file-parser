module ScratchFileParser where

import MyPrelude hiding (readFile)

import System.IO (readFile)

import Types
       (DataLine(..), FinalLine(..), HeaderLine(..),
        exampleFile)

-- unexpected :: (Stream s m t) => String -> ParsecT s u m a
-- unexpected msg
--     = ParsecT $ \s _ _ _ eerr ->
--       eerr $ newErrorMessage (UnExpect msg) (statePos s)

newtype ParseError = ParseError { unParseError :: String }
  deriving (Data, Eq, IsString, Read, Show, Typeable)

newtype ParsecT m a
    = ParsecT {unParser :: forall b .
                 String
              -> (a -> String -> ParseError -> m b) -- consumed ok
              -> (ParseError -> m b)                   -- consumed err
              -> (a -> String -> ParseError -> m b) -- empty ok
              -> (ParseError -> m b)                   -- empty err
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
  deriving (Functor, Typeable)

data Reply a
  = Ok a String ParseError
  | Error ParseError
  deriving (Functor, Typeable)

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
                  -- if (k x) consumes, those go straigt up
                  pcok = cok
                  pcerr = cerr

                  -- if (k x) doesn't consume input, but is okay,
                  -- we still return in the consumed continuation
                  peok y s'' err' = cok y s'' (mergeError err err')

                  -- if (k x) doesn't consume input, but errors,
                  -- we return the error in the 'consumed-error'
                  -- continuation
                  peerr err' = cerr (mergeError err err')
              in  unParser (k x) s' pcok pcerr peok peerr
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
mergeError = const

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

unconsStr :: Applicative m => String -> m (Maybe (Char, String))
unconsStr [] = pure Nothing
unconsStr (t:ts) = pure $ Just (t, ts)
{-# INLINE unconsStr #-}

tokenPrimEx
  :: Monad m
  => (Char -> Maybe a) -> ParsecT m a
tokenPrimEx test =
  ParsecT $ \s cok _ _ eerr -> do
    r <- unconsStr s
    case r of
      Nothing -> eerr $ ParseError "unexpected err empty string in tokenPrimEx"
      Just (c,cs) ->
        case test c of
          Just x ->
            cok x cs $ ParseError "new unknown error in tokenPrimEx"
          Nothing -> eerr $ ParseError "unexpected err weird char in tokenPrimEx"
{-# INLINE tokenPrimEx #-}

scratchFileParser :: IO ()
scratchFileParser = do
  file <- readFile exampleFile
  -- runParser parserFile file
  undefined
