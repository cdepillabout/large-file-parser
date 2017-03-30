

I'm trying to write the following parser using [parsec](https://hackage.haskell.org/package/parsec):

    manyLength
      :: forall s u m a.
         Monad m
      => ParsecT s u m a -> ParsecT s u m Int
    manyLength p = go 0
      where
        go :: Int -> ParsecT s u m Int
        go !i = (p *> go (i + 1)) <|> pure i
        
This is just like the [`many`](https://hackage.haskell.org/package/parsec-3.1.11/docs/Text-Parsec.html#v:many) function, but instead of returning `[a]`, it
just returns the number of times `Parser a` succeeds.

This works, but I can't seem to make it run in constant heap space. This makes
sense, since the recursive call to `go` is not in the tail-call position.

If parsec would export the constructor to [`ParsecT`](https://hackage.haskell.org/package/parsec-3.1.11/docs/src/Text.Parsec.Prim.html#ParsecT), it would be possible to
rewrite `manyLength` in CPS'ed form. This is very similar to the [`manyAccum`](https://hackage.haskell.org/package/parsec-3.1.11/docs/src/Text.Parsec.Prim.html#manyAccum)
function:

    manyLengthCPS :: forall s u m a. ParsecT s u m a -> ParsecT s u m Int
    manyLengthCPS p = ParsecT f
      where
        f
          :: forall b.
             State s u
          -> (Int -> State s u -> ParseError -> m b) -- consumed ok
          -> (ParseError -> m b)                     -- consumed err
          -> (Int -> State s u -> ParseError -> m b) -- empty ok
          -> (ParseError -> m b)                     -- empty err
          -> m b
        f s cok cerr eok _ =
          let walk :: Int -> a -> State s u -> ParseError -> m b
              walk !i _ s' _ =
                unParser p s'
                  (walk $ i + 1)            -- consumed-ok
                  cerr                      -- consumed-err
                  manyLengthCPSErr          -- empty-ok
                  (\e -> cok (i + 1) s' e)  -- empty-err
          in unParser p s (walk 0) cerr manyLengthCPSErr (\e -> eok 0 s e)
        {-# INLINE f #-}

    manyLengthCPSErr :: Monad m => m a
    manyLengthCPSErr =
      fail "manyLengthCPS can't be used on parser that accepts empty input"
        
This `manyLengthCPS` function *does* run in constant heap space.
        
Here is the `ParsecT` constructor just for completeness:
    
    newtype ParsecT s u m a = ParsecT
      { unParser
          :: forall b .
             State s u
          -> (a -> State s u -> ParseError -> m b) -- consumed ok
          -> (ParseError -> m b)                   -- consumed err
          -> (a -> State s u -> ParseError -> m b) -- empty ok
          -> (ParseError -> m b)                   -- empty err
          -> m b
      }

I also tried to turn `manyLengthCPS` directly into a non-CPS'ed function using
the low-level [`mkPT`](https://hackage.haskell.org/package/parsec-3.1.11/docs/Text-Parsec-Prim.html#v:mkPT) function:

    manyLengthLowLevel
      :: forall s u m a.
         Monad m
      => ParsecT s u m a -> ParsecT s u m Int
    manyLengthLowLevel p = mkPT f
      where
        f :: State s u -> m (Consumed (m (Reply s u Int)))
        f parseState = do
          consumed <- runParsecT p parseState
          case consumed of
            Empty mReply -> do
              reply <- mReply
              case reply of
                Ok _ _ _ -> manyLengthErr
                Error parseErr -> pure . Empty . pure $ Ok 0 parseState parseErr
            Consumed mReply -> do
              reply <- mReply
              case reply of
                Ok a newState parseErr -> walk 0 a newState parseErr
                Error parseErr -> pure . Consumed . pure $ Error parseErr
          where
            walk
              :: Int
              -> a
              -> State s u
              -> ParseError
              -> m (Consumed (m (Reply s u Int)))
            walk !i _ parseState' _ = do
              consumed <- runParsecT p parseState'
              case consumed of
                Empty mReply -> do
                  reply <- mReply
                  case reply of
                    Ok _ _ _ -> manyLengthErr
                    Error parseErr ->
                      pure . Consumed . pure $ Ok (i + 1) parseState' parseErr
                Consumed mReply -> do
                  reply <- mReply
                  case reply of
                    Ok a newState parseErr -> walk (i + 1) a newState parseErr
                    Error parseErr -> pure . Consumed . pure $ Error parseErr
                
    manyLengthErr :: Monad m => m a
    manyLengthErr =
      fail "manyLengthLowLevel can't be used on parser that accepts empty input"

Just like `manyLength`, `manyLengthLowLevel` doesn't run in constant heap space.

----------------------------------------------------

Is it possible to write `manyLength` so it runs in constant heap space even
without writing it in CPS-style? If not, why not? Is there some fundamental
reason that it is possible in CPS-style but not in non-CPS-style?
