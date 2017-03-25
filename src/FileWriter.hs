module FileWriter where

import ClassyPrelude.Conduit

import Types (exampleFile, totalLines)

fileProducer :: Monad m => Producer m ByteString
fileProducer = do
  yield "00 date"
  yieldMany dataLines
  yield $ "99 " <> encodeUtf8 (tshow totalLines)
  where
    dataLines :: [ByteString]
    dataLines =
      fmap (\n -> "01 data " <> encodeUtf8 (tshow n)) [1 .. totalLines]

fileWriter
  :: (MonadBaseControl IO m, MonadIO m, MonadThrow m)
  => m ()
fileWriter =
  runResourceT $ fileProducer $$ sinkFile exampleFile
