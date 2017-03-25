module FileWriter where

import MyPrelude

import Types (exampleFile, totalLines)

fileProducer :: Monad m => Producer m ByteString
fileProducer = do
  yield "00 header\n"
  yieldMany dataLines
  yield $ "99 final " <> encodeUtf8 (tshow totalLines) <> "\n"
  where
    dataLines :: [ByteString]
    dataLines =
      fmap
        (\n -> "01 data " <> encodeUtf8 (tshow n) <> "\n")
        [1 .. totalLines]

fileWriter
  :: (MonadBaseControl IO m, MonadIO m, MonadThrow m)
  => m ()
fileWriter =
  runResourceT $ fileProducer $$ sinkFile exampleFile
