
module Types where

import MyPrelude

exampleFile :: FilePath
exampleFile = "big-example-file"

totalLines :: Int
totalLines = 1000000

data HeaderLine = HL
  deriving (Data, Eq, Read, Show, Typeable)

data DataLine = DL
  { quote :: ByteString
  , price :: Int
  } deriving (Data, Eq, Read, Show, Typeable)

newtype FinalLine = FL
  { unFinalLine :: Int
  } deriving (Data, Eq, Read, Show, Typeable)

data Line
  = HeaderLine HeaderLine
  | DataLine DataLine
  | FinalLine FinalLine
  deriving (Data, Eq, Read, Show, Typeable)

data FullFile = FullFile
  { fullFileHeaderLine :: HeaderLine
  , fullFileDataLine :: [DataLine]
  , fullFileFinalLine :: FinalLine
  } deriving (Data, Eq, Read, Show, Typeable)
