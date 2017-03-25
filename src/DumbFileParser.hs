module DumbFileParser where

import MyPrelude

import Text.Parsec.ByteString (Parser)

import Types (FullFile(..))

parserFile :: Parser FullFile
parserFile = undefined

dumbFileParser :: IO ()
dumbFileParser = undefined
