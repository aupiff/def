{-# LANGUAGE OverloadedStrings #-}
module Definition where

import qualified Data.Text as T
import qualified Data.Text.IO as TI
import Language

data Definition = Definition
                  { sourceLang :: Language
                  , definitionLang :: Language
                  , partOfSpeechList :: [(T.Text, [T.Text])]
                  }

showPartOfSpeech :: (T.Text, [T.Text]) -> IO ()
showPartOfSpeech (pos, ds) = do TI.putStrLn pos
                                mapM_ TI.putStrLn ds

prettyPrintDefinition :: Definition -> IO ()
prettyPrintDefinition def =
    let ps = partOfSpeechList def
    in mapM_ showPartOfSpeech ps


