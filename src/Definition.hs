{-# LANGUAGE OverloadedStrings #-}
module Definition where

import qualified Data.Text as T

import Language

data Definition = Definition
                  { word :: T.Text
                  , sourceLang :: Language
                  , definitionLang :: Language
                  , partOfSpeechList :: [(T.Text, [T.Text])]
                  }

showPartOfSpeech :: (T.Text, [T.Text]) -> T.Text
showPartOfSpeech (pos, ds) = T.concat [ pos
                                      , "\n"
                                      , T.intercalate "\n" ds
                                      , "\n"
                                      ]

prettyPrintDefinition :: Definition -> T.Text
prettyPrintDefinition def =
    let ps = partOfSpeechList def
    in T.concat [ "----------------------------\n"
                , word def
                , "\n\n"
                , T.concat $ map showPartOfSpeech ps
                , "----------------------------"
                ]
