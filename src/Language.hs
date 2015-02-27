{-# LANGUAGE OverloadedStrings #-}
module Language where

import qualified Data.Text as T
import qualified Data.Map as MP

data Language = French
              | English
              | Spanish
              | Russian
              | Arabic
              | German
              | Mandarin deriving (Ord, Eq, Show)

langCode :: Language -> String
langCode French = "fr"
langCode English = "en"
langCode Spanish = "en"
langCode Russian = "ru"
langCode Arabic = "ar"
langCode German = "de"
langCode Mandarin = "zh"

-- TODO these inconsistancies among various wiktionaries make me think I shoud
-- look for content of <span> elements within <h2/h1>
langDict :: MP.Map (Language, Language) T.Text
langDict = MP.fromList [ ((French, French), "Fran.C3.A7ais") -- why does wiktionary do this?
                                                             -- ids have to be
                                                             -- ascii?
                       , ((French, English), "French")
                       , ((Spanish, English), "Spanish")
                       , ((Russian, Russian), "Русский") -- ru.wiktionary does this in a strange way.
                                                         -- uses <h1> tags and
                                                         -- ids seem meaningless
                       , ((Russian, English), "Russian")
                       ]
