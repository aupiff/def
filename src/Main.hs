{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}

import qualified Network.HTTP.Base as N
import qualified Network.HTTP.Conduit as NHC
import qualified Data.Text as T
import qualified Data.Text.IO as TI

import qualified Definition
import qualified Language
import qualified Parse

dictionaryOutput :: forall t. Either t Definition.Definition -> IO ()
dictionaryOutput (Left _) = putStrLn "definition not found"
dictionaryOutput (Right d) = let definition = Definition.prettyPrintDefinition d
                             in do TI.putStrLn definition
                                   TI.appendFile "dict.log" definition

baseUrl :: String
baseUrl = "http://" ++ cname ++ ".wiktionary.org/w/api.php?action=parse&format=xml&prop=text|revid|displaytitle&callback=?&page="
    where cname = Language.langCode Language.destinationLang

main :: IO ()
main = do word <- getLine
          page <- NHC.simpleHttp $ baseUrl ++ N.urlEncode word
          let cc = Parse.pageToDefinition page $ T.pack word
          dictionaryOutput cc
          main
