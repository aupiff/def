{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}

import qualified Network.HTTP.Base as N
import qualified Network.HTTP.Conduit as NHC
import Control.Applicative ((<$>))

import qualified Definition
import qualified Language
import qualified Parse

prompt :: IO String
prompt = do putStrLn "Enter a word: "
            getLine

dictionaryOutput :: forall t. Either t Definition.Definition -> IO ()
dictionaryOutput (Left _) = putStrLn "definition not found"
dictionaryOutput (Right d) = Definition.prettyPrintDefinition d

baseUrl :: String
baseUrl = "http://" ++ cname ++ ".wiktionary.org/w/api.php?action=parse&format=xml&prop=text|revid|displaytitle&callback=?&page="
    where cname = Language.langCode Language.destinationLang

main :: IO ()
main = do word <- N.urlEncode <$> prompt
          page <- NHC.simpleHttp $ baseUrl ++ word
          let cc = Parse.pageToDefinition page
          dictionaryOutput cc
          main
