{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}

import qualified Network.HTTP.Base as N
import qualified Network.HTTP.Conduit as NHC
import Control.Applicative ((<$>))

import Definition
import Language
import Parse

prompt :: IO String
prompt = do putStrLn "Enter a word: "
            getLine

dictionaryOutput :: forall t. Either t Definition -> IO ()
dictionaryOutput (Left _) = putStrLn "definition not found"
dictionaryOutput (Right d) = prettyPrintDefinition d

baseUrl :: String
baseUrl = "http://" ++ langCode destinationLang ++ ".wiktionary.org/w/api.php?action=parse&format=xml&prop=text|revid|displaytitle&callback=?&page="

main :: IO ()
main = do word <- N.urlEncode <$> prompt
          page <- NHC.simpleHttp $ baseUrl ++ word
          let cc = pageToDefinition page
          dictionaryOutput cc
          main
