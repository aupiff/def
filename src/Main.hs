{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}

import qualified Network.HTTP.Base as N
import qualified Network.HTTP.Conduit as NHC
import qualified Data.Text as T
import qualified Data.Text.IO as TI
import qualified System.Environment

import qualified Definition
import qualified Language
import Language (Language)
import qualified Parse

dictionaryOutput :: forall t. Either t Definition.Definition -> IO ()
dictionaryOutput (Left _) = putStrLn "definition not found"
dictionaryOutput (Right d) = let definition = Definition.prettyPrintDefinition d
                             in do TI.putStrLn definition
                                   TI.appendFile "dict.log" definition

lookupLoop :: Language -> Language -> IO ()
lookupLoop sourceLang destLang =
    do word <- getLine
       let cname = Language.langCode destLang
       let baseUrl = "http://" ++ cname ++ ".wiktionary.org/w/api.php?action=parse&format=xml&prop=text|revid|displaytitle&callback=?&page="
       page <- NHC.simpleHttp $ baseUrl ++ N.urlEncode word
       let cc = Parse.pageToDefinition page (T.pack word) sourceLang destLang
       dictionaryOutput cc
       lookupLoop sourceLang destLang

parseArgs :: [String] -> Maybe (Language, Language)
parseArgs args = do if length args == 2 then Just () else Nothing
                    sourceLang <- Language.langCodeLookup $ head args
                    destLang <- Language.langCodeLookup $ args !! 1
                    return (sourceLang, destLang)

main :: IO ()
main = do args <- System.Environment.getArgs
          case parseArgs args of
            Just (srcLang, destLang) -> lookupLoop srcLang destLang
            _                        -> putStrLn "bad args"
