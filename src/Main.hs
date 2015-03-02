{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}

import qualified Network.HTTP.Base as N
import qualified Network.HTTP.Conduit as NHC
import qualified Data.Text as T
import qualified Data.Text.IO as TI
import qualified System.Environment
import qualified Data.Map as MP

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
parseArgs args = if length args == 2
                    then do sourceLang <- MP.lookup (head args) Language.cmdLang
                            destLang <- MP.lookup (args !! 1) Language.cmdLang
                            return (sourceLang, destLang)
                    else Nothing

main :: IO ()
main = do args <- System.Environment.getArgs
          let langPairM = parseArgs args
          case langPairM of
            Just (sourceLang, destLang) -> lookupLoop sourceLang destLang
            _                           -> print "bad args"
