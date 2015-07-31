{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}

import qualified Network.HTTP.Base    as N
import qualified Network.HTTP.Conduit as NHC
import qualified Data.Text            as T
import qualified Data.Text.IO         as TI
import qualified System.Environment
import           Control.Applicative ((*>), (<$>), liftA2)

import qualified Definition
import qualified Language
import           Language (Language)
import qualified Parse

dictionaryOutput :: forall t. Either t Definition.Definition -> IO ()
dictionaryOutput (Left _) = putStrLn "definition not found"
dictionaryOutput (Right d) = let definition = Definition.prettyPrintDefinition d
                             in do TI.putStrLn definition
                                   TI.appendFile "dict.log" definition

lookupLoop :: Language -> Language -> IO ()
lookupLoop sourceLang destLang =
    let cname = Language.langCode destLang
        baseUrl = "http://" ++ cname ++ ".wiktionary.org/w/api.php?action=parse&format=xml&prop=text|revid|displaytitle&callback=?&page="
        loop = do word <- fmap (\x -> if x == ' ' then '_' else x) <$> getLine
                  page <- NHC.simpleHttp $ baseUrl ++ N.urlEncode word
                  let wordDef = Parse.pageToDefinition page (T.pack word) sourceLang destLang
                  dictionaryOutput wordDef
                  loop
    in loop

parseArgs :: [String] -> Maybe (Language, Language)
parseArgs args = argLenM *> liftA2 (,) sourceLang destLang
        where argLenM = if length args == 2 then Just () else Nothing
              sourceLang = Language.langCodeLookup $ head args
              destLang = Language.langCodeLookup $ args !! 1

main :: IO ()
main = do args <- System.Environment.getArgs
          case parseArgs args of
            Just (srcLang, destLang) -> lookupLoop srcLang destLang
            _                        -> putStrLn "bad args"
