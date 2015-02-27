{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}

import qualified Data.Text.IO as TI
import qualified Network.HTTP.Base as N
import qualified Network.HTTP.Conduit as NHC
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Text.XML as TX
import Text.XML.Cursor (Cursor, ($//), (>=>))
import qualified Text.XML.Cursor as TXC
import Control.Exception
import qualified Data.Maybe as M
import qualified Data.Map as MP
import Data.Default as DD
import Control.Applicative ((<$>))

data Definition = Definition
                  { sourceLang :: Language
                  , definitionLang :: Language
                  , partOfSpeechList :: [(T.Text, [T.Text])]
                  }

showPartOfSpeech :: (T.Text, [T.Text]) -> IO ()
showPartOfSpeech (pos, ds) = do TI.putStrLn pos
                                mapM_ TI.putStrLn ds

showDefinition :: Definition -> IO ()
showDefinition def = let ps = partOfSpeechList def
                     in mapM_ showPartOfSpeech ps

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

baseUrl :: String
baseUrl = "http://" ++ langCode destinationLang ++ ".wiktionary.org/w/api.php?action=parse&format=xml&prop=text|revid|displaytitle&callback=?&page="

lookupLang :: Language
lookupLang = French

destinationLang :: Language
destinationLang = English

languageHeading :: T.Text
languageHeading = MP.findWithDefault "English" (lookupLang, destinationLang) langDict

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

findTextNode :: Cursor -> Maybe T.Text
findTextNode cursor = M.listToMaybe $ cursor $// textContentAxis
        where textContentAxis = TXC.element "text" >=> TXC.child >=> TXC.content

-- TODO it'd be nice to have simpleHttp return an Either type
cursorFor :: String -> IO (Either SomeException Cursor)
cursorFor url = do page <- NHC.simpleHttp url
                   return $ TXC.fromDocument <$> TX.parseLBS DD.def page

maybeToEither :: forall a a1. a1 -> Maybe a -> Either a1 a
maybeToEither = flip maybe Right . Left

wikiContentCursorFor :: Cursor -> Either SomeException Cursor
wikiContentCursorFor cursor = fmap TXC.fromDocument documentE
    where noTextException =  toException $ IndexOutOfBounds "no text element found"
          wrapRoot x = T.append (T.pack "<root>") $ T.append x (T.pack "</root>")
          textNodeM = wrapRoot <$> findTextNode cursor
          textNodeE = maybeToEither noTextException textNodeM
          documentE = TX.parseText DD.def . TL.fromStrict =<< textNodeE

cursorHeadElEquals :: String -> Cursor -> Bool
cursorHeadElEquals name cursor = name == elName
    where node = TXC.node cursor
          elName :: String
          elName = case node of
                       NodeElement el -> T.unpack . nameLocalName $ elementName el
                       _              -> ""

definitionContentCursor :: Cursor -> [Cursor]
definitionContentCursor cursor = takeWhile notH2 afterCursors
    where afterCursors = cursor $// TXC.attributeIs "id" languageHeading
                                >=> TXC.parent
                                >=> TXC.followingSibling
          notH2 = not . cursorHeadElEquals "h2"

definitionList :: [Cursor] -> [T.Text]
definitionList = map formatDefinition . filter (cursorHeadElEquals "li") . concatMap TXC.child . filter (cursorHeadElEquals "ol")

formatDefinition :: Cursor -> T.Text
formatDefinition cursor = T.concat $ cursor $// TXC.content

getSections :: [Cursor] -> [(T.Text, [T.Text])]
getSections [] = []
getSections xs = let notH3 = not . cursorHeadElEquals "h3"
                     -- TODO this must be made safe
                     (h3el:h3start) = dropWhile notH3 xs
                     title = getSectionTitle h3el
                     (part, rest) = span notH3 h3start
                     definitions = definitionList part
                 in  if null definitions then getSections rest
                                         else (title, definitions) : getSections rest

getSectionTitle :: Cursor -> T.Text
getSectionTitle cursor = let headline = cursor $// TXC.attributeIs "class" "mw-headline"
                                               >=> TXC.child
                                               >=> TXC.content
                         in M.fromMaybe "Word" $ M.listToMaybe headline

prompt :: IO String
prompt = do TI.putStrLn "Enter a word: "
            getLine

main :: IO ()
main = do
    userWord <- prompt
    let word = N.urlEncode userWord
    cursorE <- cursorFor $ baseUrl ++ word
    let cc = do cursor <- cursorE
                contentCursor <- wikiContentCursorFor cursor
                let defContent = definitionContentCursor contentCursor
                let wordPartSections = getSections defContent
                return wordPartSections
    print cc
    main
