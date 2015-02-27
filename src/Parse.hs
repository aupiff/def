{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
module Parse where

import qualified Text.XML.Cursor as TXC
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import qualified Data.Maybe as M
import qualified Data.ByteString.Lazy as LBS
import Text.XML as TX
import Text.XML.Cursor (Cursor, ($//), (>=>))
import Data.Default as DD
import Control.Applicative ((<$>))
import Control.Exception

import Definition
import Language

maybeToEither :: forall a a1. a1 -> Maybe a -> Either a1 a
maybeToEither = flip maybe Right . Left

cursorFor :: LBS.ByteString -> Either SomeException Cursor
cursorFor = fmap TXC.fromDocument . TX.parseLBS DD.def

findTextNode :: Cursor -> Maybe T.Text
findTextNode cursor = M.listToMaybe $ cursor $// textContentAxis
        where textContentAxis = TXC.element "text" >=> TXC.child >=> TXC.content

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
definitionList =  map formatDefinition . getListElements . getOrderedLists
    where getOrderedLists = filter (cursorHeadElEquals "ol")
          getListElements = filter (cursorHeadElEquals "li") . concatMap TXC.child

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

pageToDefinition :: LBS.ByteString -> Either SomeException Definition
pageToDefinition page =
    do cursor <- cursorFor page
       contentCursor <- wikiContentCursorFor cursor
       let defContent = definitionContentCursor contentCursor
       let wordPartSections = getSections defContent
       return Definition { sourceLang = lookupLang
                         , definitionLang = destinationLang
                         , partOfSpeechList = wordPartSections
                         }
