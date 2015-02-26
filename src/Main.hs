{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}

import qualified Network.HTTP.Base as N
import qualified Network.HTTP.Conduit as NHC
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Text.XML as TX
import Text.XML.Cursor (Cursor, ($//), ($.//), (&|), (&.//), (&//), (&/), (>=>))
import qualified Text.XML.Cursor as TXC
import Control.Exception
import qualified Data.Maybe as M
import Data.Default as DD
import Control.Applicative ((<$>))

prompt :: IO String
prompt = do putStrLn "Enter a word: "
            getLine

baseUrl = "http://en.wiktionary.org/w/api.php?action=parse&format=xml&prop=text|revid|displaytitle&callback=?&page="

-- <h2>
--      <span class="mw-headline" id="French">French</span>
-- </h2>
-- ...
-- <h2>
-- </h2>

searchLanguage = "French"

findTextNode :: Cursor -> Maybe T.Text
findTextNode cursor = M.listToMaybe $ cursor $// textContentAxis
        where textContentAxis = TXC.element "text" >=> TXC.child >=> TXC.content

-- TODO it'd be nice to have simpleHttp return an Either type
cursorFor :: String -> IO (Either SomeException Cursor)
cursorFor url = do page <- NHC.simpleHttp url
                   return $ TXC.fromDocument <$> TX.parseLBS DD.def page

maybeToEither = flip maybe Right . Left

wikiContentCursorFor :: Cursor -> Either SomeException Cursor
wikiContentCursorFor cursor = fmap TXC.fromDocument documentE
    where noTextException =  toException $ IndexOutOfBounds "no text element found"
          wrapRoot x = T.append (T.pack "<root>") $ T.append x (T.pack "</root>")
          textNodeM = fmap wrapRoot $ findTextNode cursor
          textNodeE = maybeToEither noTextException textNodeM
          documentE = TX.parseText DD.def . TL.fromStrict =<< textNodeE

cursorHeadElEquals :: String -> Cursor -> Bool
cursorHeadElEquals name cursor = name == elName
    where node = TXC.node cursor
          elName :: String
          elName = case node of
                       NodeElement el -> T.unpack . nameLocalName $ elementName el
                       otherwise -> ""

definitionContentCursor :: Cursor -> [Cursor]
definitionContentCursor cursor = takeWhile notH2 afterCursors
    where afterCursors = cursor $// TXC.attributeIs "id" searchLanguage
                                >=> TXC.parent
                                >=> TXC.followingSibling
          notH2 = not . cursorHeadElEquals "h2"

renderDefinitionContent :: [Cursor] -> String
renderDefinitionContent = undefined

definitionList :: [Cursor] -> [Cursor]
definitionList = filter $ cursorHeadElEquals "ol"

main :: IO ()
main = do
    userWord <- prompt
    let word = N.urlEncode userWord
    cursorE <- cursorFor $ baseUrl ++ word
    let cc = do cursor <- cursorE
                contentCursor <- wikiContentCursorFor cursor
                let defContent = definitionContentCursor contentCursor
                return $ definitionList defContent
    print cc
