{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}

import qualified Network.HTTP.Base as N
import Network.HTTP.Conduit (simpleHttp)
import qualified Data.Text as T
import Data.Text.Lazy (fromStrict)
import Text.XML
import Data.Text.Encoding
import Text.XML.Cursor (Cursor, attribute, attributeIs, content, laxElement, element, fromDocument, child,
                        ($//), ($.//), (&|), (&.//), (&//), (&/), (>=>))
import Control.Exception
import Data.Maybe (listToMaybe)
import Data.Default
import Control.Applicative

prompt :: IO String
prompt = do putStr "Enter a word: "
            getLine

baseUrl = "http://en.wiktionary.org/w/api.php?action=parse&format=xml&prop=text|revid|displaytitle&callback=?&page="

-- <h2>
--      <span class="mw-headline" id="French">French</span>
-- </h2>
-- ...
-- <h2>
-- </h2>

isFrench :: Cursor -> Bool
isFrench = undefined

findTextNode :: Cursor -> [T.Text]
findTextNode = element "text" >=> child >=> content

-- TODO it'd be nice to have simpleHttp return an Either type
cursorFor :: String -> IO (Either SomeException Cursor)
cursorFor url = do
     page <- simpleHttp url
     return $ fromDocument <$> parseLBS def page

maybeToEither = flip maybe Right . Left

wikiContentCursorFor :: Cursor -> Either SomeException Cursor
wikiContentCursorFor cursor = fmap fromDocument . parseText def . fromStrict =<< textNodeE
                              where noTextException =  toException $ IndexOutOfBounds "no text element found"
                                    textNodeM = listToMaybe $ cursor $.// findTextNode
                                    textNodeE = maybeToEither noTextException textNodeM

main :: IO ()
main = do
    let word = N.urlEncode "gorge"
    cursorE <- cursorFor $ baseUrl ++ word
    let cc = do cursor <- cursorE
                wikiContentCursorFor cursor
    print cc
