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
findTextNode = TXC.element "text" >=> TXC.child >=> TXC.content

-- TODO it'd be nice to have simpleHttp return an Either type
cursorFor :: String -> IO (Either SomeException Cursor)
cursorFor url = do page <- NHC.simpleHttp url
                   return $ TXC.fromDocument <$> TX.parseLBS DD.def page

maybeToEither = flip maybe Right . Left

wikiContentCursorFor :: Cursor -> Either SomeException Cursor
wikiContentCursorFor cursor =
    fmap TXC.fromDocument . TX.parseText DD.def . TL.fromStrict =<< textNodeE
    where noTextException =  toException $ IndexOutOfBounds "no text element found"
          wrapRoot x = T.append (T.pack "<root>") $ T.append x (T.pack "</root>")
          textNodeM = fmap wrapRoot . M.listToMaybe $ cursor $.// findTextNode
          textNodeE = maybeToEither noTextException textNodeM

definitionContentCursor :: Cursor -> [Cursor]
definitionContentCursor cursor = cursor $.// TXC.element "h2"

main :: IO ()
main = do
    let word = N.urlEncode "gorge"
    cursorE <- cursorFor $ baseUrl ++ word
    let cc = do cursor <- cursorE
                contentCursor <- wikiContentCursorFor cursor
                return $ definitionContentCursor contentCursor
    print cc
