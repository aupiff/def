{-# LANGUAGE OverloadedStrings #-}

import qualified Network.HTTP.Base as N
import Network.HTTP.Conduit (simpleHttp)
import qualified Data.Text as T
import Text.HTML.DOM (parseLBS)
import Text.XML.Cursor (Cursor, attribute, attributeIs, content, laxElement, element, fromDocument, child,
                        ($//), (&|), (&//), (>=>))
import qualified Data.ByteString.Lazy.Char8 as L

prompt :: IO String
prompt = do putStr "Enter a word: "
            getLine

url = "http://en.wiktionary.org/w/api.php?action=parse&format=xml&prop=text|revid|displaytitle&callback=?&page=obnubiler"

findTextNode :: Cursor -> [Cursor]
findTextNode = element "text"

cursorFor :: String -> IO Cursor
cursorFor u = do
     page <- simpleHttp u
     return $ fromDocument $ parseLBS page

main :: IO ()
main = do
    let word = N.urlEncode "être"
    cursor <- cursorFor url
    print $ cursor $// findTextNode
