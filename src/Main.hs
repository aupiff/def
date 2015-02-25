{-# LANGUAGE OverloadedStrings #-}

import qualified Network.HTTP.Base as N
import Network.HTTP.Conduit (simpleHttp)
import qualified Data.Text as T
import Text.HTML.DOM (parseLBS)
import Text.XML.Cursor (Cursor, attribute, attributeIs, content, laxElement, element, fromDocument, child,
                        ($//), ($.//), (&|), (&.//), (&//), (&/), (>=>))
import qualified Data.ByteString.Lazy.Char8 as L

prompt :: IO String
prompt = do putStr "Enter a word: "
            getLine

baseUrl = "http://en.wiktionary.org/w/api.php?action=parse&format=xml&prop=text|revid|displaytitle&callback=?&page="

-- <h2>
-- 	<span class="mw-headline" id="French">French</span> 
-- </h2>
-- ...
-- <h2>
-- </h2>

isFrench :: Cursor -> Bool
isFrench = undefined

findTextNode :: Cursor -> [T.Text]
findTextNode = element "text" >=> child >=> content

cursorFor :: String -> IO Cursor
cursorFor u = do
     page <- simpleHttp u
     return $ fromDocument $ parseLBS page

main :: IO ()
main = do
    let word = N.urlEncode "gorge"
    cursor <- cursorFor $ baseUrl ++ word
    print $ cursor $.// findTextNode
