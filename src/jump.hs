--{-# OPTIONS_GHC -Wall -Werror #-}

import Network.CGI hiding (Html)
import System.FilePath
import System.Random
import Text.Html hiding((</>))

linkFile :: FilePath
linkFile = ".." </> "data" </> "link.dat"
---------------------------------------

main :: IO ()
main = runCGI $ handleErrors cgiMain

cgiMain :: CGI CGIResult
cgiMain = setHeader "Content-type" "text/html; charset=UTF-8" >> url >>= output . prettyHtml . html
     where
        cnts = lines <$> (liftIO . readFile) linkFile
        len = length <$> cnts
        idx = getIndex <$> gen <*> len
        url = (!!) <$> cnts <*> idx
        gen = mkStdGen <$> getVar' "index"

getIndex :: StdGen -> Int -> Int
getIndex g n = fst $ randomR range g
    where
        range = (0, n - 1)

getVar' :: (MonadCGI m) => String -> m Int
getVar' x = safety <$> readInput x
    where
    safety (Just str) = str
    safety _ = 100

html :: FilePath -> Html
html path = htmlHeader +++ body noHtml
    where
        htmlHeader = header . concatHtml $ [meta', title']
        meta' = meta ! [httpequiv "refresh", content ("0; URL=" ++ path), strAttr "charset" "UTF-8"]
        title' = tag "TITLE" . Html $ [HtmlString "jump"]
