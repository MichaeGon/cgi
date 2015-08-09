{-# OPTIONS_GHC -Wall -Werror #-}
import Control.Applicative
import Control.Monad
import Data.Maybe
import Network.CGI
import System.FilePath
import System.Random
import Text.Html hiding((</>))

linkFile :: FilePath
linkFile = ".." </> "data" </> "link.dat"
---------------------------------------

main :: IO ()
main = runCGI $ handleErrors cgiMain

cgiMain :: CGI CGIResult
cgiMain = setHeader "Content-type" "text/html; charset=UTF-8" >> liftIO url >>= output . prettyHtml . html
 	where
		cnts = readFile linkFile
		len = length <$> cnts
		idx = getIndex <$> gen <*> len
		url = (!!) <$> cnts <*> idx
		gen = mkStdGen <$> getVar' "index"

getIndex :: StdGen -> Int -> Int
getIndex n = fst $ randomR range (mkStdGen n)
	where
		range = (0, n - 1)

getVar' :: (Monad m) => String -> m Int
getVar' x = readInput x >>= return . safety
	where
	safety (Just str) = str
	safety _ = 100

html :: FilePath -> Html
{-
html path = concat [
	"<html>",
	"<head>",
	"<meta http-equiv=\"refresh\" content=\"0; URL=" ++ path ++ "\">",
	"<meta charset=\"UTF-8\">",
	"<title>jump</title>",
	"</head>",
	"<body>",
	"</body>",
	"</html>"]
-}
html path = htmlHeader +++ body noHtml
	where
		htmlHeader = header . concatHtml $ [meta', title']
		meta' = meta ! [httpequiv "refresh", content ("0; URL=" ++ path), strAttr "charset" "UTF-8"]
		title' = tag "TITLE" . Html $ [HtmlString "jump"]
