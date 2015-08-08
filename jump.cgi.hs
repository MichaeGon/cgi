{-# OPTIONS -Wall -Werror #-}
import Control.Monad
import Data.Maybe
import Network.CGI

import FileManager
import RandomST

main :: IO ()
main = runCGI $ handleErrors cgiMain

cgiMain :: CGI CGIResult
cgiMain = setHeader "Content-type" "text/html; charset=UTF-8" >> url >>= output . html

url :: CGI FilePath
url = getVar' "index" >>= liftIO . liftM (fromJust . toString) . content

getVar' :: (MonadCGI m) => String -> m Int
getVar' x = readInput x >>= return . safety
	where
	safety (Just str) = str
	safety _ = 100

content :: Int -> IO Content
content x = liftM2 (!!) linkContents index
	where	
	range xs = (0, length xs - 1)
	index = linkContents >>= return . fst . flip randomR (mkStdGen x) . range

html :: FilePath -> String
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

