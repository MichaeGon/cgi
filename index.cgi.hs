{-# OPTIONS -Wall -Werror #-}
import Data.List
import Network.CGI

import FileManager
import RandomST

main :: IO ()
main = runCGI $ handleErrors cgiMain

cgiMain :: CGI CGIResult
cgiMain = setHeader "Content-type" "text/html; charset=UTF-8" >> liftIO getArguments >>= liftIO . html >>= output


html :: [String] -> IO String
html [c, title, msg, name, apachev, portn] = linkEmbed title >>= return . rems
		where
		rems x = concat [
			"<html>",
			"<head>",
			"<meta charset=\"UTF-8\">",
			"<title>" ++ c ++ " " ++ title ++ "</title>",
			"<link rel=\"stylesheet\" type=\"text/css\" href=\"basic.css\" />",
			"</head>",
			"<body>",
			"<h1>" ++ x ++ "</h1>",
			"<br />",
			msg ++ "<br />",
			"<hr />",
			"<div id=\"body_second\">",
			"Apache/" ++ apachev ++ " (" ++ name ++ ") Server at michaegon.jp Port " ++ portn ++ "<br />",
			"</div>",
			"</body>",
			"</html>"]

html _ = error "html: invalid number of arguments"

linkEmbed :: String -> IO String
linkEmbed xs = rands >>= return . foldr mkTag [] . flip zip xs

mkTag :: (Int, Char) -> String -> String
mkTag (n, x) acc = "<a href=\"tmp/jump.cgi?index=" ++ show n ++ "\" target=\"_blank\">" ++ [x] ++ "</a>" ++ acc

rands :: IO [Int]
rands = getStdGen >>= return . fst . runState source
	where
	source = sequence $ repeat randomST


getArguments :: IO [String]
getArguments = sequence [codeContents, osContents] >>= getComponents >>= return . mkArguments

mkArguments :: [Content] -> [String]
mkArguments (HCode (u, v, w) : Strings x : Strings y : Strings z : _) = [u, v, w, x, y, z]
mkArguments _ = ["000", "Undefined", "", "undefined", "0.0.0", "0"]

getComponents :: [[Content]] -> IO [Content]
getComponents xs = source >>= return . foldr (\(x, i) acc -> (x !! i) : acc) []
	where
	source = getStdGen >>= return . zip xs' . fst . runState (starray $ getRanges xs')
	xs' = xs ++ [apacheVer, portNum]

apacheVer :: [Content]
apacheVer = map (Strings . intersperse '.' . show) ([0..1000] :: [Int])

portNum :: [Content]
portNum = map (Strings . show) ([0..10000] :: [Int])

getRanges :: [[Content]] -> [(Int, Int)]
getRanges = foldr (\x acc -> (0, pred $ length x) : acc) []



