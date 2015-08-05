{-# OPTIONS_GHC -Wall -Werror #-}
import Control.Applicative
import Control.Arrow hiding ((+++))
import Control.Monad
import Control.Monad.State
import Data.List
import Network.CGI hiding (Html)
import System.FilePath
import System.Random
import Text.Html hiding ((</>))

nextCGI :: FilePath
nextCGI = "tmp" </> "jump.cgi"

osFile :: FilePath
osFile = "data" </> "os.dat"

codeFile :: FilePath
codeFile = "data" </> "code.dat"
--------------------------------

main :: IO ()
main = runCGI $ handleErrors cgiMain

cgiMain :: CGI CGIResult
cgiMain = setHeader "Content-type" "text/html; charset=UTF-8" >> serverName >>= doc >>= output
	where
		doc x = liftIO $ do
			cnt <- selectContents <$> contents <*> getStdGen
			let (os, c, t, m) = convert cnt
			xs <- seeds (length t) <$> getStdGen
			(avn, g) <- randomR (0, 1000) <$> getStdGen
			let pn = fst $ randomR (0, 10000) g
			let title' = titleTag c t
			let msg = Html [HtmlString m]
			let links = embedLinks t xs
			let apachev = apacheInfo avn os x pn
			return . render' $ html title' msg links apachev


-----------------------------------------------------------------

render' :: Html -> String
render' = foldr ff "" . getHtmlElements
	where
		ff x acc = (concat . prettyHtml') x ++ acc


html :: Html -> Html -> Html -> Html -> Html
html title' msg links apachev = htmlHeader +++ htmlBody
	where
		htmlHeader = header . concatHtml $ [meta', title', css]
		meta' = meta ! [strAttr "charset" "UTF-8"]
		css = itag "link" ! [rel "stylesheet", strAttr "type" "text/css", href "basic.css"]
		htmlBody = body . concatHtml $ [links, br, msg, br, hr, div']
		div' = tag "div" (apachev +++ br) ! [identifier "body_second"]

titleTag :: String -> String -> Html
titleTag c t = tag "title" . Html $ [HtmlString (c ++ " " ++ t)]

embedLinks :: String -> [Int] -> Html
embedLinks xs = h1 . concatHtml . foldr ff [] . zip xs
	where
		ff (c, n) acc = (anchor . Html) [HtmlString [c]] ! [href (nextCGI ++ "?index=" ++ show n), target "_blank"] : acc


apacheInfo :: Int -> String -> String -> Int -> Html
apacheInfo ver n sname port = Html [HtmlString str] +++ br
	where
		ver' = intersperse '.' . show $ ver
		str = "Apache/" ++ ver' ++ " (" ++ n ++ ") Server at " ++ sname ++ " Port " ++ show port


selectContents :: (RandomGen g) => [([String], Int)] -> g -> [String]
selectContents xs = evalState xs'
	where
		xs' = mapM mbf xs
		mbf (x, n) = state (random' x (0, n))
		random' x b = first (x !!) . randomR b

seeds :: (RandomGen g) => Int -> g -> [Int]
seeds n = take n . randoms

convert :: [String] -> (String, String, String, String)
convert [x, y] = (x, a, b, c)
	where
		(a, b, c) = read y
convert _ = (a, a, a, a)
	where
		a = "error at convert"

------------------------------------------

contents :: IO [([String], Int)]
contents = mapM (edit . lines <=< readFile) [osFile, codeFile]
	where
		edit xs = return (xs, length xs)
