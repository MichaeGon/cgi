{-# OPTIONS -Wall -Werror #-}

module FileManager
	( Content(..)
	, HTTPCode
	, toString
	, toHTTPCode
	, osContents
	, codeContents
	, linkContents
	) where

import Control.Monad
import System.FilePath

type HTTPCode = (String, String, String)

data Content = Strings String | HCode HTTPCode deriving (Eq)

osPath :: FilePath
osPath = "data" </> "os.dat"

codePath :: FilePath
codePath = "data" </> "code.dat"

linkPath :: FilePath
linkPath = ".." </> "data" </> "link.dat"

toString :: Content -> Maybe String
toString (Strings str) = Just str
toString _ = Nothing

toHTTPCode :: Content -> Maybe HTTPCode
toHTTPCode (HCode xs) = Just xs
toHTTPCode _ = Nothing

readFile' :: FilePath -> IO [String]
readFile' = liftM lines . readFile

osContents :: IO [Content]
osContents = readFile' osPath >>= return . map Strings

codeContents :: IO [Content]
codeContents = readFile' codePath >>= return . map (HCode . read)

linkContents :: IO [Content]
linkContents = readFile' linkPath >>= return . map Strings
