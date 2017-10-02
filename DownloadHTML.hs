module DownloadHTML where

import Network.URI
import Network.HTTP
import Data.Maybe
import Data.Either
import qualified Data.ByteString as B

-- | This function is used to download the .html file from the IMDB TOP 250 website
downloadHTML :: String -> String -> IO()
downloadHTML filename source = do
    source <- get source
    B.writeFile filename source
  where
    get url = let uri = case parseURI url of
                          Nothing -> error $ "Invalid URI: " ++ url
                          Just u -> u in
              simpleHTTP (defaultGETRequest_ uri) >>= getResponseBody