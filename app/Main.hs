module Main where

import Control.Concurrent.Async
import qualified Data.ByteString.Char8 as BC
import Data.Time
import Lib
import Network.HTTP.Simple
import System.Environment
import Text.Layout.Table

servers :: [String]
servers = ["", "1a", "1b", "1c", "1d", "2", "2a", "2b", "2c", "2d", "3", "3a", "3b", "3c", "4", "4a", "4b", "4c", "5", "5a", "5b", "5c"]

buildPath :: String -> String
buildPath server = "/retail-mobile" ++ server ++ "/public/api/appinfo-health"

buildRequest :: String -> String -> Request
buildRequest host path =
  setRequestMethod "GET" $
    setRequestHost (BC.pack host) $
      setRequestPath (BC.pack path) $
        setRequestHeader "User-Agent" ["client"] $
          setRequestSecure True $
            setRequestPort 443 $
              setRequestIgnoreStatus $
                defaultRequest

makeRequest :: String -> IO (String, Int, NominalDiffTime)
makeRequest server = do
  host <- getEnv "HOST"
  let path = buildPath server
  let request = buildRequest host path
  t0 <- getCurrentTime
  result <- safeHttpLBS request
  t1 <- getCurrentTime
  let diff = diffUTCTime t1 t0
  case result of
    Left _ -> return (server, 0, diff)
    Right resp -> return (server, getResponseStatusCode resp, diff)

main :: IO ()
main =
  do
    result <- mapConcurrently makeRequest servers
    let rows = map formatRow result
    putStrLn $
      tableString
        [fixedLeftCol 12, fixedLeftCol 12, fixedLeftCol 12]
        asciiS
        (titlesH ["Server", "Latency", "Status"])
        rows