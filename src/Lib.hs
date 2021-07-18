module Lib (formatRow, safeHttpLBS) where

import Control.Exception
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import Data.Char
import Data.Time
import Network.HTTP.Simple
import Text.Layout.Table
import Text.Layout.Table.Cell.Formatted

safeHttpLBS :: Request -> IO (Either SomeException (Response L.ByteString))
safeHttpLBS request = try $ httpLBS request

red :: String -> Formatted String
red s = formatted "\ESC[31m" s "\ESC[0m"

green :: String -> Formatted String
green s = formatted "\ESC[32m" s "\ESC[0m"

yellow :: String -> Formatted String
yellow s = formatted "\ESC[33m" s "\ESC[0m"

toUpperString :: String -> String
toUpperString = map toUpper

formatRow :: (String, Int, NominalDiffTime) -> RowGroup (Formatted String)
formatRow (server, code, time) = rowsG [[color serverName, color $ show latency ++ "ms", color status]]
  where
    serverName = if server == "" then "1" else toUpperString server
    latency = round $ time * 1000
    (color, status)
      | code /= 204 = (red, "Offline")
      | latency <= 500 = (green, "Online")
      | otherwise = (yellow, "Degraded")