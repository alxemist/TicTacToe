{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Gintis
import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)
import Network.HTTP.Types.Header
import Data.Char
import Data.Tuple.Select
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Lazy.Char8 as C
import Data.Maybe
--import Data.ByteString.Lazy.pack as C
urlSuper = "http://tictactoe.homedir.eu/game/antanas_test_13/player/1"

post :: String -> IO()
post ""= do
  Prelude.putStrLn $ "Test: " ++ (("d1:0")++(fromJust (turn "d")))
  manager <- newManager defaultManagerSettings
  initialRequest <- parseUrlThrow urlSuper
  let request = initialRequest { method = B.pack "POST",
                                 requestHeaders = [(hContentType,B.pack "application/bencode+map"),(hAccept,B.pack "application/bencode+map")],
                                 requestBody = RequestBodyLBS $ C.pack (("d1:0")++(fromJust (turn "d")))
                               }
  response <- httpLbs request manager
  Prelude.putStrLn $ "The status code was: " ++ (show $ statusCode $ responseStatus response)
  print $ responseBody response
  get
post msg= do
  manager <- newManager defaultManagerSettings
  initialRequest <- parseUrlThrow urlSuper
  let request = initialRequest { method = B.pack "POST",
                                 requestHeaders = [(hContentType,B.pack "application/bencode+map"),(hAccept,B.pack "application/bencode+map")],
                                 requestBody = RequestBodyLBS $ C.pack ((Prelude.take ((Prelude.length msg) - 1) msg)++"1:"++[(intToDigit (div (Prelude.length(pirmasZingsnis msg)) 2))]++(fromJust (turn msg)))
                               }
  response <- httpLbs request manager
  Prelude.putStrLn $ "The status code was atejo: " ++ (show $ statusCode $ responseStatus response)
  Prelude.putStrLn $ msg
  print $ responseBody response
  get
get :: IO()
get = do
  manager <- newManager defaultManagerSettings
  initialRequest <- parseUrlThrow urlSuper
  let request = initialRequest { method = "GET",
                                 requestHeaders = [(hContentType, "application/bencode+map"),(hAccept, "application/bencode+map")]
                               }
  response <- httpLbs request manager
  Prelude.putStrLn $ "The status code was1: " ++ (show $ statusCode $ responseStatus response)
  post $ unpack $ responseBody response

-- parseMove :: Maybe (Int, Int, Char) -> Maybe String
-- parseMove Nothing = Nothing
-- parseMove (Just (x,y,z)) = Just (" (m \"x\" " ++ [(intToDigit x)] ++ " \"y\" " ++ [(intToDigit y)] ++  "\"v\" \"" ++ [z] ++"\"))")

main :: IO()
main = do
    post ""
