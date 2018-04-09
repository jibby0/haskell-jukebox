{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Lib

import Web.Scotty

import qualified Text.Blaze.Html5 as BlazeHtml
import qualified Text.Blaze.Html5.Attributes as BlazeAttributes

import qualified Text.Blaze.Html.Renderer.Text as BlazeRenderer

import Network.Wai.Middleware.RequestLogger  -- Logging

import Data.Monoid (mconcat)

import System.Random(randomIO)
import System.Process
import System.IO
import Control.Concurrent(threadDelay)

import Network.Socket(connect, close, SocketType(Stream), socket)
import Network.Socket.Internal(Family(AF_UNIX), SockAddr(SockAddrUnix))
import Network.Socket.ByteString(send, recv)

import Data.ByteString(breakSubstring)

import Data.Aeson
import GHC.Generics
import Data.Text
import qualified Data.Vector as V

import qualified Data.ByteString.Lazy as DBSL
import qualified Data.ByteString as DBS

data Command = Command
  { command :: Array
  } deriving (Generic, Show)

instance ToJSON Command where
    toJSON (Command command) =
        object ["command" .= command]

    -- this encodes directly to a bytestring Builder
    toEncoding (Command command) =
        pairs ("command" .= command)

mpvSocket :: String
mpvSocket = "/tmp/haskell-jukebox-mpv-socket"

main :: IO ()
main = do
  putStrLn ("MPV listening at " ++ mpvSocket)
  (_, _, _, p) <- createProcess (proc "mpv"
                                  [ "--input-ipc-server=" ++ mpvSocket
                                  , "/home/josh/shrimp.mp3"
                                  ]) { std_in = NoStream
                                     , std_out = NoStream
                                     , std_err = NoStream}
  threadDelay 2000000
  putStrLn "sending pause"
  togglePause
  threadDelay 2000000
  putStrLn "sending pause"
  togglePause
  waitForProcess p
  return ()

togglePause :: IO ()
togglePause = do
  received <- sendToMPV Command { command = V.fromList
                                  [ "get_property"
                                  , "pause"
                                  ]}
  sendToMPV Command { command = V.fromList
                      [ "set_property"
                      , "pause"
                      , case snd $ breakSubstring "true" received of
                         "" -> Bool True
                         otherwise -> Bool False
                      ]}
  return ()

sendToMPV :: Command -> IO DBS.ByteString
sendToMPV c = do
  putStrLn ("Connected to Socket: " ++ mpvSocket)
  soc <- socket AF_UNIX Stream 0
  connect soc (SockAddrUnix mpvSocket)
  send soc toSend
  putStrLn "Sending: "
  print toSend
  received <- recv soc 4096
  putStrLn ("Received from MPV: " ++ (show received))
  close soc
  return received
  where toSend = (DBSL.toStrict ((encode (c)) `DBSL.append` "\n"))

oldmain :: IO ()
oldmain = scotty 3000 $ do
  middleware logStdoutDev  -- Logging
  get "/say/:word" $ do
    beam <- param "word"
    if beam == "" then
      html "<h1>Go to a URI, such as http://URL/say/beam.</h1>"
    else
      html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
  get "/hello" $ do  -- Access this with /hello?name=yee
    name <- param "name"
    text name
  get "/agent" $ do
    agent <- header "User-Agent"
    text $ maybe "Couldn't determine user agent." id agent
  get "/" $ do
    html . BlazeRenderer.renderHtml $ do
      BlazeHtml.h1 "This is blaze!"
