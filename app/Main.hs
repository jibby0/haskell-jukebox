{-# LANGUAGE OverloadedStrings #-}
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

main :: IO ()
main = do
  num <- (randomIO :: IO Integer)
  let socket = "/tmp/mpvsocket" ++ (show num)
    in do
    putStrLn ("MPV listening at " ++ socket)
    (_, _, _, p) <- createProcess (proc "mpv" ["--input-ipc-server=" ++ socket, "/home/josh/'IS THAT A MAN RIDING A SHRIMP'-9p_QW_HsKPI.mp3"]){ std_in = NoStream, std_out = NoStream, std_err = NoStream}
    threadDelay 2000000
    togglePause socket
    threadDelay 2000000
    togglePause socket
    waitForProcess p
    return ()

togglePause :: String -> IO ()
togglePause ipcloc = do
  soc <- socket AF_UNIX Stream 0
  connect soc (SockAddrUnix ipcloc)
  send soc "{ \"command\": [\"get_property\", \"pause\"] }\n"
  received <- recv soc 4096
  putStrLn ("Recieved from MPV: " ++ (show received))
  send soc $
    case snd $ breakSubstring "true" received of
      "" -> "{ \"command\": [\"set_property\", \"pause\", true] }\n"
      otherwise -> "{ \"command\": [\"set_property\", \"pause\", false] }\n"
  close soc

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
