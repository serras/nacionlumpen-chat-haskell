{-# language OverloadedStrings #-}
module Main where

-- Used to generate new threads
import Control.Concurrent
-- Library for data manipulation
import Conduit
-- Wrappers for other libraries as conduits
import Data.Conduit.Network
import Data.Conduit.TMChan

import Model
import Controller

main :: IO ()
main = do
  -- Initialize two channels for the server
  -- * serverIn: server receives requests sent by clients
  -- * serverOut: server sends responses to clients
  -- Note: we should be using bounded channels,
  -- but I use unbounded ones for simplicity
  serverIn  <- newTMChanIO
  serverOut <- newTMChanIO
  -- Start the background server thread
  forkIO backgroundServer
  -- Listen to new connections
  let settings = serverSettings 1212 "*"
  runTCPServer settings initializeClient

  where
    -- This part listens to server requests
    -- and generates the responses
    backgroundServer :: IO ()
    backgroundServer = return ()

    -- Initialize two client threads:
    -- one where the incoming messages are transformed
    -- into requests for the server, and another where
    -- responses are sent if appropiate
    initializeClient :: AppData -> IO ()
    initializeClient _ = return ()
