{-# language OverloadedStrings #-}
module Main where

-- Used to generate new threads
import Control.Concurrent
-- Library for data manipulation
import Conduit
-- Wrappers for other libraries as conduits
import Data.Conduit.Network
import Data.Conduit.TMChan

import Types
import ServerLoop
import ResponsePrinter
import RequestParser

main :: IO ()
main = do
  putStrLn "Welcome to Nacion Lumpen λchat server 0.1!"
  -- Initialize two channels for the server
  -- * serverIn: server receives requests sent by clients
  -- * serverOut: server sends responses to clients
  -- Note: we should be using bounded channels,
  -- but I use unbounded ones for simplicity
  serverIn  <- newTMChanIO
  serverOut <- newTMChanIO
  -- Start the background server thread
  forkIO (backgroundServer serverIn serverOut)
  -- Listen to new connections
  putStrLn ("Listening on port " ++ show lISTEN_PORT)
  let settings = serverSettings lISTEN_PORT "*"
  runTCPServer settings initializeClient

lISTEN_PORT :: Int
lISTEN_PORT = 1212

-- This part listens to server requests
-- and generates the responses
backgroundServer :: TMChan Request -> TMChan Response -> IO ()
backgroundServer inCh outCh = do
      -- Wrap TMChan's as conduits
  let sourceIn = sourceTMChan inCh
      sinkOut  = sinkTMChan outCh True
      -- Build pipeline
      pipeline =    sourceIn 
                 .| scanlC loop' (initialServerState, [])
                 .| concatMapC snd  -- get only messages
                 .| sinkOut
  -- Start pipeline!
  runConduit pipeline

-- Initialize two client threads:
-- one where the incoming messages are transformed
-- into requests for the server, and another where
-- responses are sent if appropiate
initializeClient :: AppData -> IO ()
initializeClient _ = return ()
