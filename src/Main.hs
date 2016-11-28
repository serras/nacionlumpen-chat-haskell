{-# language OverloadedStrings   #-}
{-# language ScopedTypeVariables #-}
module Main where

-- Used to generate new threads
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
-- Needed to run the parser
import Data.Attoparsec.ByteString
-- Library for data manipulation
import Conduit
import Data.Conduit.Combinators (splitOnUnboundedE)
-- Wrappers for other libraries as conduits
import Data.Conduit.Network
import Data.Conduit.TMChan
-- Utilities
import System.Random (randomRIO)
import qualified Data.ByteString as B
import Data.Word (Word8)

import Debug.Trace

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
  -- Start the server threads
  let settings = serverSettings lISTEN_PORT "*"
  concurrently_ (backgroundServer serverIn serverOut)
                (runTCPServer settings (initializeClient serverIn serverOut))

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
  putStrLn "Starting background server..."
  runConduit pipeline

-- Initialize two client threads:
-- one where the incoming messages are transformed
-- into requests for the server, and another where
-- responses are sent if appropiate
initializeClient :: TMChan Request -> TMChan Response
                 -> AppData -> IO ()
initializeClient serverIn serverOut app = do
  -- Generate random id
  (clientId :: UserId) <- randomRIO (0, 10000)
  putStrLn ("New client with id " ++ show clientId)
  -- Duplicate channels
  (requestsCh, responsesCh) <- atomically $
    (,) <$> dupTMChan serverIn <*> dupTMChan serverOut
  -- Start response pipeline
  let rspPipeline =    sourceTMChan responsesCh
                    .| takeWhileC (not . isFinalMessage clientId)
                    .| filterC (isForMe clientId)
                    .| mapC printResponse
                    .| appSink app
  -- Start request pipeline
  let reqPipeline =    appSource app
                    .| splitOnUnboundedE isEndOfLine
                    .| filterC (\x -> B.length x > 0)
                    .| mapC (parseOnly parseRequest)
                    .| mapC (either (const ReqUnknown) id)
                    .| mapC (Request clientId)
                    .| sinkTMChan requestsCh True
  concurrently_ (runConduit rspPipeline)
                (runConduit reqPipeline)

isEndOfLine :: Word8 -> Bool
isEndOfLine x = x == 0 || x == 10 || x == 13