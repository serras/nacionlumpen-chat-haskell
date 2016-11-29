{-# language OverloadedStrings   #-}
{-# language ScopedTypeVariables #-}
module Main where

-- Used to generate new threads
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
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
  let settings = serverSettings lISTENPORT "*"
  concurrently_ (backgroundServer serverIn serverOut)
                (runTCPServer settings (initializeClient serverIn serverOut))

lISTENPORT :: Int
lISTENPORT = 1212

-- This part listens to server requests
-- and generates the responses
backgroundServer :: TMChan Request -> TMChan Response -> IO ()
backgroundServer inCh outCh = do
      -- Wrap TMChan's as conduits
  let sourceIn = sourceTMChan inCh
      sinkOut  = sinkTMChan outCh True
      -- Build pipeline
      pipeline =    sourceIn
                 .| concatMapAccumC loop initialServerState
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
                       -- If the connection should be terminated,
                       -- throw an exception to kill thread
                    .| mapC (\msg -> if isFinalMessage clientId msg
                                        then throw EndSocketException
                                        else msg)
                       -- Only send messages for me
                    .| filterC (isForMe clientId)
                    .| mapC printResponse
                    .| appSink app
  -- Start request pipeline
  let reqPipeline =    appSource app
                       -- Split incoming messages by newlines
                    .| splitOnUnboundedE isEndOfLineChar
                    .| filterC (\x -> B.length x > 0)
                       -- Try to parse, unknown messages are
                       -- converted into ReqUnknown
                    .| mapC (parseOnly parseRequest)
                    .| mapC (either (const ReqUnknown) id)
                       -- Decorate with sender information
                    .| mapC (Request clientId)
                    .| sinkTMChan requestsCh True
  concurrently_ (runConduit rspPipeline)
                (runConduit reqPipeline)

isEndOfLineChar :: (Eq a, Num a) => a -> Bool
isEndOfLineChar x = x == 0 || x == 10 || x == 13

-- Define a new exception type
data EndSocketException = EndSocketException
                        deriving Show
instance Exception EndSocketException