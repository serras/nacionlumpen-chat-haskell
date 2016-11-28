{-# language OverloadedStrings #-}
module RequestParser where

import Control.Applicative
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Attoparsec.ByteString
import qualified Data.Attoparsec.ByteString as AP

import Types

parseRequest :: Parser RequestMessage
parseRequest = newlines *> parseRequest' <* newlines

parseRequest' :: Parser RequestMessage
parseRequest'
  =   ReqChangeNick <$  (string "NICK"  <?> "start nick")
                    <*  space
                    <*> (parseNick      <?> "new nick")
  <|> ReqNick       <$  (string "NICK"  <?> "start nick")
  <|> ReqMessage    <$  (string "MSG"   <?> "start msg")
                    <*  space
                    <*> (parseMessage   <?> "msg")
  <|> ReqNames      <$  (string "NAMES" <?> "start names")
  <|> ReqQuit       <$  (string "QUIT"  <?> "start quit")
                    <*> (option Nothing (Just <$ space <*> parseMessage) <?> "bye msg")
  <|> ReqKick       <$  (string "KICK"  <?> "start kick")
                    <*> (parseNick      <?> "nick")

parseNick :: Parser UserNick
parseNick = takeWhile1 (notInClass " \0\n\r")

parseMessage :: Parser ByteString
parseMessage = takeWhile1 (notInClass "\0\n\r")

space :: Parser ()
space = const () <$> string " " <?> "space"

newlines :: Parser ()
newlines = const () <$> AP.takeWhile (inClass "\0\n\r") <?> "start of line"