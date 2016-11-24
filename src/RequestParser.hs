{-# language OverloadedStrings #-}
module RequestParser where

import Control.Applicative
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Attoparsec.ByteString

import Types

parseRequest :: Parser RequestMessage
parseRequest
  =   try (ReqChangeNick <$  string "NICK"
                         <*  space
                         <*> parseNick
                         <*  endOfLine)
  <|> try (ReqNick       <$  string "NICK"
                         <*  endOfLine)
  <|> try (ReqMessage    <$  string "MSG"
                         <*  space
                         <*> parseMessage
                         <*  endOfLine)
  <|> try (ReqNames      <$  string "NAMES"
                         <*  endOfLine)
  <|> try (ReqQuit       <$  string "QUIT"
                         <*> option Nothing (Just <$ space <*> parseMessage)
                         <*  endOfLine)
  <|> try (ReqKick       <$  string "KICK"
                         <*> parseNick
                         <*  endOfLine)
  <|> pure ReqUnknown

parseNick :: Parser UserNick
parseNick = B.pack <$> many1 (satisfy (notInClass " \n\r"))

parseMessage :: Parser ByteString
parseMessage = B.pack <$> many1 (satisfy (notInClass "\n\r"))

space :: Parser ()
space = skip (== 32)  -- 32 = ASCII code for space

endOfLine :: Parser ()
endOfLine = skip (inClass "\n\r")