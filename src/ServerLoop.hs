{-# language NamedFieldPuns #-}
module ServerLoop where

import Control.Applicative
import Data.IntMap (IntMap)
import qualified Data.IntMap as M

import Types

-- Adapter for use within scanl
loop' :: (ServerState, [Response]) -> Request -> (ServerState, [Response])
loop' (s, _) = loop s

-- Defines the basic loop of the server
-- Each request affects the state and returns zero or more messages
loop :: ServerState -> Request -> (ServerState, [Response])
loop s (Request { from, req }) = case req of

  ReqNick -> obtainUserNick $ \nick ->
    ( s, [ Response (OneUser from) from (NickIs nick) ] )
  
  ReqChangeNick newNick -> case findKey newNick (users s) of
    Just _  -> ( s, [ Response (OneUser from) from NickInUse ] )
    Nothing -> -- TODO: validation
      let respMethod = case M.lookup from (users s) of
                         Just oldNick -> Renamed oldNick newNick
                         Nothing      -> Joined  newNick
      in ( s { users = M.insert from newNick (users s) }
         , [ Response (OneUser from) from NickAccepted
           , Response Everybody from respMethod ] )
  
  ReqMessage msg -> obtainUserNick $ \nick ->
    ( s, [ Response (OneUser from) from MessageAccepted
         , Response Everybody from (Message nick msg) ] )
  
  ReqNames ->
    let usrs = snd <$> M.toList (users s) 
     in ( s, [ Response (OneUser from) from (Names usrs) ] )
  
  ReqQuit msg -> obtainUserNick $ \nick ->
    ( s { users = M.delete from (users s)}
    , [ Response (OneUser from) from QuitAccepted
      , Response Everybody from (Gone nick) ] )
  
  ReqKick nick -> case findKey nick (users s) of
    Just usr -> ( s { users = M.delete usr (users s)}
                , [ Response (OneUser from) from KickAccepted
                  , Response Everybody usr (Gone nick) ] )
    Nothing  -> ( s, [ Response (OneUser from) from KickUnknown ])
  
  ReqUnknown -> ( s, [ Response (OneUser from) from RequestUnknown ])
  
  where obtainUserNick :: (UserNick -> a) -> a
        obtainUserNick f = case M.lookup from (users s) of
                             Just nick -> f nick
                             Nothing   -> error "This should never happen"

findKey :: Eq a => a -> IntMap a -> Maybe Int
findKey v = M.foldlWithKey f Nothing
  where f prev k x | x == v    = prev <|> Just k
                   | otherwise = prev