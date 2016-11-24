{-# language NamedFieldPuns #-}
module Controller where

import qualified Data.IntMap as M

import Model

loop :: ServerState -> Request -> (ServerState, Response)
loop s (Request { from, req }) = case req of

  ReqNick -> case M.lookup from (users s) of
    Just nick -> (s, Response (OneUser from) from (NickIs nick))
    Nothing   -> error "This should never happen"
  
  ReqChangeNick newNick ->
    let respMethod = case M.lookup from (users s) of
                       Just oldNick -> Renamed oldNick newNick
                       Nothing      -> Joined  newNick
    in ( s { users = M.insert from newNick (users s) }
       , Response Everybody from respMethod )
