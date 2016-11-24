module Model where

import Data.IntMap (IntMap)

type UserId   = Int
type UserNick = String
type Users    = IntMap UserNick

data ServerState
  = ServerState { users :: Users }

data Request
  = Request { from :: UserId
            , req  :: RequestMessage }
  deriving (Eq, Show)

data RequestMessage
  = ReqNick
  | ReqChangeNick UserNick
  | ReqMessage    String
  | ReqNames
  | ReqQuit       (Maybe String)
  | ReqKick       UserNick
  deriving (Eq, Show)

data Response
  = Response { recipient :: Recipient
             , initiator :: UserId
             , resp :: ResponseMessage }
  deriving (Eq, Show)

data Recipient
  = OneUser UserId
  | Everybody
  deriving (Eq, Show)

data ResponseMessage
  = NickIs UserNick
  | NickMalformed
  | NickInUse
  -- | Names
  | QuitAccepted
  | KickAccepted
  | KickUnknown
  -- Broadcast messages
  -- They behave differently depending on
  -- the client which receives it
  | Message  String
  | Joined   UserNick
  | Renamed  UserNick UserNick
  | Gone     UserNick
  deriving (Eq, Show)