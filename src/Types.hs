module Types where

import Data.ByteString (ByteString)
import Data.IntMap (IntMap)
import qualified Data.IntMap as M

type UserId   = Int
type UserNick = ByteString
type Users    = IntMap UserNick

data ServerState
  = ServerState { users :: Users }

initialServerState :: ServerState
initialServerState = ServerState { users = M.empty }

data Request
  = Request { from :: UserId
            , req  :: RequestMessage }
  deriving (Eq, Show)

data RequestMessage
  = ReqNick
  | ReqChangeNick UserNick
  | ReqMessage    ByteString
  | ReqNames
  | ReqQuit       (Maybe ByteString)
  | ReqKick       UserNick
  | ReqUnknown
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
  = -- Messages for a user
    NickIs UserNick
  | NickAccepted
  | NickMalformed
  | NickInUse
  | MessageAccepted
  | Names [UserNick]
  | QuitAccepted
  | KickAccepted
  | KickUnknown
  | RequestUnknown
  -- Broadcast messages
  | Message  UserNick ByteString
  | Joined   UserNick
  | Renamed  UserNick UserNick
  | Gone     UserNick
  deriving (Eq, Show)

isFinalMessage :: UserId -> Response -> Bool
isFinalMessage usr (Response Everybody initiator (Gone _))
  = usr == initiator
isFinalMessage _ _ = False

isForMe :: UserId -> Response -> Bool
isForMe usr (Response Everybody   u _) = usr /= u
isForMe usr (Response (OneUser u) _ _) = usr == u