{-# language OverloadedStrings #-}
module ResponsePrinter where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Monoid ((<>))

import Types

-- Just get the message as a ByteString
-- Note: ByteStrings is the type used for
--       communication via a socket
printRM :: ResponseMessage -> ByteString
printRM (NickIs nick)
  = "201 " <> nick
printRM NickAccepted
  = "200 Nick name accepted"
printRM NickMalformed
  = "301 Malformed nick"
printRM NickInUse
  = "302 Nick name in use"
printRM MessageAccepted
  = "200 Message accepted"
printRM (Names usrs)
  = B.intercalate "\n" (("203 " <>) <$> init usrs)
    <> ("202 " <> last usrs)
printRM QuitAccepted
  = "200 Command acknowledged"
printRM KickAccepted
  = "200 Bon voyage"
printRM KickUnknown
  = "303 Unknown user"
printRM RequestUnknown
  = "400 Unknown request"
printRM (Message usr msg)
  = "MSG " <> usr <> ": " <> msg
printRM (Joined usr)
  = "JOINED " <> usr
printRM (Renamed old new)
  = "RENAME " <> old <> " " <> new
printRM (Gone usr)
  = "LEFT " <> usr