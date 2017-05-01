module Network.Ident where

import Network.Simple.TCP
import Data.Ident

-- | Start an ident server given a function to
-- build replies
identServer :: HostPreference -> ServiceName -> (PortPair -> IO Reply) -> IO ()
identServer pref serviceName buildReply =
  serve pref serviceName $ \(sock, addr) -> do
    mRequestString <- recv sock 1024
    let
      sendReply portPair = do
        r <- buildReply portPair
        send sock $ renderReply r
    maybe
      (return ())
      sendReply
      $ parsePortPair =<< mRequestString
