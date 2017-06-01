{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module Config where

import Data.FileEmbed (embedFile)
import Network.Socket (PortNumber)
import Data.ByteString (ByteString)

token :: ByteString
token = "token"

proxy :: (PortNumber, ByteString)
proxy = (443, "proxy.bbs.ptsecurity.com")

certificate :: ByteString
certificate = $(embedFile "res/rootCert.pem")
