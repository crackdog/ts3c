{-# LANGUAGE OverloadedStrings #-}
module JSON (Client(..), Channel(..), decodeClients, decodeChannel) where

import Network.URI
import Network.HTTP
import System.IO
import Data.Aeson ((.:), (.:?), decode, FromJSON(..), Value(..))
import Control.Applicative ((<$>), (<*>))
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.ByteString.Lazy as L
import Data.List

data Client = Client
    { clid            :: Int
    , client_nickname :: String
    , client_type     :: Int
    } deriving Show

instance FromJSON Client where
    parseJSON (Object v) = Client <$>
                           v .: "clid" <*>
                           v .: "client_nickname" <*>
                           v .: "client_type" 

data Channel = Channel
    { channel_name :: String
    , clients      :: [Client]
    } deriving Show

instance FromJSON Channel where
    parseJSON (Object v) = Channel <$>
                           v .: "channel_name" <*>
                           v .: "clients"

decodeClients :: L.ByteString -> Maybe [Client]
decodeClients s = decode s

decodeChannel :: L.ByteString -> Maybe [Channel]
decodeChannel s = decode s
