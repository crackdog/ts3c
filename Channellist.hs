{-# LANGUAGE OverloadedStrings #-}
module Channellist (Client(..), Channel(..), decodeClients, decodeChannel) where

import Prelude
import Data.Aeson ((.:), {-(.:?),-} decode, FromJSON(..), Value(..))
import qualified Data.ByteString.Lazy as L

data Client = Client
    { clid                      :: Int
    , clientNickname           :: String
    , clientType               :: Int
    , connectionConnectedTime :: Int
    } deriving Show

instance FromJSON Client where
    parseJSON (Object v) = Client <$>
                           v .: "clid" <*>
                           v .: "client_nickname" <*>
                           v .: "client_type" <*>
                           v .: "connection_connected_time"
    parseJSON _ = error "error at FromJSON"

data Channel = Channel
    { channelName :: String
    , clients      :: [Client]
    } deriving Show

instance FromJSON Channel where
    parseJSON (Object v) = Channel <$>
                           v .: "channel_name" <*>
                           v .: "clients"
    parseJSON _ = error "error at FromJSON"

decodeClients :: L.ByteString -> Maybe [Client]
decodeClients = decode

decodeChannel :: L.ByteString -> Maybe [Channel]
decodeChannel = decode
