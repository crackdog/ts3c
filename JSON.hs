{-# LANGUAGE OverloadedStrings #-}
module JSON (Client(..), Channel(..), decodeClients, decodeChannel) where

import Network.URI
import Network.HTTP
import System.IO
import Data.Aeson ((.:), (.:?), decode, FromJSON(..), Value(..))
import Control.Applicative ((<$>), (<*>))
import qualified Data.ByteString.Lazy.Char8 as BS
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

decodeClients :: String -> Maybe [Client]
decodeClients s = decode (BS.pack s)

decodeChannel :: String -> Maybe [Channel]
decodeChannel s = decode (BS.pack s)

getClientlist :: String -> IO String
getClientlist url = simpleHTTP (getRequest url) >>= getResponseBody
