module Main(main) where

import Prelude


--type Client = (Integer, String, Integer)
data Client = Client Integer String Integer

client :: Integer -> String -> Integer -> Client
client clid client_nickname client_type = Client clid client_nickname client_type

clid :: Client -> Integer
clid (Client c _ _) = c

client_nickname :: Client -> String
client_nickname (Client _ cn _) = cn

client_type :: Client -> Integer
client_type (Client _ _ ct) = ct

isClient :: Client -> Bool
isClient (Client _ _ 1) = False
isClient _              = True

instance Show Client where
  show c = client_nickname c


data Clients = Clients [Client]

clients :: [Client] -> Clients
clients [] = Clients []
clients (c:cs) | isClient c = Clients (c : (getClients (clients cs)))
               | otherwise  = clients cs

getClients :: Clients -> [Client]
getClients (Clients cs) = cs

instance Show Clients where
  show (Clients [])     = ""
  show (Clients [c])    = show c
  show (Clients (c:cs)) | client_type c == 1 = show cs
                        | otherwise          = show c ++ ", "


data Channel = Channel String Clients

channel_name :: Channel -> String
channel_name (Channel s _) = s

channel_clients :: Channel -> Clients
channel_clients (Channel _ cs) = cs



--testdata = [{"clid":2,"client_nickname":"ts3chatter","client_type":1}]
testdata = client 2 "ts3chatter" 1


main = print testdata
