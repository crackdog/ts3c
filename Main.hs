module Main(main) where

import Prelude
import Data.List
import Network.HTTP.Conduit
import Channellist
import qualified Data.ByteString.Lazy as L

showClient :: Client -> String
showClient c = client_nickname c ++ " (" ++ (showSeconds (connection_connected_time c)) ++ ")"

showSeconds :: Int -> String
showSeconds millis = intercalate ":" [h, m, s]
  where
    t = div millis 1000
    h = show $ div t 3600
    m' = div t 60 `mod` 60
    m = (if m' < 10 then "0" else "") ++ show m'
    s' = t `mod` 60
    s = (if s' < 10 then "0" else "") ++ show s'

isUser :: Client -> Bool
isUser c = client_type c /= 1

showClients :: [Client] -> String
showClients = intercalate ", " . map showClient . filter isUser

showChannel :: Channel -> String
showChannel c = channel_name c ++ ": \t" ++ showClients (clients c)

showChannels :: [Channel] -> String
showChannels cs = if null out then "0 clients are online." else out
  where
    out = intercalate "\n" . map showChannel . filter (not . null . filter isUser . clients) $ cs

get :: String -> IO L.ByteString
get url = simpleHttp url

getChannellist :: String -> IO L.ByteString
getChannellist url = get (url ++ "channellist")

main :: IO ()
main = do
    str <- getChannellist "https://fkarchery.de/ts3chatter/"
    case decodeChannel str of
      Nothing -> putStrLn "internal error"
      Just cs -> putStrLn $ showChannels cs
