module Main(main) where

import Prelude
import Data.List
import Network.HTTP.Conduit
import Channellist
import qualified Data.ByteString.Lazy as L
import Control.Exception

showClient :: Client -> String
showClient c = "\t" ++ clientNickname c ++ " (" ++ showSeconds (connectionConnectedTime c) ++ ")"

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
isUser c = clientType c /= 1

showClients :: [Client] -> String
showClients = intercalate "\n" . map showClient . filter isUser

showChannel :: Channel -> String
showChannel c = channelName c ++ ":\n" ++ showClients (clients c)

showChannels :: [Channel] -> String
showChannels cs = if null out then "0 clients are online." else out
  where
    out = intercalate "\n" . map showChannel . filter (any isUser . clients) $ cs

get :: String -> IO L.ByteString
get = simpleHttp

getChannellist :: String -> IO L.ByteString
getChannellist url = get (url ++ "channellist")

catchCallback :: HttpException -> IO ()
catchCallback _ = putStrLn "Internal Error or Server Offline."

main :: IO ()
main = catch
    (do
      str <- getChannellist "https://fkarchery.de/ts3chatter/"
      putStrLn . maybe "internal error" showChannels $ decodeChannel str)
    catchCallback
