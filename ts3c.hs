module Main(main) where

import Prelude
import Network.URI
import Network.HTTP
import System.IO
import JSON

showClient :: Client -> String
showClient c = client_nickname c

isUser :: Client -> Bool
isUser c = client_type c /= 1

isNull :: [Client] -> Bool
isNull [] = True
isNull (c:cs) = not (isUser c) && isNull cs

showClients :: [Client] -> String
showClients []     = ""
showClients [c]    | isUser c  = showClient c
                   | otherwise = ""
showClients (c:cs) | isUser c  = showClient c ++ ", " ++ showClients cs
                   | otherwise = showClients cs

showChannel :: Channel -> String
showChannel c = channel_name c ++ ": " ++ showClients (clients c)

showChannels :: [Channel] -> String
showChannels xs = showChannels' zs
  where
    createChannels :: [Channel] -> [Channel]
    createChannels [] = []
    createChannels (x:xs) = Channel {
                            channel_name = channel_name x,
                            clients      = (takeEvery isUser (clients x)) 
                            } : createChannels xs
    ys :: [Channel]
    ys = createChannels xs
    zs :: [Channel]
    zs = [ z | z <- xs, not (isNull (clients z)) ]
    showChannels' []     = ""
    showChannels' [c]    = showChannel c
    showChannels' (c:cs) = showChannel c ++ "\n" ++ showChannels' cs

maybeListToList :: Maybe [a] -> [a]
maybeListToList (Just xs) = xs
maybeListToList (Nothing) = []

takeEvery :: (a -> Bool) -> [a] -> [a]
takeEvery f xs = [ y | y <- xs, f y ]

url :: String
url = "http://fkarchery.de/ts3chatter/"

get :: String -> IO String
get url = simpleHTTP (getRequest url) >>= getResponseBody

getClientlist :: IO String
getClientlist = get (url ++ "clientlist")

getChannellist :: IO String
getChannellist = get (url ++ "channellist")

main = do
    str <- getChannellist
    let channel = maybeListToList $ decodeChannel str
    let output = showChannels channel
    if length output > 0
        then putStrLn output
        else putStrLn "0 Clients are online."
