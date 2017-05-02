{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import Control.Concurrent ( threadDelay )
import Data.Binary
import Data.Typeable

import Control.Distributed.Process
import Control.Distributed.Process.Node
import Network.Transport.TCP

-- Serializable (= Binary + Typeable)
data Ping = Ping (SendPort Pong) deriving (Typeable)
instance Binary Ping where
    put (Ping sendPort) = putWord8 sendPort
    get      = do { sendPort <- getWord8; return Ping sendPort }

data Pong = Pong deriving (Typeable)
instance Binary Pong where
    put (Pong) = putWord8 0
    get = do { getWord8; return Pong }
    
server :: ReceivePort Ping -> Process ()
server rPing = do
    (Ping clientPort) <- receiveChan rPing
    liftIO $ putStrLn "Got a ping!"
    sendChan clientPort Pong
    
client :: SendPort Ping -> Process ()
client serverPort =
    sendChan serverPort Ping
    
{-
ignition :: Process ()
ignition = do
    -- start the server
    serverPort <- spawnChannelLocal server
    -- serverPort :: SendPort Ping
    -- start the client
    spawnLocal $ client serverPort
    liftIO $ threadDelay 100000 -- wait a while
    -}
    
ignition :: Process ()
ignition = 
    -- start the server
    spawnChannelLocal server >>= (\serverPort ->
        -- (serverPort :: SendPort Ping)
        -- start the client
        (spawnLocal $ client serverPort) >> 
        (liftIO $ threadDelay 100000) -- wait a while
        )
    
main :: IO ()
main = do
    Right transport <- createTransport "127.0.0.1" "8080"
                            defaultTCPParameters
    node <- newLocalNode transport initRemoteTable
    runProcess node ignition


{-
data SendPort a     -- Serializable
data ReceivePort a  -- NOT Serializable
newChan     :: Serializable a => Process (SendPort a, ReceivePort a)
sendChan    :: Serializable a => SendPort a -> a -> Process ()
receiveChan :: Serializable a => ReceivePort a -> Process a 
-}

{-
Finally, it's worth noticing that SendPort's are themselves Serializable, meaning that they can be copied and shipped around to other processes possibly on other computers. This allows a channel to accept data from more than one place, and also makes for idioms like including a reply-to SendPort in your messages. ReceivePort's on the other hand are (deliberately) left unserializable which leaves them tied to single computer.
-}

{-
Our little example was more “hello wo” than “hello world”; we'd only managed to send a Ping without even thinking about sending Pong's back. Want to try your hand at Cloud Haskell? Here's a great opportunity!

1. [Easy] Start with a cabal install distributed-process and make sure you can run this example. Note that you'll need GHC 7.4.1 and up for this

2. [Less easy] Next, add a new Pong message (as a separate data type), extending the server to send this message back, and the client to receive that reply. There are some puzzle pieces to work through here. How does the server know where to send its replies? Moreover, how do we keep the server nice and decoupled from the client? We want it to receive pings from any client, and send a reply back to the ping'er (and not just some hard-coded client). Hint: you can solve this without touching ignition or main. Remember that SendPort is Serializable!

3. [Easy] You now have a single ping/pong interaction. Can you make the game go back and forth indefinitely (or until the threadDelay ends)? Hint: have a look at Control.Monad; it's not essential, but it's a bit nicer.
-}
