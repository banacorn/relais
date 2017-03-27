{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Socket hiding (recv, sendAll)
import Network.Socket.ByteString
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C

import Data.Monoid

import Control.Concurrent
import Control.Concurrent.MVar

type Status = Bool

main :: IO ()
main = do
    sock <- setup
    var <- newMVar True
    forkIO $ wait sock var
    repl sock var
    return ()

wait :: Socket -> MVar Status -> IO ()
wait sock var = do
    (conn, _) <- accept sock
    loop conn var
    wait sock var

loop :: Socket -> MVar Status -> IO ()
loop conn var = do
    status <- readMVar var
    msg <- recv conn 1024
    sendAll conn msg
    C.putStrLn $ C.pack (show status) <> " > " <> msg
    if msg == "bye\r\n" || msg == "" || status == False
        then close conn
        else loop conn var

setup :: IO Socket
setup = do
    sock <- socket AF_INET Stream 0    -- create socket
    setSocketOption sock ReuseAddr 1   -- make socket immediately reusable - eases debugging.
    bind sock (SockAddrInet 8192 iNADDR_ANY)
    listen sock 2                      -- set a max of 2 queued connections
    return sock

repl :: Socket -> MVar Status -> IO ()
repl sock var = do
    s <- getLine
    status <- readMVar var
    putStrLn $ show status <> " : " <> s
    if s == "close"
        then do
            tryPutMVar var False
            close sock
        else repl sock var
