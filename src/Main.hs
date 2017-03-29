{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Network.Socket as Net
import Network.Socket (Socket)
import Network.Socket.ByteString
import Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C

import Data.Monoid

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.Chan
import Control.Monad.Reader

forkM :: M a -> M ThreadId
forkM f = do
    state <- ask
    liftIO $ forkIO $ do
        void $ runReaderT f state


data State = State
    {   socketVar :: MVar Socket
    ,   connectionVar :: MVar Socket
    ,   messageChan :: Chan ByteString
    }

type M = ReaderT State IO

genDefaultState :: IO State
genDefaultState = do
    s <- newEmptyMVar
    c <- newEmptyMVar
    m <- newChan
    return $ State s c m

get :: (State -> MVar Socket) -> M Socket
get selector = asks selector >>= liftIO . readMVar

set :: (State -> MVar Socket) -> Socket -> M Socket
set selector sock = do
    var <- asks selector
    liftIO $ putMVar var sock
    return sock

takeAway :: (State -> MVar Socket) -> M Socket
takeAway selector = asks selector >>= liftIO . takeMVar

-- if the server has bind the socket
isBound :: M Bool
isBound = asks socketVar >>= liftIO . fmap not . isEmptyMVar

bind :: M Socket
bind = do
    bound <- isBound
    if bound
        then get socketVar
        else setup >>= set socketVar
    where
        setup :: M Socket
        setup = liftIO $ do
            sock <- Net.socket Net.AF_INET Net.Stream 0    -- create socket
            Net.setSocketOption sock Net.ReuseAddr 1   -- make socket immediately reusable - eases debugging.
            Net.bind sock (Net.SockAddrInet 8192 Net.iNADDR_ANY)
            Net.listen sock 2                      -- set a max of 2 queued connections
            return sock

unbind :: M ()
unbind = do
    bound <- isBound
    if bound
        then takeAway socketVar >>= liftIO . Net.close
        else return ()

disconnect :: M ()
disconnect = do
    connected <- isConnected
    if connected
        then takeAway connectionVar >>= liftIO . Net.close
        else return ()

isConnected :: M Bool
isConnected = asks connectionVar >>= liftIO . fmap not . isEmptyMVar

getMessage :: M ByteString
getMessage = asks messageChan >>= liftIO . readChan

type Session = ReaderT Socket M

main :: IO ()
main = do
    defaultState <- genDefaultState
    runReaderT prog defaultState

prog :: M ()
prog = do
    repl

data Action = Noop | Start | Stop | Kill | Info deriving (Show)

parseAction :: String -> Maybe Action
parseAction "" = Just Noop
parseAction "start" = Just Start
parseAction "stop" = Just Stop
parseAction "kill" = Just Kill
parseAction "info" = Just Info
parseAction _ = Nothing


repl :: M ()
repl = do
    bound <- isBound
    connected <- isConnected
    -- display the prompt
    liftIO $ if bound
        then putStr "*"
        else putStr " "
    liftIO $ if connected
        then putStr "-*"
        else putStr "  "
    liftIO $ putStr " > "

    -- read
    str <- liftIO getLine
    case parseAction str of
        Just Noop -> liftIO $ putStr ""
        Just Start -> void $ do
            bind
            forkM $ do
                sock <- get socketVar
                (conn, _) <- liftIO $ Net.accept sock
                set connectionVar conn
                loop conn
                return ()
        Just Stop -> do
            disconnect
            unbind
            return ()
        Just Kill -> liftIO $ putStr "kill"
        Just Info -> do
            liftIO $ putStrLn "          connected   bound   listening   readable   writable"
            liftIO $ putStr " server   "
            if bound
                then do
                    sock <- get socketVar
                    liftIO $ do
                        Net.isConnected sock >>= putStr . star >> putStr "           "
                        Net.isBound sock >>= putStr . star >> putStr "       "
                        Net.isListening sock >>= putStr . star >> putStr "           "
                        Net.isReadable sock >>= putStr . star >> putStr "          "
                        Net.isWritable sock >>= putStr . star
                else return ()
            liftIO $ putStr "\n"

            liftIO $ putStr " client   "
            if connected
                then do
                    conn <- get connectionVar
                    liftIO $ do
                        Net.isConnected conn >>= putStr . star >> putStr "           "
                        Net.isBound conn >>= putStr . star >> putStr "       "
                        Net.isListening conn >>= putStr . star >> putStr "           "
                        Net.isReadable conn >>= putStr . star >> putStr "          "
                        Net.isWritable conn >>= putStr . star
                else return ()
            liftIO $ putStr "\n"
            --         putStrLn
            return ()
        Nothing -> liftIO $ putStrLn $ "cannot read: " <> str
    repl

    where
        star :: Bool -> String
        star True = "*"
        star False = " "

loop :: Socket -> M ()
loop conn = do
    msg <- liftIO $ recv conn 1024
    let disconnected = S.null msg
    if disconnected
        then do
            liftIO $ putStrLn "[0]"
            disconnect
            return ()
        else do
            -- liftIO $ sendAll conn msg
            liftIO $ C.putStr $ "[" <> C.pack (show $ S.length msg) <> "] " <> msg
            loop conn

-- loop :: Socket -> MVar Status -> IO ()
-- loop conn var = do
--     status <- readMVar var
--     msg <- recv conn 1024
--     sendAll conn msg
--     C.putStrLn $ C.pack (show status) <> " > " <> msg
--     if msg == "bye\r\n" || msg == "" || status == False
--         then close conn
--         else loop conn var
--
