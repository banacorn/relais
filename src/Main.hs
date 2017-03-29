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
    }

type M = ReaderT State IO

genDefaultState :: IO State
genDefaultState = do
    s <- newEmptyMVar
    c <- newEmptyMVar
    return $ State s c

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

type Session = ReaderT Socket M

main :: IO ()
main = do
    defaultState <- genDefaultState
    runReaderT repl defaultState

data Action = Noop | Start | Stop | Kill | Info deriving (Show)

parseAction :: String -> Maybe Action
parseAction "" = Just Noop
parseAction "s" = Just Start
parseAction "q" = Just Stop
parseAction "k" = Just Kill
parseAction "i" = Just Info
parseAction _ = Nothing

printNewline :: M ()
printNewline = liftIO $ putStrLn ""

printPrompt :: M ()
printPrompt = do
    bound <- isBound
    connected <- isConnected
    -- display the prompt
    liftIO $ if bound
        then putStr "@"
        else putStr " "
    liftIO $ if connected
        then putStr "*"
        else putStr " "
    liftIO $ putStr " > "

printInfo :: M ()
printInfo = do
    bound <- isBound
    connected <- isConnected
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
    where
        star :: Bool -> String
        star True = "*"
        star False = " "

repl :: M ()
repl = do
    printPrompt
    -- read
    str <- liftIO getLine
    case parseAction str of
        Just Noop -> liftIO $ putStr ""
        Just Start -> void $ do
            bind
            forkM accept
        Just Stop -> do
            disconnect
            unbind
        Just Kill -> do
            disconnect
        Just Info -> printInfo
        Nothing -> liftIO $ putStrLn $ "cannot read: " <> str
    repl

accept :: M ()
accept = do
    sock <- get socketVar
    (conn, _) <- liftIO $ Net.accept sock
    set connectionVar conn
    printNewline
    printPrompt
    loop conn
    accept

loop :: Socket -> M ()
loop conn = do
    msg <- liftIO $ recv conn 1024
    let disconnected = S.null msg
    killed <- fmap not isConnected
    if disconnected || killed
        then do
            disconnect
            printNewline
            printPrompt
            return ()
        else do
            -- liftIO $ sendAll conn msg
            liftIO $ C.putStr $ "[" <> C.pack (show $ S.length msg) <> "] " <> msg
            printPrompt
            loop conn
