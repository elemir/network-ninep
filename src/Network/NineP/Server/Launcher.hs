{-# LANGUAGE NoImplicitPrelude, TypeFamilies, FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Network.NineP.Server.Launcher
    ( Stat(..)
    , Connection(..)
    , Server(..), launchServer, launchServer'
    ) where
import Prelude hiding (read, lookup)

import Control.Concurrent (killThread)
import Control.Exception (bracketOnError)
import Control.Monad (forever)
import Control.Monad.Error (catchError, throwError, runErrorT)
import Control.Monad.State (runStateT)
import Control.Monad.Trans (lift, liftIO)
import Data.Maybe (listToMaybe, fromMaybe)
import Data.Word (Word16)
import System.IO (Handle, hClose, hIsOpen)

import qualified Data.Map as M

import Control.Concurrent.MState (evalMState, forkM, forkM_, waitM)
import Data.Binary.Get (runGetState)
import Network
  ( Socket, PortID(..), HostName
  , accept, withSocketsDo, sClose, listenOn
  )
import Network.BSD (getHostByName, getProtocolNumber, hostAddress)
import Network.Socket
  ( SocketOption(..), SockAddr(..), PortNumber(..), SocketType(..), Family(..)
  , socket, listen, bindSocket
  , maxListenQueue, setSocketOption
  )
import Text.Regex.Posix ((=~))

import qualified Data.ByteString.Lazy as L

import Control.Monad.Launcher
import Network.NineP.Binary hiding (Stat(..), Bin(..))
import Network.NineP.Server.File.Internal
import Network.NineP.Server.Utils
import Network.NineP.Server.Error

import qualified Network.NineP.Binary as B

class Connection a where
    cAccept :: a -> IO Handle
    cClose :: a -> IO ()

class Server s where
    attach :: String -> String -> NineP s (File' s)

instance Connection Socket where
    cAccept sock = do
      (h, _, _) <- accept sock
      return h
    cClose  = sClose

launchServer :: Server s => s -> String -> IO ()
launchServer serv str =
    withSocketsDo $ connection str >>= launchServer' serv

listen' :: HostName -> PortNumber -> IO Socket
listen' hostname port = do
    proto <- getProtocolNumber "tcp"
    bracketOnError
      (socket AF_INET Stream proto)
      sClose
      (\sock -> do
	setSocketOption sock ReuseAddr 1
        he <- getHostByName hostname
      	bindSocket sock (SockAddrInet port (hostAddress he))
	listen sock maxListenQueue
	return sock
      )

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

connection :: String -> IO Socket
connection s =
    let pat = "tcp!(.*)!([0-9]*)|unix!(.*)"
        wrongAddr = ioError $ userError $ "wrong 9p connection address: " ++ s
        (bef, _, aft, grps) = s =~ pat :: (String,String,String,[String])
     in if (bef /= "" || aft /= "" || grps == [])
        then wrongAddr
        else case grps of
            [addr, port, ""] ->
              listen' addr $ PortNum $ fromMaybe 2358 $ maybeRead port
            ["", "", addr]   -> listenOn $ UnixSocket addr
            _                -> wrongAddr

launchServer' :: (Server s, Connection c) => s -> c -> IO ()
launchServer' serv conn =
    let catch' :: Server s => Handle -> Word16 -> NinePError -> Launcher s ()
        catch' h tag err = do
          isOpen <- liftIO $ hIsOpen h
          if (isOpen)
            then sendRMsg h tag (Rerror (toString err))
            else throwError err

        msgLoop :: L.ByteString -> [Msg TMsgBody]
        msgLoop buf =
          let (x, buf', _) = runGetState B.get buf 0
           in x : msgLoop buf'

        parseLoop :: Server s => Handle -> [Msg TMsgBody] -> Launcher s ()
        parseLoop h (Msg tag body:xs) = do
          let parse = case body of 
                        Tversion sz ver' ->
                          setVersion h tag sz $
                            if ver' /= "9P2000"
                              then return Nothing
                              else return $ Just "9P2000"
                        Tauth _ _ _ ->
                          throwError NoAuthRequired 
                        Tflush oldtag ->
                          checkVersion $ do
                            withThreadIdDo oldtag $
                              \threadId -> do
                                liftIO $ killThread threadId
                                clunkTag oldtag
                                sendRMsg h tag Rclunk
                        Tattach fid _ uname aname ->
                          checkVersion $ 
                            withoutFileDo fid $ do
                              root' <- lift $ attach uname aname
                              case root' of
                                File' file _ -> do
                                  qid' <- lift $ getQid file
                                  sendRMsg h tag $ Rattach qid'
                                  putFile fid root'
                        Twalk fid newfid wnames ->
                          checkVersion $
                            withFileDo fid $
                              \(File' file _) -> do
                                (newFile, qids) <- lift $ walk file wnames
                                putFile newfid newFile
                                sendRMsg h tag $ Rwalk qids
                        Topen fid mode ->
                          checkVersion $
                            withFileDo fid $
                              \(File' file _) -> do
                                d <- lift $ open file $ fromBinOMode mode
                                qid <- lift $ getQid file
                                sendRMsg h tag $ Ropen qid 0
                                putFile fid $ File' file (Just d)
                        Tcreate fid name perm mode ->
                          checkVersion $
                            withFileDo fid $
                              \(File' dir _) -> do
                                File' file _ <- lift $ create dir name 
                                                        (fromIntegral perm)
                                                        (fromBinOMode mode)
                                qid <- lift $ getQid file
                                sendRMsg h tag $ Rcreate qid 0
                        Tread fid offset count ->
                          checkVersion $
                            withFileDo fid $
                              \(File' file md) ->
                                case md of
                                  Just d -> do
                                    (buf, d') <-
                                      lift $ runStateT (read 
                                                file offset count) d
                                    sendRMsg h tag $ Rread buf
                                    putFile fid $ File' file (Just d')
                                  Nothing -> throwError ErrInvalArg
                        Twrite fid offset dat ->
                          checkVersion $
                            withFileDo fid $
                              \(File' file md) ->
                                case md of
                                  Just d -> do
                                    (count, d') <- lift $ runStateT (write
                                                            file offset dat) d
                                    sendRMsg h tag $ Rwrite count
                                    putFile fid $ File' file (Just d')
                                  Nothing -> throwError ErrInvalArg
                        Tclunk fid -> 
                          checkVersion $
                            withFileDo fid $
                              \(File' file d) -> do
                                lift $ clunk file d
                                sendRMsg h tag Rclunk
                                clunkFile fid
                        Tremove fid ->
                          checkVersion $
                            withFileDo fid $
                              \(File' file _) -> do
                                lift $ remove file
                                sendRMsg h tag Rremove
                        Tstat fid ->
                          checkVersion $ 
                            withFileDo fid $
                              \f@(File' file _) -> do
                                mp <- lift $ lookup file
                                stat' <- mapM (lift . binStat) (f : M.elems mp)
                                sendRMsg h tag $ Rstat stat'
                        Twstat fid stats ->
                          checkVersion $ 
                            withFileDo fid $
                              \(File' file _) -> do
                                mapM_ (\st -> lift . wstat file 
                                                  . fromBinStat $ st) stats
                                sendRMsg h tag Rwstat
          forkM (catchError parse $ catch' h tag) >>= 
            (\threadId -> putThreadId tag threadId 
                           >> waitM threadId >> clunkTag tag)
          parseLoop h xs
        parseLoop h [] = liftIO $ hClose h

        thread :: Server s => Handle -> Launcher s ()
        thread h = do
          buf <- liftIO $ L.hGetContents h
          parseLoop h $ msgLoop buf

        loop :: Server s => NineP s ()
        loop = do
          h <- liftIO $ cAccept conn
          forkM_ $ evalMState True (thread h) (Nothing, M.empty, M.empty)

     in do
         result <- runErrorT $ evalMState True (forever loop) serv
         case result of
            Left err -> ioError $ userError (toString err)
            Right _ -> return ()

